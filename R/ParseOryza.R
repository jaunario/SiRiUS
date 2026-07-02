#' Convert a vector of strings to a table
#'
#' This function takes a vector of strings, where each string represents a row of a table,
#' and converts it into a data frame or a matrix.
#'
#' @param x A character vector where each element is a row of the table.
#' @param delim The delimiter separating the columns in the strings. Default is tab (`\t`).
#' @param header A boolean indicating whether the first string in `x` is the header. Default is `TRUE`.
#' @param out.df A boolean indicating whether to return a data frame. If `FALSE`, a matrix is returned. Default is `TRUE`.
#' @param fields An optional character vector of column names to select from the table.
#' @return A data frame or matrix.
#' @export
stringVectorToTable <- function(x, delim = "\t", header = TRUE, out.df = TRUE, fields = NULL) {
  if (header == TRUE) {
    hdr <- x[1]
    hdr <- unlist(strsplit(hdr, delim))
    x <- x[-1]
  }
  x <- trimws(x)
  x <- x[x != ""]
  x <- sapply(x, strsplit, split = delim, simplify = FALSE)
  x <- suppressWarnings(lapply(unlist(x, recursive = FALSE), as.numeric))
  dat.table <- do.call(rbind, x)

  colnames(dat.table) <- hdr
  if (out.df == TRUE) {
    dat.table <- as.data.frame(dat.table)
  }

  if (!is.null(fields)) {
    dat.table <- dat.table[, fields]
  }

  return(dat.table)
}

#' Read an ORYZA res.dat file
#'
#' This function reads and parses an ORYZA `res.dat` output file.
#'
#' @param resdat.file The path to the `res.dat` file.
#' @param cell The cell number associated with the simulation.
#' @param selected.fields An optional character vector of fields to select from the output.
#' @param verbose A boolean indicating whether to print progress messages. Default is FALSE.
#' @return A data frame containing the parsed data from the `res.dat` file.
#' @export
readRESDAT <- function(resdat.file, cell, selected.fields = NULL, verbose = FALSE) {
  #cell <- as.numeric(sub("cell", "", sub("_res.dat", "", basename(resdat.file))))

  if (verbose) message(cell, ": Parsing data.")
  dat.thiscell <- readLines(resdat.file)
  dat.thiscell <- trimws(dat.thiscell)

  date.start <- grep("System start", dat.thiscell, value = TRUE)
  date.start <- as.Date(date.start, "*    Year: %Y, day: %j, System start")
  date.end <- grep("System end", dat.thiscell, value = TRUE)
  date.end <- as.Date(date.end, "*    Year: %Y, day: %j, System end")
  data.dates <- mapply("seq", as.list(date.start), as.list(date.end), by = list("day"), SIMPLIFY = FALSE)
  data.dates <- do.call(c, data.dates)
  if (verbose) message(cell, ": Expecting ", length(data.dates), " records.")
  #dimDate <- ncdim_def(name = "time", units = "date", vals = as.integer(data.dates))

  dat.thiscell <- dat.thiscell[!grepl("\\*", dat.thiscell)]
  dat.thiscell <- dat.thiscell[dat.thiscell != ""]

  idx.hdrs <- grep("TIME\t", dat.thiscell)
  idx.endtable <- c(idx.hdrs[-1] - 1, length(dat.thiscell))
  idx.data <- mapply(":", as.list(idx.hdrs), as.list(idx.endtable), SIMPLIFY = FALSE)

  dat.thiscell <- mapply("[", list(x = dat.thiscell), idx.data)
  dat.thiscell <- lapply(dat.thiscell, stringVectorToTable, fields = selected.fields)
  dat.thiscell <- do.call(rbind, dat.thiscell)
  dat.thiscell <- as.data.frame(dat.thiscell)
  dat.thiscell <- cbind(cell, date = data.dates, dat.thiscell)
  rownames(dat.thiscell) <- NULL
  return(dat.thiscell)
}

# Read an (only 1) op.dat oryza output file. Do sapply on a processing script to make this function more flexible
#' Read an ORYZA op.dat file
#'
#' This function reads an ORYZA `op.dat` output file.
#'
#' @param opdat.file The path to the `op.dat` file. Default is "op.dat".
#' @param selected.fields An optional character vector of fields to select from the output.
#' @param cell An optional identifier for the cell.
#' @return A data frame containing the data from the `op.dat` file.
#' @export
readOPDAT <- function(opdat.file = "op.dat", selected.fields = NULL, cell = NULL) {
  dat.opdat <- read.table(opdat.file, header = TRUE)
  if (!is.null(selected.fields)) dat.opdat <- dat.opdat[, c("RUNNUM", selected.fields)]
  if (!is.null(cell)) dat.opdat$cell <- cell
  return(dat.opdat)
}

#' Read an ORYZA experiment file
#'
#' This function reads and parses an ORYZA experiment file (.exp), extracting parameters and their values.
#'
#' @param experiment.file The path to the experiment file.
#' @param reftable A boolean indicating whether to attach a reference table of all parameters as an attribute to the output. Default is FALSE.
#' @param include.options A boolean indicating whether to include all available parameter options as an attribute to the output. Default is FALSE.
#' @param skip.lines The number of header lines to skip at the beginning of the file. Default is 12.
#' @return A data frame containing the active parameters and their values found in the experiment file.
#' @export
readEXPFILE <- function(experiment.file, reftable = FALSE, include.options = FALSE, skip.lines = 12) {
  txt.file <- readLines(experiment.file)
  txt.file <- txt.file[-(1:skip.lines)]

  # Identify all parameter markers (active and inactive)
  # Parameters start at the beginning of the line, possibly with a * prefix
  idx.allparams <- grep("^[\\*]*[A-Z0-9_]+[[:space:]]*=", txt.file)

  # Extract parameter names
  param.lines <- txt.file[idx.allparams]
  param.names <- sub("^[\\*]*([A-Z0-9_]+)[[:space:]]*=.*", "\\1", param.lines)
  is.active <- !grepl("^\\*", param.lines)

  # Group indices by parameter name to handle options/multiline
  param.groups <- split(idx.allparams, param.names)
  # Maintain original order of first appearance
  unique.names <- param.names[!duplicated(param.names)]

  dat.paraminfo <- list()
  dat.options <- list()
  lst.active <- list()

  for (i in seq_along(unique.names)) {
    pname <- unique.names[i]
    pindices <- param.groups[[pname]]

    # Determine full text for this parameter (including comments and multiline values)
    # A parameter's block ends before the next parameter starts
    next.param.idx <- idx.allparams[which(idx.allparams == max(pindices)) + 1]
    if (is.na(next.param.idx)) {
      end.idx <- length(txt.file)
    } else {
      end.idx <- next.param.idx - 1
    }
    # Actually, ORYZA multiline values are often followed by other parameters or blank lines.
    # The original code used blank lines as boundaries for multiline-parameters.

    pfulltxt <- txt.file[min(pindices):end.idx]

    # Vartype detection
    vartype <- "required"
    if (any(grepl("!@", pfulltxt))) vartype <- "switch"
    else if (any(grepl("!\\*", pfulltxt))) vartype <- "optional"
    vartype <- factor(vartype, levels = c("required", "switch", "optional"))

    # Extract options
    poptions.list <- list()
    for (pidx in pindices) {
      # Find end of this specific option (until blank line or next param marker in pfulltxt)
      rel.idx <- pidx - min(pindices) + 1
      # Look ahead for blank line in pfulltxt
      remaining <- pfulltxt[rel.idx:length(pfulltxt)]
      blank <- which(remaining == "")
      if (length(blank) > 0) opt.end <- rel.idx + blank[1] - 2
      else opt.end <- length(pfulltxt)

      # Check if another parameter marker starts before opt.end
      next.m <- grep("^[\\*]*[A-Z0-9_]+[[:space:]]*=", remaining[-1])
      if (length(next.m) > 0) {
        opt.end <- min(opt.end, rel.idx + next.m[1] - 1)
      }

      opt.txt <- pfulltxt[rel.idx:(opt.end)]

      # Parse value and description
      desc <- NA
      val.txt <- opt.txt
      desc.m <- grep("!", val.txt)
      if (length(desc.m) > 0) {
        # Only take from the first ! found in the option lines
        parts <- strsplit(val.txt[desc.m[1]], "!")[[1]]
        val.txt[desc.m[1]] <- parts[1]
        desc <- trimws(paste(c(parts[-1], val.txt[(desc.m[1]+1):length(val.txt)]), collapse = "; "))
        val.txt <- val.txt[1:desc.m[1]]
      }

      val <- trimws(sub(paste0("^[\\*]*", pname, "[[:space:]]*=[[:space:]]*"), "", paste(val.txt, collapse = " ")))
      val <- sub("[[:space:]]*;[[:space:]]*$", "", val) # Clean trailing semicolons if any

      poptions.list[[length(poptions.list) + 1]] <- data.frame(parameter = pname, value = val, description = desc, active = !grepl("^\\*", txt.file[pidx]))
    }

    poptions.df <- do.call(rbind, poptions.list)
    dat.options[[pname]] <- poptions.df[, 1:3]

    # Active value
    active.opt <- poptions.df[poptions.df$active, ]
    if (nrow(active.opt) > 0) {
      lst.active[[pname]] <- active.opt[1, c("parameter", "value")]
    }

    # Datatype and Nval
    nval <- if (any(grepl(",", poptions.df$value))) "matrix" else "scalar"
    datatype <- "integer"
    if (any(grepl("'", poptions.df$value))) datatype <- "character"
    else if (any(grepl("[[:digit:]]+\\.", poptions.df$value))) datatype <- "float"
    datatype <- factor(datatype, levels = c("character", "integer", "float"))

    # Description from comments above
    idx.desc <- grep(paste0("^\\*--[[:space:]]+", pname), txt.file)
    txt.desc <- NA
    if (length(idx.desc) > 0) {
       # simplistic take on description
       txt.desc <- sub(paste0("^\\*--[[:space:]]+", pname, "[[:space:]]*[:]*[[:space:]]*"), "", txt.file[idx.desc[1]])
    }
    if (is.na(txt.desc) || txt.desc == "") {
      txt.desc <- paste(na.omit(poptions.df$description), collapse = "; ")
    }

    dat.paraminfo[[pname]] <- data.frame(
      name = pname,
      vartype = vartype,
      datatype = datatype,
      description = txt.desc,
      nval = nval,
      sequence = i,
      line_st = min(pindices),
      line_en = min(pindices) + end.idx - 1
    )
  }

  dat.paraminfo <- do.call(rbind, dat.paraminfo)
  dat.options <- do.call(rbind, dat.options)
  lst.active <- do.call(rbind, lst.active)

  if (include.options) attr(lst.active, "options") <- dat.options
  if (reftable) attr(lst.active, "reftable") <- dat.paraminfo

  return(lst.active)
}
