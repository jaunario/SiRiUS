#' Convert a vector of strings to a table
#'
#' This function takes a vector of strings, where each string represents a row of a table,
#' and converts it into a data frame or a matrix.
#'
#' @param x A character vector where each element is a row of the table.
#' @param delim The delimiter separating the columns in the strings. Default is tab (`\t`).
#' @param header A boolean indicating whether the first string in `x` is the header. Default is TRUE.
#' @param out.df A boolean indicating whether to return a data frame. If FALSE, a matrix is returned. Default is TRUE.
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
  #txt.expheader <- txt.file[1:12]
  txt.file <- txt.file[-(1:skip.lines)]

  #idx.sections <- grep("^\\* [0-9]{1}\\.[[:space:]]+[[:alpha:]]+[[:space:]]*$", txt.file)
  idx.activeparams <- unlist(sapply(c("^[A-Z0-9]+[[:space:]]*\\=", "^[A-Z0-9]+_[A-Z0-9]+[[:space:]]*\\="), grep, txt.file, USE.NAMES = FALSE))
  idx.inactiveparams <- unlist(sapply(c("^\\*[A-Z0-9]+[[:space:]]*\\=", "^\\*[A-Z0-9]+_[A-Z0-9]+[[:space:]]*\\="), grep, txt.file, USE.NAMES = FALSE))

  dat.idx <- data.frame(idx = c(idx.activeparams, idx.inactiveparams),
                        active = c(rep(TRUE, length(idx.activeparams)),
                                   rep(FALSE, length(idx.inactiveparams))))

  dat.idx <- dat.idx[order(dat.idx$idx), ]
  dat.idx$txt <- txt.file[dat.idx$idx]

  # if (!exists("dat.idxbk")) dat.idxbk <- dat.idx
  # dat.idxbk$done <-  FALSE
  # dat.idxbk -> dat.idx

  seq.tracker <- 0
  # if (exists("dat.options")) rm(dat.options)
  # if (exists("dat.paraminfo")) rm(dat.paraminfo)
  # if (exists("lst.active")) rm(lst.active)

  # TODO: Simplify/Remove this while-loop
  while (nrow(dat.idx) > 0) {

    this.param <- trimws(unlist(strsplit(txt.file[dat.idx$idx[1]], split = "=", fixed = TRUE)))
    thisparam.name <- this.param[1]
    thisparam.name <- sub("\\*", "", thisparam.name)

    if (exists("dat.paraminfo") && thisparam.name %in% dat.paraminfo$name) next
    seq.tracker <- seq.tracker + 1

    # Find if there are other lines mentioning the parameter
    idx.parammarkers <- c(grep(paste0("^", thisparam.name, "[[:space:]]*\\="), txt.file),
                          grep(paste0("^\\*", thisparam.name, "[[:space:]]*\\="), txt.file))

    if (nrow(dat.idx) > 1) {
      dat.tmp <- dat.idx[-which(dat.idx$idx %in% idx.parammarkers), ]
      # Check if more lines in between the next param
      if (nrow(dat.tmp) > 0) {
        if (max(idx.parammarkers) > (dat.tmp$idx[1])) {
          idx.parammarkers <- idx.parammarkers[idx.parammarkers < dat.tmp$idx[1]]
        }
        idx.fullparamtxt <- min(idx.parammarkers):(dat.tmp$idx[1] - 1)
      } else {
        idx.fullparamtxt <- min(idx.parammarkers):max(idx.parammarkers)
      }
      rm(dat.tmp)
    } else {
      idx.fullparamtxt <- min(idx.parammarkers):max(idx.parammarkers)
    }

    thisparam.txt <- txt.file[idx.fullparamtxt]

    # Identify vartype based on flags
    if (sum(grepl("!@", thisparam.txt)) > 0) {
      vartype <- factor("switch", levels = c("required", "switch", "optional"))
    } else if (sum(grepl("!\\*", thisparam.txt)) > 0) {
      vartype <- factor("optional", levels = c("required", "switch", "optional"))
    } else {
      vartype <- factor("required", levels = c("required", "switch", "optional"))
    }

    # Multiline-parameters are bound by blank lines
    param.group <- which(thisparam.txt != "")
    txt.groups <- mapply("[", list(thisparam.txt), consecutive.groups(param.group), SIMPLIFY = FALSE)
    if (length(txt.groups) > 1) {
      groupmarked <- which(sapply(mapply(grep, list(paste0("^", thisparam.name, "[[:space:]]*\\=")), txt.groups), length) > 0)
      if (length(groupmarked) == 0) {
        groupmarked <- which(sapply(mapply(grep, list(paste0("^\\*", thisparam.name, "[[:space:]]*\\=")), txt.groups), length) > 0)
      }
      txt.groups <- txt.groups[[groupmarked]]
    } else {
      txt.groups <- txt.groups[[1]]
    }
    thisparam.txt <- txt.groups

    file.loc <- match(thisparam.txt, txt.file) # Determine location to find where to replace

    # Local only to param txt
    idx.paramoptions <- grep(paste0(thisparam.name, "[[:space:]]*\\="), thisparam.txt)
    if (length(idx.paramoptions) > 1) {
      idx.paramend <- c(idx.paramoptions[2:length(idx.paramoptions)] - 1, length(thisparam.txt))
      idx.options <- mapply(":", as.list(idx.paramoptions), as.list(idx.paramend), SIMPLIFY = FALSE)
      param.options <- mapply("[", list(thisparam.txt), idx.options, SIMPLIFY = FALSE)
    } else {
      param.options <- list(thisparam.txt)
    }
    idx.selvalue <- unlist(which(sapply(sapply(param.options, grep, pattern = paste0("^", thisparam.name)), length) > 0))
    param.options.desc <- param.options.vals <- vector()
    for (i in seq_along(param.options)) {
      this.option <- param.options[[i]]

      # Separate the description
      desc.marker <- grep("!", this.option)
      if (length(desc.marker) > 0) {
        thisparamoption.desc <- strsplit(this.option[desc.marker], split = "!")
        thisparamoption.desc <- do.call(rbind, thisparamoption.desc)
        thisparamoption.desc <- sub("\\*", "", thisparamoption.desc)
        this.option[desc.marker] <- thisparamoption.desc[, 1]
        param.options.desc <- c(param.options.desc, trimws(paste(thisparamoption.desc[, 2], collapse = "; ")))
      } else {
        param.options.desc <- c(param.options.desc, NA)
      }

      # Parse parameter values
      thisparamoption.val <- sub(paste0(thisparam.name, "[[:space:]]*\\="), "", this.option)
      thisparamoption.val <- sub("\\*", "",  thisparamoption.val)
      thisparamoption.val <- thisparamoption.val[thisparamoption.val != ""]
      param.options.vals <- c(param.options.vals, trimws(paste(thisparamoption.val, collapse = "; ")))
    }

    param.options <- data.frame(parameter = thisparam.name, value = param.options.vals, description = param.options.desc)

    param.options$description <- sub("@", "", param.options$description)
    param.options$description <- sub("\\*", "", param.options$description)
    param.options$description <- trimws(param.options$description)

    if (ncol(param.options) == 3) {
      param.options <- data.frame(parameter = thisparam.name, value = param.options[, 2], description = param.options[, 3])
    } else {
      param.options <- data.frame(parameter = thisparam.name, value = param.options[, 2], description = NA)
    }

    #if (thisparam.name == "SWITIR") switir.marker <- param.options[idx.selvalue, ]

    if (length(idx.selvalue) > 0) {
      if (!exists("lst.active")) {
        lst.active <- list(param.options[idx.selvalue, c("parameter", "value")])
        names(lst.active) <- thisparam.name
      } else {
        old.names <- names(lst.active)
        lst.active <- c(lst.active, list(param.options[idx.selvalue, c("parameter", "value")]))
        names(lst.active) <- c(old.names, thisparam.name)
      }
    }

    idx.desc <- grep(paste0("^\\*--[[:space:]]+", thisparam.name), txt.file)
    if (length(idx.desc) > 0) {
      idx.desc <- idx.desc:(min(idx.parammarkers) - 1)
      txt.desc <- paste(txt.file[idx.desc], collapse = "; ")
      txt.desc <- gsub("\\*", "", txt.desc)
      txt.desctemp <- trimws(unlist(strsplit(txt.desc, ":", fixed = TRUE))[2])
      if (is.na(txt.desctemp)) txt.desc <- trimws(unlist(strsplit(txt.desc, "is"))[2]) else txt.desc <- txt.desctemp
    } else {
      txt.desc <- paste(param.options$description, collapse = "; ")
    }

    if (!exists("dat.options")) dat.options <- param.options else dat.options <- rbind(dat.options, param.options)

    if (sum(grepl("\\,", param.options$value)) > 0) {
      nval <- "matrix"
    } else {
      nval <- "scalar"
    }

    if (sum(grepl("'", param.options$value)) > 0) {
      datatype <- factor("character", levels = c("character", "integer", "float"))
    } else if (sum(grepl("[[:digit:]]+\\.", param.options$value)) > 0) {
      datatype <- factor("float", levels = c("character", "integer", "float"))
    } else {
      datatype <- factor("integer", levels = c("character", "integer", "float"))
    }

    dat.thisparam <- data.frame(name = thisparam.name,
                                vartype = vartype,
                                datatype = datatype,
                                description = txt.desc,
                                nval = nval,
                                sequence = seq.tracker,
                                line_st = min(idx.fullparamtxt),
                                line_en = max(file.loc))

    if (!exists("dat.paraminfo")) dat.paraminfo <- dat.thisparam else dat.paraminfo <- rbind(dat.paraminfo, dat.thisparam)

    dat.idx <- dat.idx[-which(dat.idx$idx %in% idx.fullparamtxt), ]
  }
  lst.active <- do.call(rbind, lst.active)
  if (include.options) attr(lst.active, "options") <- dat.options
  if (reftable) attr(lst.active, "reftable") <- dat.paraminfo

  return(lst.active)
}
