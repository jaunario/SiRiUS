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

resdatToDF <- function(resdat.file, cell, selected.fields = NULL, verbose = FALSE) {
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

  return(dat.thiscell)
}
