canRunMore <- function(totalram = NULL, limit.ram = 0.2, limit.cpu = 0.9) {

  if (Sys.info()["sysname"] == "Windows") {
    totalram <- ifelse(is.null(totalram) || is.na(totalram),
                       as.numeric(trimws(system2("wmic", args = "ComputerSystem get TotalPhysicalMemory", stdout = TRUE)[2])) / 1024^2,
                       totalram)
    sys.availram <- system2("wmic", args = "OS get FreePhysicalMemory", stdout = TRUE)[2]
    sys.cpu <- system2("wmic", args = "cpu get loadpercentage", stdout = TRUE, wait = TRUE)
  } else {
    #TODO: linux implementation
  }
  # Getting available RAM
  sys.availram <- as.numeric(trimws(sys.availram)) / 1024
  sys.availrampct <- round((sys.availram / totalram), 2)

  # Getting CPU usage
  sys.cpu <- as.numeric(trimws(sys.cpu[2])) / 100

  return((sys.availrampct > limit.ram) | (sys.cpu < limit.cpu))

}

# Retries reading RDS files when RDS file is used by another process
force.readrds <- function(filename, ...) {
  if (!file.exists(filename)) {
    result <- NULL
  } else {
    repeat {
      result <- try(readRDS(file = filename), ...)
      if (!inherits(result, "try-error")) break
    }
  }
  return(result)
}

# Formating numbers
numvectorToString <- function(num, sigdigits) {
  return(paste(sprintf(paste0("%0.0", sigdigits, "f"), round(num, sigdigits)), collapse = ","))
}


memberID <- function(grp, nums) {
  if (is.na(grp)) {
    ids <- 1:sum(is.na(nums))
  } else {
    ids <- 1:sum(nums == grp)
  }
  return(ids)
}
