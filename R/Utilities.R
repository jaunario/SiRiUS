#' Check if more processes can be run
#'
#' This function checks the system's available RAM and CPU load to determine if more processes can be run.
#'
#' @param totalram The total physical memory of the system in GB. If NULL, it is determined automatically.
#' @param limit.ram The minimum fraction of available RAM required to run more processes. Default is 0.2.
#' @param limit.cpu The maximum CPU load allowed to run more processes. Default is 0.9.
#' @return A boolean indicating whether more processes can be run.
#' @export
canRunMore <- function(totalram = NULL, limit.ram = 0.2, limit.cpu = 0.9) {

  if (Sys.info()["sysname"] == "Windows") {
    totalram <- ifelse(is.null(totalram) || is.na(totalram),
                       as.numeric(trimws(system2("wmic", args = "ComputerSystem get TotalPhysicalMemory", stdout = TRUE)[2])) / 1024^3,
                       totalram)
    sys.availram <- system2("wmic", args = "OS get FreePhysicalMemory", stdout = TRUE)[2]
    sys.availram <- as.numeric(trimws(sys.availram)) / 1024^2
    sys.cpu <- system2("wmic", args = "cpu get loadpercentage", stdout = TRUE, wait = TRUE)
    sys.cpu <- as.numeric(trimws(sys.cpu[2])) / 100
  } else {
    meminfo <- readLines("/proc/meminfo")
    if (is.null(totalram) || is.na(totalram)) {
      totalram <- as.numeric(gsub("[^0-9]", "", grep("MemTotal", meminfo, value = TRUE))) / 1024^2
    }
    sys.availram <- grep("MemAvailable", meminfo, value = TRUE)
    if (length(sys.availram) == 0) sys.availram <- grep("MemFree", meminfo, value = TRUE)
    sys.availram <- as.numeric(gsub("[^0-9]", "", sys.availram)) / 1024^2
    loadavg <- readLines("/proc/loadavg")
    sys.cpu <- as.numeric(strsplit(loadavg, " ")[[1]][1])
    ncores <- parallel::detectCores()
    if (is.na(ncores)) ncores <- 1
    sys.cpu <- sys.cpu / ncores
  }
  # Getting available RAM
  sys.availrampct <- round((sys.availram / totalram), 2)

  return((sys.availrampct > limit.ram) | (sys.cpu < limit.cpu))

}

# Retries reading RDS files when RDS file is used by another process
#' Force reading of an RDS file
#'
#' This function repeatedly tries to read an RDS file until it succeeds. This is useful when the file might be temporarily locked by another process.
#'
#' @param filename The path to the RDS file.
#' @param ... Additional arguments to be passed to `readRDS`.
#' @return The object read from the RDS file, or NULL if the file does not exist.
#' @export
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
#' Convert a numeric vector to a string
#'
#' This function converts a numeric vector into a single comma-separated string, with a specified number of significant digits.
#'
#' @param num A numeric vector.
#' @param sigdigits The number of significant digits to use for formatting.
#' @return A character string.
#' @export
numvectorToString <- function(num, sigdigits) {
  return(paste(sprintf(paste0("%0.0", sigdigits, "f"), round(num, sigdigits)), collapse = ","))
}

#' Get member ID within a group
#'
#' This function returns the sequence of member IDs for a given group.
#'
#' @param grp The group identifier.
#' @param nums A vector of numbers containing the group identifiers.
#' @return A sequence of integers representing the member IDs.
#' @export
memberID <- function(grp, nums) {
  if (is.na(grp)) {
    ids <- 1:sum(is.na(nums))
  } else {
    ids <- 1:sum(nums == grp)
  }
  return(ids)
}

#' Group consecutive identical values
#'
#' This function takes a vector and returns a list of indices where consecutive elements are identical.
#'
#' @param x A vector.
#' @return A list of integer vectors.
#' @export
consecutive.groups <- function(x) {
  if (length(x) == 0) return(list())
  n <- length(x)
  if (n == 1) return(list(x))
  d <- c(1, diff(x))
  starts <- which(d != 1)
  if (length(starts) == 0) return(list(x))

  ends <- c(starts - 1, n)
  starts <- c(1, starts)

  lapply(seq_along(starts), function(i) x[starts[i]:ends[i]])
}
