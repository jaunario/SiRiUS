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
