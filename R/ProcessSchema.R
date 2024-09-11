
createWorker <- function(worker.id, workplace, resources) {
  # Create Worker dir and copy ORYZA files
  if (!dir.exists(paste0(workplace, "/", worker.id))) {
    if (dir.create(paste0(workplace, "/", worker.id), recursive = TRUE)) {
      file.copy(resources, paste0(workplace, "/", worker.id, "/", basename(resources)))
      # Set Worker status
      saveRDS("ready", file = paste0(workplace, "/", worker.id, "/", "status.rds"))
    }
  }
  return(worker.id)
}

canRunMore <- function(totalram, limit.ram = 0.2, limit.cpu = 0.9) {

  if (Sys.info()["sysname"] == "Windows") {
    sys.availram <- system2("wmic", args = "OS get FreePhysicalMemory", stdout = TRUE)[2]
    sys.cpu <- system2("wmic", args = "cpu get loadpercentage", stdout = TRUE, wait = TRUE)
  } else {

  }
  # Getting available RAM
  sys.availram <- as.numeric(trimws(sys.availram)) / 1024
  sys.availrampct <- round((sys.availram / sys.totalram), 2)

  # Getting CPU usage
  sys.cpu <- as.numeric(trimws(sys.cpu[2])) / 100

  return((sys.availrampct < limit.ram) | (sys.cpu > limit.cpu))

}

# Retries reading RDS files when RDS file is used by another process
force.readrds <- function(filename, ...) {
  if (!file.exists(filename)) {
    result <- NULL
  } else {
    repeat {
      result <- try(readRDS(file = filename), ...)
      if (class(result) != "try-error") break
    }
  }
  return(result)
}
