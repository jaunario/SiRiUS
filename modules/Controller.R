# Title: Start Simulation
# TODO: Make this generic. This will be useful for a Distributed computing system

# Controller Settings
ORYZA_CORE      <- "./oryza/ORYZA3.exe" # Should be moved to app

SCHEMA_SELECTED <- "CF_CanTho_250m_2017-2022_MVMedium_daily"

# Override SCHEMA_SELECTED if passed as command line argument
if (!interactive()) {
  cargs <- commandArgs(trailingOnly = TRUE)
  if (length(cargs) > 0) {
    SCHEMA_SELECTED <- cargs[1]
  }
}
message("Selected Schema: ", SCHEMA_SELECTED)
schema.dir <- paste0("./schemas/", SCHEMA_SELECTED)

library(terra)

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

runJob <- function(worker, jobid) {

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


# Load schema ----
load(paste0(schema.dir, "/config.rdata"))
source("./modules/RerunBuilder.R")
source("./modules/SiriusSettings.R")
dat.schemaprog <- readRDS(paste0(schema.dir, "/progress_DF.rds"))

# Initiating Work session ----
sys.totalram <- system2("wmic", args = "ComputerSystem get TotalPhysicalMemory", stdout = TRUE)
sys.totalram <- as.numeric(trimws(sys.totalram[2])) / 1024^2

# Getting existing worker list
workers <- dir(schema.dir, pattern = "^worker_")
if (length(workers) > 0) {
  workers.status <- sapply(paste0(schema.dir, "/", workers, "/status.rds"), readRDS)

  # Updating delegated jobs status ----
  workers.jobs <- sapply(paste0(schema.dir, "/", workers, "/prevjobs.rds"), force.readrds, simplify = FALSE)
  workers.jobs <- do.call(rbind, workers.jobs)
  dat.schemaprog <- dplyr::rows_update(dat.schemaprog, workers.jobs, by = "cell")

  # Resetting unfinished jobs ----
  dat.schemaprog$status[!dat.schemaprog$status %in% c("done", "discard")] <- "available"
  saveRDS(dat.schemaprog, paste0(schema.dir, "/progress_DF.rds"))

  # Resetting worker status ----
  sapply(paste0(schema.dir, "/", workers, "/status.rds"), saveRDS, object = "ready")
  idle.workers <- workers
} else {
  idle.workers <- vector()
}

while (length(grep("available", dat.schemaprog$status)) > 0) {

  # Check Schema progress ----
  progress.pct <- round(sum(dat.schemaprog$status %in% c("done", "discard")) / nrow(dat.schemaprog) * 100, 2)
  message(SCHEMA_SELECTED, ": ", sprintf("%3.3f %% done.", progress.pct))
  if (progress.pct == 100) {
    message(SCHEMA_SELECTED, ": Done.", appendLF = TRUE)
    break
  }

  # Getting available jobs ----
  job.available <- which(dat.schemaprog$status == "available")[1]
  if (length(job.available) < 1) {

    message("Seems like no more jobs are available for this schema.")
    message("Will just wait 'til every job is done.")
    Sys.sleep(2)
    next
  }

  # Found a job, check if resources are available ----
  if (!canRunMore(totalram = sys.totalram)) {
    message("Low memory. Sleeping to wait for some memory to free up.")
    Sys.sleep(10)
    next
  }

  # Resources are ok, Finding available workers ----
  if (length(workers) > 0) {
    workers.status <- sapply(paste0(schema.dir, "/", workers, "/status.rds"), force.readrds)
    idle.workers <- workers[workers.status == "ready"]
  }

  if (length(idle.workers) > 0) {
    worker <- idle.workers[1]
  } else {
    # Recruiting Worker ----
    worker <- paste0("worker_", sample(1:100, 1)) # Create a worker with random id
    createWorker(worker.id = worker, workplace = schema.dir,
                 resources = c(ORYZA_CORE,
                               paste(schema.dir, "CONTROL.DAT", sep = "/"),
                               SCHEMA_VARIETYFILE,
                               paste(schema.dir, paste0(SCHEMA_NAME, ".exp"), sep = "/")))
    workers <- c(workers, worker)
  }

  # Update job status in progress db
  dat.schemaprog$status[job.available] <- "delegated"
  dat.schemaprog$run.sttime[job.available] <- Sys.time()
  saveRDS(dat.schemaprog, file = paste(schema.dir, "/progress_DF.rds", sep = "/"))

  # Assign job to worker
  saveRDS(dat.schemaprog[job.available, ], file = paste0(schema.dir, "/", worker, "/job.rds"))

  # Rerun File
  if (SCHEMA_WTHSRC == "agera5") {
    job.rerunparams <- c(SCHEMA_RERUNPARAMS, list(ISTN = dat.schemaprog$wthcell[job.available]))
  } else {
    job.rerunparams <- SCHEMA_RERUNPARAMS
  }

  if (length(job.rerunparams) > 0) {
    dat.reruns <- writeRerun(job.rerunparams, schema_name = SCHEMA_NAME, filename = paste(schema.dir, worker, paste0(SCHEMA_NAME, ".rer"), sep = "/"))
    #saveRDS(dat.reruns, file = paste0(schema.dir, "/reruns.rds"))
  }

  system2("RScript", args = c("./modules/Worker.R", paste0(schema.dir, "/", worker)), stdout = FALSE, wait = FALSE)

  # Updating delegated jobs status ----
  workers.jobs <- sapply(paste0(schema.dir, "/", workers, "/prevjobs.rds"), force.readrds, simplify = FALSE)
  workers.jobs <- do.call(rbind, workers.jobs)
  #if (is.null(workers.jobs)) next

  dat.schemaprog <- dplyr::rows_update(dat.schemaprog, workers.jobs, by = "cell")
  saveRDS(dat.schemaprog, paste0(schema.dir, "/progress_DF.rds"))

}

# Clean-up ----
# Delete worker files
sapply(paste0(schema.dir, "/", workers), unlink, recursive = TRUE)
saveRDS(Sys.Date(), file = paste0(schema.dir, "/schema_lastrun.rds"))
