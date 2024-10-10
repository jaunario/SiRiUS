# Title: Start Simulation
# TODO: Make this generic. This will be useful for a Distributed computing system

# Controller Settings
# Override SCHEMA_SELECTED if passed as command line argument
if (!interactive()) {
  cargs <- commandArgs(trailingOnly = TRUE)
  if (length(cargs) > 0) {
    SCHEMA_SELECTED <- cargs[1]
  }
} else if (!exists("SCHEMA_SELECTED")) {
  SCHEMA_SELECTED <- list.dirs(paste0(SIRIUS_HOME, "/schemas"), recursive = FALSE, full.names = FALSE)[8]
}

message("Selected Schema: ", SCHEMA_SELECTED)
schema.dir <- paste0("./schemas/", SCHEMA_SELECTED)

# Load schema ----
load(paste0(schema.dir, "/schemaconfig.rdata"))
#source("./modules/RerunBuilder.R")
#source("./modules/SiriusSettings.R")
dat.schemaprog <- readRDS(paste0(schema.dir, "/progress_DF.rds"))

# Initiating Work session ----
if (Sys.info()[["sysname"]] == "Windows") {
  sys.totalram <- system2("wmic", args = "ComputerSystem get TotalPhysicalMemory", stdout = TRUE)
  sys.totalram <- as.numeric(trimws(sys.totalram[2])) / 1024^2
} else {
  #TODO: Test
  sys.totalram <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'"))
}

# Getting existing worker list
workers <- dir(schema.dir, pattern = "^worker_")
if (length(workers) > 0) {
  workers.status <- sapply(paste0(schema.dir, "/", workers, "/status.rds"), readRDS)

  # Updating delegated jobs status ----
  workers.jobs <- sapply(paste0(schema.dir, "/", workers, "/prevjobs.rds"), force.readrds, simplify = FALSE)
  workers.jobs <- do.call(rbind, workers.jobs)
  if (!is.null(workers.jobs)) {
    dat.schemaprog <- dplyr::rows_update(dat.schemaprog, workers.jobs, by = "cell")
  }
  # Resetting unfinished jobs ----
  dat.schemaprog$status[!dat.schemaprog$status %in% c("done", "discard")] <- "available"
  saveRDS(dat.schemaprog, paste0(schema.dir, "/progress_DF.rds"))
  # Resetting worker status ----
  sapply(paste0(schema.dir, "/", workers, "/status.rds"), saveRDS, object = "ready")
  sapply(paste0(schema.dir, "/", workers, "/job.rds"), file.remove)
  idle.workers <- workers
} else {
  idle.workers <- vector()
}

#xx <- 0
while (length(grep("available", dat.schemaprog$status)) > 0) {
  #xx <- xx + 1
  #if (xx > 100) break
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
    message("Will just wait 'til every worker is done.")
    Sys.sleep(10)
    next
  }

  # Found a job, check if resources are available ----
  if (!canRunMore(totalram = sys.totalram)) {
    message("Low memory. Sleeping to wait for some memory to free up.")
    Sys.sleep(10)
    next
  }

  # Resources are ok, Finding available workers ----
  repeat {
    #message(paste(idle.workers, collapse = "\n"))
    worker <- idle.workers[1]
    if (!is.na(worker)) {
      idle.workers <- idle.workers[-1]
      break
    }

    # update existing workers statuses
    if (length(workers) > 0) {
      workers.status <- sapply(paste0(schema.dir, "/", workers, "/job.rds"), file.exists)
      idle.workers <- workers[!workers.status]
    }

    # create new worker if none available
    if ((length(idle.workers) == 0) && (length(workers) < 12)) {
      worker <- paste0("worker_", sample(1:100, 1)) # Create a worker with random id
      createWorker(worker.id = worker, workplace = schema.dir,
                   resources = c(ORYZA_CORE,
                                 SCHEMA_VARIETYFILE))
                                #  paste(schema.dir, paste0(SCHEMA_NAME, ".exp"), sep = "/")))
      workers <- c(workers, worker)
      idle.workers <- c(idle.workers, worker)
    }
  }


  # Update job status in progress db
  dat.schemaprog$status[job.available] <- "delegated"
  dat.schemaprog$run.sttime[job.available] <- Sys.time()
  saveRDS(dat.schemaprog, file = paste0(schema.dir, "/progress_DF.rds"))

  # Assign job to worker
  saveRDS(dat.schemaprog[job.available, ], file = paste0(schema.dir, "/", worker, "/job.rds"))

  # Run Worker
  system2("RScript", args = c(system.file(package = "SiRiUS", "app/Worker.R"), shQuote(paste0(schema.dir, "/", worker))), stdout = FALSE, wait = FALSE)

  # Updating delegated jobs status ----
  workers.jobs <- sapply(paste0(schema.dir, "/", workers, "/prevjobs.rds"), force.readrds, simplify = FALSE, USE.NAMES = FALSE)
  workers.jobs <- do.call(rbind, workers.jobs)
  #if (is.null(workers.jobs)) next

  if (!is.null(workers.jobs)) {
    dat.schemaprog <- dplyr::rows_update(dat.schemaprog, workers.jobs, by = "cell")
    saveRDS(dat.schemaprog, paste0(schema.dir, "/progress_DF.rds"))
  }

  #  interfere <- readline()
  #  if (interfere == "Q") break
}

# Clean-up ----
# Delete worker files
sapply(paste0(schema.dir, "/", workers), unlink, recursive = TRUE)
saveRDS(Sys.Date(), file = paste0(schema.dir, "/schema_lastrun.rds"))
