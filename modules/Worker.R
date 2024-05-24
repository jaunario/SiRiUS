#Title: Worker/Runner
# Tasks:
# 1. Run ORYZA
# 2. Rename output files (op.dat, res.dat)
# 3. Update progress of schema
# TODO: Make this generic. This will be useful for a Distributed computing system

# Checking job ----
if (file.exists("job.rds")) {

  # Worker preparing ----
  saveRDS("busy", file = "status.rds")
  load("../config.rdata")
  message("Working on ", SCHEMA_NAME, ".")

  message("Getting job details. ")
  my.job <- readRDS("job.rds")

  if ("cell" %in% colnames(my.job)) {
    opdat.newname <- sprintf("../out/cell%06g_OP.dat", my.job$cell[1])
  } else {
    opdat.newname <- sprintf("../out/y%04g_plantdoy%03g_OP.dat", my.job$IYEAR, my.job$EMD)
  }
  # Running ORYZA ----
  worker.outcome <- system("./ORYZA3", intern = TRUE)

  if (sum(grepl("Fatal execution error", worker.outcome)) > 0) {
    my.job$status <- "error"
    my.job$run.entime <- Sys.time()
    saveRDS(my.job, "job.rds")
  } else {
    my.job$status <- "done"
    my.job$run.entime <- Sys.time()

    # Moving output to schema output folder ----
    file.rename("OP.dat", opdat.newname)
  }

  if (file.exists("prevjobs.rds")) {
    dat.prevjobs <- readRDS("prevjobs.rds")
    dat.prevjobs <- rbind(dat.prevjobs, my.job)
    saveRDS(dat.prevjobs, "prevjobs.rds")
  } else {
    dat.prevjobs <- my.job
    saveRDS(dat.prevjobs, "prevjobs.rds")
  }
  file.remove("job.rds")
} else {
  message("I'm jobless.")
}
# Sleep for a random amount of time (milliseconds) to allow controller create new workers, and more simultaneous jobs
Sys.sleep(runif(1))
# Done. ----
saveRDS("ready", file = "status.rds")
