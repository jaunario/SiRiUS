#Title: Worker/Runner
# Tasks:
# 1. Run ORYZA
# 2. Rename output files (op.dat, res.dat)
# 3. Update progress of schema
# TODO: Make this generic. This will be useful for a Distributed computing system

# Checking appropriate working directory ----
cmd.args <- commandArgs(TRUE)
cmd.args <- subset(cmd.args, cmd.args != "--args")
if (dir.exists(cmd.args[1])) setwd(cmd.args[1]) else stop("Invalid working directory.")

# Checking job ----
tries <- 0
repeat {
  message("Getting job details. ")
  my.job <- try(readRDS("job.rds"), silent = TRUE)
  if (class(my.job) != "try-error") break else  Sys.sleep(runif(1))
  tries <- tries + 1
  if (tries > 10) stop("No job found. Try again later.")
}

# Worker preparing ----
saveRDS("busy", file = "status.rds")
load("../config.rdata")

message("Working on ", SCHEMA_NAME, ".")
opdat.newname <- sprintf("../out/cell%06g_OP.dat", my.job$cell[1])

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

# Sleep for a random amount of time (milliseconds) to allow controller create new workers, and more simultaneous jobs
Sys.sleep(runif(1))
# Done. ----
saveRDS("ready", file = "status.rds")
