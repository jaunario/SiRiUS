#Title: Worker/Runner
# Tasks:
# 1. Run ORYZA
# 2. Rename output files (op.dat, res.dat)
# 3. Update progress of schema

# Checking job ----
if(file.exists("job.rds")){
# Worker preparing ----
  saveRDS("busy", file = "status.rds")
  load("../config.rdata")
  message("Working on ", SCHEMA_NAME,".")
  
  #source(paste0(SIRIUS_HOME, "/modules/RerunBuilder.R"))
  message("Getting job details. ")
  my.job <- readRDS("job.rds")
  
  #my.job <- readRDS(paste0(schema.dir, "/",workers[1],"/job.rds"))
  
  if(!is.na(my.job$cell)){
    opdat.newname <- sprintf("../out/cell%06g_OP.dat", my.job$cell[1])
   } else if(my.job$unit=="time"){
    message("Probably lost")
  }
# Running ORYZA ----

  worker.outcome <- system("./ORYZA3", intern = TRUE)
  if(sum(grepl("Fatal execution error", worker.outcome))>0) {
    my.job$status <- "error"
    my.job$run.entime <- Sys.time()
    saveRDS(my.job, "job.rds")
  } else { 
    my.job$status <- "done"
    my.job$run.entime <- Sys.time()
    saveRDS(my.job, paste0("job.rds"))
    
    #if(success){

# Saving output ----
    file.rename("OP.dat", opdat.newname)
     
  }
  

  if (file.exists("prevjobs.rds")){
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

# Done. ----
saveRDS("ready", file = "status.rds")
