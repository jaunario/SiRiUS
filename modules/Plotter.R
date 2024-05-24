# Title: Plot ORYZA simulation output maps

SCHEMA_SELECTED = "SensitivityRun02" 
ORYZA_FIELDS = c("RUNNUM", "WRR14", "S_CO2C", "S_CH4C", "S_N2ON")

library(terra)

schema.dir <- paste0("./schemas/", SCHEMA_SELECTED)

rst.base <- rast(paste0(schema.dir, "/baseraster.tif"))

files.opdat <- dir(path=paste0(schema.dir, "/out"), pattern = "cell.*_OP.dat$", full.names=TRUE)
message(SCHEMA_SELECTED, ": Reading output files.", appendLF = TRUE)
bar.progress <- txtProgressBar(min = 1, max = length(files.opdat), initial = 0)
for(i in seq_along(files.opdat)){
  dat.thiscell <- read.table(files.opdat[i], header=TRUE)
  dat.thiscell <- dat.thiscell[, ORYZA_FIELDS]
  dat.thiscell$cell <- as.numeric(sub("cell", "", sub("_OP.dat", "", basename(files.opdat[i]))))
  if(i == 1) {
    dat.op <- dat.thiscell
  } else {
    dat.op <- rbind(dat.op, dat.thiscell)
  }
  setTxtProgressBar(bar.progress, i)
}
close(bar.progress)
lst.maps <- list()
for(i in unique(dat.op$RUNNUM)){
  dat.thisrun <- subset(dat.op, RUNNUM==i)
  for(j in 2:length(ORYZA_FIELDS)){
    rst.thisfield <- rast(rst.base)
    rst.thisfield[dat.thisrun$cell] <- dat.thisrun[, ORYZA_FIELDS[j]]
    if(j==2) rst.thisrun <- rst.thisfield else rst.thisrun <- c(rst.thisrun, rst.thisfield)
  }
  names(rst.thisrun) <- ORYZA_FIELDS[-1]
  plot(rst.thisrun)
  lst.maps <- c(lst.maps, rst.thisrun)
}
xx <- sapply(files.opdat, read.table, header=FALSE, skip = 1, nrows = 1)

