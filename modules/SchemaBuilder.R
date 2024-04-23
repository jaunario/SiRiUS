# Title: Schema Builder
# Description:  
# The Schema builder allows the user to define a schema. A schema is an ORYZA simulation configuration over an area of interest.
# It's major components are: 
# 1. The Area of Interest - could be shapefile, raster, geographical bounding box/extent
# 2. Weather Dataset - Must exist in the inventory/repository Can be historical 
# 
SIRIUS_HOME = "W:/Projects/SiRiUS"
# Schema Parameters ----
SCHEMA_NAME = "ProtoRun01"

## Simulation Period ----
SCHEMA_STARTYEAR = 2001
SCHEMA_ENDYEAR   = 2020
SCHEMA_PLANTDOY  = c(91, 322)
#SCHEMA_EMERGDOY  = 15

## Area of Interest ----
SCHEMA_AOIFILE   = "./data/aoi/Can_Tho/CanTho.shp"

## ORYZA Files ----
SCHEMA_EXPFILE = "./oryza/ir_va01.exp" # Experiment file 
# TODO: Should have a builder, 
#   - Experiment file for settings that doesn't change over time and location
#   - Rerun file for settings that change over time and location 

SCHEMA_CULTIFILE = "./oryza/cultivar_v1.crp" # Cultivar File

# TODO: Probably good to have an option to 
# SCHEMA_SAVERERUNS = FALSE
SCHEMA_RERUNFILE   = paste0(SCHEMA_NAME,".rer") # Rerun File
SCHEMA_RERUNPARAMS = list()


SCHEMA_SOILSRC          = "soilgrids.org"
SCHEMA_SOILGRIDS_SUBSET = "Can Tho" # Shouldn't matter if soilsrc is not soilgrids

# SOILGRIDS Settings (should be moved to app)
SOILGRIDS_TIFDIR   = "data/soilgrids"
SOILGRIDS_ORYZADIR = "data/soil"
# If source is soilgrids.org, add option to start script to generate soil files using API will be started 

SCHEMA_WTHSRC  = "agera5"
SCHEMA_AGERA5_SUBSET  = "Can Tho" # Shouldn't matter if wthsrc is not agera5

# SOILGRIDS Settings (should be moved to app)
AGERA5_ORYZADIR = "data/weather/agera5"


SCHEMA_CONTSIM = TRUE
# Implications of Parameter
#            |  TRUE   |  FALSE  | 
# CONTROL _________________________
#   SOILKILL |  'NO'   |  'YES'  |
# RERUN ___________________________
#   FILEI2   |   One   |   All   | Soil
#   ISTN     |   One   |   All   | Weather
#   IYEAR    |   All   |   One   | 
#   STTIME   |   All   |   One   | 
# 
# TODO: When Soilfile is in the rerun file, will it mess up SOILKILL?
# When 'One' is indicated, can it be omitted and placed in Exp file instead?

# Advanced Options
SCHEMA_SENSITIVITYSOIL = TRUE
SCHEMA_ENSEMBLE        = FALSE

# Schema Creation ----

## Workspace Preparation ----
library(terra)
library(curl)

source("./modules/SoilBuilder.R")

# Create Schema Dirs and Subdirs
dir.create(paste("./schemas",SCHEMA_NAME, sep = "/"))
dir.create(paste("./schemas",SCHEMA_NAME, "out", sep = "/"))

## Simulation Period ----
#sim.years <- SCHEMA_STARTYEAR:SCHEMA_ENDYEAR # Need to expand.grid this thing with EMDs
#dat.simdates <- expand.grid(sim.years, paste0(SCHEMA_PLANTDOY, "."), KEEP.OUT.ATTRS = FALSE)
#colnames(dat.simdates) <- c("IYEAR", "STTIME")
#SCHEMA_SIMDATES <- dat.simdates[with(dat.simdates, order(IYEAR, STTIME)),]

## Standardized AOI ----
if(grepl(".shp$", SCHEMA_AOIFILE)) {
  aoi.gis <- terra::vect(SCHEMA_AOIFILE)
} else if(grepl(".tif$", SCHEMA_AOIFILE)){
  aoi.gis <- terra::rast(SCHEMA_AOIFILE)
} else {
  stop("Unsupported AOI file.")
}

if(SCHEMA_SOILSRC == "soilgrids.org"){
  rst.base <- terra::rast(getSoilGridsRaster(aoi = aoi.gis))
} else {
  # TODO: Parameter to set base raster
}

if(class(aoi.gis)=="SpatVector"){
  rst.aoi <- terra::rasterize(aoi.gis, rst.base)
} else if(class(aoi.gis)=="SpatRaster"){
  rst.aoi <- terra::resample(aoi.gis, rst.base)
}

rst.aoi_cells <- rast(rst.aoi, vals = 1:ncell(rst.aoi))
names(rst.aoi_cells) <- "cell"
rst.aoi_cells <- mask(rst.aoi_cells, rst.aoi, maskvalue=NA)
rst.aoi_cells <- writeRaster(rst.aoi_cells, paste("./schemas", SCHEMA_NAME, "baseraster.tif", sep = "/"), datatype="INT2U", overwrite=TRUE)
cells.aoi <- values(rst.aoi_cells)
cells.aoi <- cells.aoi[!is.na(cells.aoi)]
dat.aoi <- data.frame(cell=cells.aoi, xyFromCell(rst.aoi_cells, cells.aoi))
#TODO: Remove pixels which are not cropland using LCLU Layers, e.g. ESRI Living Atlas or ESA Globe Cover

# Progress DF
if(SCHEMA_CONTSIM){
  dat.schemaprog <- data.frame(dat.aoi, status=rep("available", nrow(dat.aoi)), run.sttime=as.POSIXct(NA), run.entime=as.POSIXct(NA))
  dat.schemaprog$status <- factor(dat.schemaprog$status, levels=c("available", "delegated", "simulating", "done", "error", "discard"))
  saveRDS(dat.schemaprog, file = paste("./schemas", SCHEMA_NAME, "progress_DF.rds", sep="/"))
  
  ## CONTROL.DAT ----
  txt.controldat <- c(
    "CONTROLFILE = 'CONTROL.DAT'",
    "strun = 1",
    "FILEON   = 'res.dat'      ! Output file",
    "FILEOL   = 'model.log'    ! Log file",
    paste0("FILEIT   = '", basename(SCHEMA_EXPFILE), "'    ! Experimental data"),
    paste0("FILEI1   = '", basename(SCHEMA_CULTIFILE), "'  ! Crop data"),
    paste0("FILEIR   = '", SCHEMA_RERUNFILE, "'            ! Rerun file"),
    paste0("FILEI2   = '", paste0(SCHEMA_NAME,".sol"), "'  ! Soil File"),
    paste0("SOILKILL = 'NO'  !* 'NO' soil processes continue after crop maturity."),
    "PRDEL    = 1.    ! Output time step (day)",
    "IPFORM   = 5     ! Code for output table format:",
    "                 ! 4 = spaces between columns",
    "                 ! 5 = TAB's between columns (spreadsheet output)",
    "                 ! 6 = two column output",
    "COPINF   = 'N'   ! Switch variable whether to copy the input files",
    "                 ! to the output file ('N' = do not copy,",
    "                 ! 'Y' = copy)",
    "DELTMP   = 'Y'   ! Switch variable what should be done with the",
    "                 ! temporary output file ('N' = do not delete,",
    "                 ! 'Y' = delete)",
    "IFLAG    = 1100  ! Indicates where weather error and warnings",
    "                 ! go (1101 means errors and warnings to log",
    "                 ! file, errors to screen, see FSE manual)")
  writeLines(txt.controldat, con = paste("./schemas", SCHEMA_NAME,"CONTROL.DAT", sep="/"))
  
} else {
  #TODO: Finish this later
}

if(SCHEMA_WTHSRC=="agera5"){
  wth.dir <- normalizePath(paste(SIRIUS_HOME, AGERA5_ORYZADIR, SCHEMA_AGERA5_SUBSET, sep="/"))
  SCHEMA_RERUNPARAMS <- c(SCHEMA_RERUNPARAMS, list(WTRDIR=paste0("'", wth.dir, "'"), CNTR="'cdsag'"))
}

# Save AOI base raster
save(list=c("SIRIUS_HOME", grep("^SCHEMA", ls(), value = TRUE)), file = paste("./schemas", SCHEMA_NAME, "config.rdata", sep="/"))
# Save Configuration

saveRDS(SIRIUS_HOME, file = paste("./schemas", SCHEMA_NAME, "sirius_home.rds", sep="/"))
