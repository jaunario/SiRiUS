SIRIUS_HOME <- ""

# SOILGRIDS Settings
SOILGRIDS_TIFDIR   <- "data/soilgrids/raster" # Dedicated for raster downloads. Use of soilgrids api will use json.
SOILGRIDS_ORYZADIR <- "oryza/soil"
SOILGRIDS_TRANSLATOR <- system.file(package = "SiRiUS", "app/Sirius_SoilGrids2ORYZA.R")
HYDRAULICS_EXE    <- "tools/soilhydrau.exe"
HYDRAULICS_EXE    <- ifelse(!file.exists(HYDRAULICS_EXE), "", HYDRAULICS_EXE)

# AgERA5 Settings
AGERA5_ORYZADIR <-  "oryza/weather/agera5"

# Controller Settings
ORYZA_CORE    <- "oryza/ORYZA3.exe" # Should be moved to app

# Oryzaparams for Experiment and Rerun Files
.onLoad <- function(libname, pkgname) {
  data("sysdata", package = pkgname, envir = parent.env(environment()))
  #data("config", package = pkgname, envir = parent.env(environment()))
}
# If setup was run, above variables will be saved to config.RData
# if (exists("SIRIUS_HOME") && file.exists(paste0(SIRIUS_HOME, "/config.Rdata"))) load(paste0(SIRIUS_HOME, "/config.Rdata"))