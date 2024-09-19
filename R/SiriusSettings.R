# Default folders used for Soilgrids and AgERA5
SIRIUS_HOME <- "./Sirius"

# SOILGRIDS Settings
SOILGRIDS_TIFDIR   <- "data/soilgrids/raster" # Dedicated for raster downloads. Use of soilgrids api will use json.
SOILGRIDS_ORYZADIR <- "oryza/soil"

# AgERA5 Settings
AGERA5_ORYZADIR <-  "oryza/weather/agera5"

# For better performance, this directory should be in an SSD drive or
# a drive with the the fastest writespeed on your system
SPEED_STORAGE <- ""

# Controller Settings
ORYZA_CORE    <- "oryza/ORYZA3.exe" # Should be moved to app

# If setup was run, above variables will be saved to config.RData
# if (exists("SIRIUS_HOME") && file.exists(paste0(SIRIUS_HOME, "/config.Rdata"))) load(paste0(SIRIUS_HOME, "/config.Rdata"))