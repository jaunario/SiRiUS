
SIRIUS_HOME <-  "~/SiRiUS"

# SOILGRIDS Settings (should be moved to app)
SOILGRIDS_TIFDIR   <- "data/soilgrids"
SOILGRIDS_ORYZADIR <- "oryza/soil"

# SOILGRIDS Settings (should be moved to app)
AGERA5_ORYZADIR <-  "oryza/weather/agera5"

# For better performance, this directory should be in an SSD drive or
# the fastest drive on your system
SPEED_STORAGE <- ""

# Controller Settings
ORYZA_CORE      <- "./oryza/ORYZA3.exe" # Should be moved to app

# If setup was run, above variables will be saved to config.RData
if (exists("SIRIUS_HOME") && file.exists(paste0(SIRIUS_HOME, "/config.Rdata"))) load(paste0(SIRIUS_HOME, "/config.Rdata"))