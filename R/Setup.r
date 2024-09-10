
setup.sirius <- function(settings.file = system.file(package = "SiRiUS", "SiriusSettings.R"), install.oryza = TRUE, verbose = TRUE) {
  if (is.null(settings.file) || is.na(settings.file)) {
    SIRIUS_HOME <-  scan(what = "character", n = 1)

    SOILGRIDS_TIFDIR   <- scan(what = "character", n = 1)
    SOILGRIDS_ORYZADIR <- scan(what = "character", n = 1)

    AGERA5_ORYZADIR <-  scan(what = "character", n = 1)

    # For better performance, this directory should be in an SSD drive or
    # the fastest drive on your system
    SPEED_STORAGE <- scan(what = "character", n = 1)
  } else {
    ifelse(file.exists(settings.file), source(settings.file))
  }
  #remotes::install_github("rspatial/terra")
  #remotes::install_github("rspatial/geodata")
  save(list = c("SIRIUS_HOME", "SOILGRIDS_TIFDIR", "SOILGRIDS_ORYZADIR", "AGERA5_ORYZADIR", "SPEED_STORAGE"), file = "config.Rdata")

  sirius.dirs <- c("schemas",
                   "oryza", "oryza/soil", "oryza/weather", "oryza/variety",
                   "data", "data/aoi", "data/soilgrids")

  if (SPEED_STORAGE == "") SPEED_STORAGE <- "schemas"

  if (install.oryza){
    zip.oryza <- ifelse(Sys.info()[["sysname"]] == "Windows",
                        system.file(package = "SiRiUS", "oryza/ORYZA3_Win64bit.zip"),
                        system.file(package = "SiRiUS", "oryza/ORYZA3_Linux.zip"))
    status.oryza <- try(unzip(zipfile = zip.oryza, exdir = paste0(SIRIUS_HOME, "/oryza")))
  } else {
    status.oryza <- "not done (user preference)"
  }
  status.dirs <- try(sapply(paste0(SIRIUS_HOME, "/", sirius.dirs), dir.create, recursive = TRUE))
  return(c(status.dirs, status.oryza))
}
