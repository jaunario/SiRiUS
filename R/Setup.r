#' Set up the SiRiUS environment
#'
#' This function sets up the necessary directories and configuration for the SiRiUS application.
#'
#' @param install.dir The directory where SiRiUS will be installed. If NULL or NA on Windows, a directory selection dialog will be shown.
#' @param install.oryza A boolean indicating whether to install the ORYZA model. Default is FALSE.
#' @param SPEED_STORAGE The path to a directory on a fast storage device for better performance.
#' @param verbose A boolean indicating whether to print progress messages. Default is TRUE.
#' @param ... Additional arguments (not used).
#' @return A character vector containing the paths of the created directories and the status of the ORYZA installation.
#' @export
sirius.setup <- function(install.dir, install.oryza = FALSE, SPEED_STORAGE = "", verbose = TRUE, ...) {

  if (Sys.info()["sysname"] == "Windows" && (is.null(install.dir) || is.na(install.dir))) {
    install.dir <- choose.dir()
  }

  if (is.na(install.dir) || is.null(install.dir)) {
    message("Kindly key-in the full path of directory where you want to put Sirius.")
    install.dir <- scan(what = "character", n = 1)
  }

  SIRIUS_HOME <- paste0(install.dir, "/Sirius")
  if (!dir.exists(SIRIUS_HOME)) dir.create(SIRIUS_HOME, recursive = TRUE)

  # For better performance, this directory should be in an SSD drive or
  # a drive with the the fastest writespeed on your system
  if (SPEED_STORAGE != "") {
    if (!dir.exists(SPEED_STORAGE)) dir.create(SPEED_STORAGE, recursive = TRUE)
  }

  # AgERA5 Settings
  AGERA5_ORYZADIR <-  "oryza/weather/agera5"

  sirius.dirs <- paste0(SIRIUS_HOME, "/",
    c("schemas",
      "oryza", unique("oryza/soil", SOILGRIDS_ORYZADIR), unique("oryza/weather", AGERA5_ORYZADIR), "oryza/variety",
      "data", "data/aoi", SOILGRIDS_TIFDIR)
  )

  attr(sirius.dirs, "created") <- try(sapply(sirius.dirs, manipulateR::force.directories, recursive = TRUE))

  if (install.oryza) {
    zip.oryza <- ifelse(Sys.info()[["sysname"]] == "Windows",
                        system.file(package = "SiRiUS", "oryza/ORYZA3_Win64bit.zip"),
                        system.file(package = "SiRiUS", "oryza/ORYZA3_Linux.zip"))
    status.oryza <- try(unzip(zipfile = zip.oryza, exdir = paste0(SIRIUS_HOME, "/oryza")))
  } else {
    status.oryza <- "not done (user preference)"
  }

  configfile <- system.file(package = "SiRiUS", "app/config.Rdata")
  if (configfile == "") {
    configfile <- paste0(grep("SiRiUS$", list.dirs(.libPaths()[1], recursive = FALSE), value = TRUE), "/app/config.Rdata")
  }

  save(list = c("SIRIUS_HOME", "SOILGRIDS_TIFDIR", "SOILGRIDS_ORYZADIR", "AGERA5_ORYZADIR", "SPEED_STORAGE"),
       file = configfile)
  load(configfile, envir = .GlobalEnv)
  return(c(sirius.dirs, status.oryza))
}

#' Start the SiRiUS application
#'
#' This function starts the SiRiUS application by loading the configuration and setting the working directory.
#' It can also launch the Shiny app.
#'
#' @param verbose A boolean indicating whether to print progress messages. Default is TRUE.
#' @param setup A boolean indicating whether to run `sirius.setup` before starting. Default is FALSE.
#' @param launch.app A boolean indicating whether to launch the Shiny application. Default is FALSE.
#' @param ... Additional arguments to be passed to `sirius.setup` if `setup` is TRUE.
#' @return The path to the SiRiUS home directory.
#' @export
sirius.start <- function(verbose = TRUE, setup = FALSE, launch.app = FALSE, ...) {
  message("SiRiUS Starting")
  configfile <- system.file(package = "SiRiUS", "app/config.Rdata")

  if (!file.exists(configfile) || setup) {
    sirius.setup(...)
    configfile <- system.file(package = "SiRiUS", "app/config.Rdata")
  }

  if (verbose) message("Loading SiRiUS config: ")
  load(configfile)
  load(configfile, envir = .GlobalEnv)

  if (verbose) message("Changing working directory to: ", SIRIUS_HOME)
  setwd(SIRIUS_HOME)

  if (launch.app) {
    shiny::runApp()
  }

  return(SIRIUS_HOME)
}