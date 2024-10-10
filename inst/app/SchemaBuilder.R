# Title: Schema Builder
# Description:
# The Schema builder allows the user to define a schema.
# A schema is an ORYZA simulation configuration over an area of interest.
# Its major components are:
# 1. The Area of Interest - could be shapefile, raster, or bounding box/extent
# 2. Simulation Period
# 3. Weather Dataset - Must exist in the inventory/repository Can be historical or future climate
# Use SchemaTemplate.txt in the Tools folder for reference
# TODO: Variety file should be location specific

if (!interactive()) {
  SCHEMA_FILE <- commandArgs(trailingOnly = TRUE)
  if (length(SCHEMA_FILE) > 0 && file.exists(SCHEMA_FILE)) {
    message("Schema file: ", SCHEMA_FILE)
  } else {
    stop("Error reading: ", SCHEMA_FILE, " does not exist")
  }
} else if (!exists("SCHEMA_FILE")) {
  SCHEMA_FILE <- system.file(package = "SiRiUS", "sample/Schema_Sample.txt")
}

# Read schema setup file. ----
source(SCHEMA_FILE)

# Creating the schema. ----
if (SCHEMA_NAME == "auto") {
  ## Schema name construction ----
  message("[Generated] ", appendLF = FALSE)
  SCHEMA_NAME <- paste(SCHEMA_COUNTRY_ISO,
                       ifelse(grepl("gadm", SCHEMA_AOIFILE),
                              SCHEMA_AOIFILE,
                              basename(sub(tools::file_ext(SCHEMA_AOIFILE), "", SCHEMA_AOIFILE))),
                       paste(SCHEMA_AOIFILTER, collapse = "-"),
                       SCHEMA_STARTYEAR,
                       SCHEMA_ENDYEAR,
                       sep = "_")
}
message(SCHEMA_NAME)

## Workspace Preparation ----
SCHEMA_INTERMDIR <- paste(ifelse(SPEED_STORAGE == "", "./schemas", SPEED_STORAGE),
                          SCHEMA_NAME, "interm", sep = "/")
if (!dir.exists(paste("./schemas", SCHEMA_NAME, "maps", sep = "/"))) dir.create(paste("./schemas", SCHEMA_NAME, "maps", sep = "/"), recursive = TRUE)

## Variety file ----
if (SCHEMA_VARIETYFILE == "auto") {
  SCHEMA_VARIETYFILE <- "./oryza/variety/IR72.crp" # Use IR72 crop variety file
}

## Soil file ----
if (SCHEMA_SOILFILE == "auto") {
  SCHEMA_SOILFILE <- "soilgrids.org"
  # SCHEMA_SOILFILE <- paste0(SCHEMA_NAME, ".sol")
}

## Standardized AOI ----
if (grepl(".shp$", SCHEMA_AOIFILE)) {
  aoi.gis <- terra::vect(SCHEMA_AOIFILE)
} else if (grepl(".tif$", SCHEMA_AOIFILE)) {
  aoi.gis <- terra::rast(SCHEMA_AOIFILE)
} else if (grepl("^gadm", SCHEMA_AOIFILE)) {
  lvl <- as.numeric(sub("gadm.", "", SCHEMA_AOIFILE))
  aoi.gis <- geodata::gadm(SCHEMA_COUNTRY_ISO, level = lvl, path = "./data/aoi")
  dat.gis <- data.frame(aoi.gis)
  idx.selected <- unlist(lapply(dat.gis[, paste0(c("NAME_", "VARNAME_"), lvl)], match, x = SCHEMA_AOIFILTER))
  aoi.gis <- aoi.gis[na.omit(idx.selected)]
} else {
  stop("Unsupported AOI file.")
}

# TODO: base raster if not soilgrids nor agera5
if (SCHEMA_BASERASTER == "soilgrids.org") {
  this.dl <- try(getSoilGridsRaster(aoi = aoi.gis,
                                    path = paste0(SOILGRIDS_TIFDIR, "/temp")),
                 silent = TRUE)
  rst.base <- rast(this.dl)
} else if (SCHEMA_BASERASTER == "agera5") {
  rst.base <- terra::rast()
  res(rst.base) <- 0.1
} else {
  stop("Unsupported base raster.")
}

if (class(aoi.gis) == "SpatVector") {
  rst.aoi <- terra::rasterize(aoi.gis, rst.base)
} else if (class(aoi.gis) == "SpatRaster") {
  rst.aoi <- terra::resample(aoi.gis, rst.base)
  rst.aoi <- terra::crop(rst.aoi, aoi.gis)
}

rst.aoicells <- rast(rst.aoi, vals = 1:ncell(rst.aoi))
names(rst.aoicells) <- "cell"
rst.aoicells <- mask(rst.aoicells, rst.aoi, maskvalue = NA)

# Remove pixels which are not cropland using LCLU Layers, e.g. ESRI Living Atlas or ESA Globe Cover
if (exists("SCHEMA_MASKFILE") && !is.na(SCHEMA_MASKFILE)) {
  rst.lulc <- terra::rast(SCHEMA_MASKFILE)
  # For cropping, prevent 'cannot allocate' error
  aoi.buff <- project(aoi.gis, crs(rst.lulc))
  aoi.buff <- buffer(aoi.buff, 500)
  rst.lulc <- crop(rst.lulc, aoi.buff, extend = TRUE)
  rst.lulc <- project(rst.lulc, crs(rst.aoi))

  rst.ncells <- rast(rst.lulc, vals = 1)
  rst.ncells <- resample(rst.ncells, rst.aoi, method = "sum")
  rst.lulc <- !rst.lulc %in% SCHEMA_MASKVAL
  rst.lulc <- resample(rst.lulc, rst.aoi, method = "sum")
  rst.lulc <- rst.lulc / rst.ncells
  #rst.lulc <- crop(rst.lulc, project(rst.aoi, crs(rst.lulc)))
  rst.aoicells <- mask(rst.aoicells, rst.lulc >= SCHEMA_AGGTHRES, maskvalue = FALSE)
  rm(rst.lulc, rst.ncells, aoi.buff)
}

if (SCHEMA_SOILFILE == "soilgrids.org") {
  message("Generating Soil files from SoilGrids.")
  # TODO: Generate file if specified
  # if (SCHEMA_GENERATE_SOIL) source(SOILGRIDS_TRANSLATOR)
  soil.invfile <- paste(SOILGRIDS_ORYZADIR, SCHEMA_BUILTINS_SUBSET, "inventory.rds", sep = "/")
  if (!file.exists(soil.invfile)) {
    files.soil <- dir(paste(SOILGRIDS_ORYZADIR, SCHEMA_BUILTINS_SUBSET, sep = "/"), pattern = ".sol$")
    dat.soil <- sapply(sub("\\.sol", "", files.soil), strsplit, split = "_", USE.NAMES = FALSE, simplify = TRUE)
    dat.soil <- do.call(rbind, dat.soil)
    if (ncol(dat.soil) == 2) {
      dat.soil <- data.frame(filename = files.soil, cell = as.numeric(sub("cell", "", dat.soil[, 2])))
      dat.soil <- cbind(dat.soil, xyFromCell(rst.aoicells, dat.soil$cell))
    } else {
      dat.soil <- data.frame(filename = files.soil, x = as.numeric(sub("x", "", dat.soil[, 2])), y = as.numeric(sub("y", "", dat.soil[, 3])))
      dat.soil$cell <- cellFromXY(rst.aoicells, dat.soil[, c("x", "y")])
    }
    saveRDS(dat.soil, soil.invfile)
  } else {
    dat.soil <- readRDS(soil.invfile)
  }
  rst.exists <- rast(rst.aoicells)
  rst.exists[dat.soil$cell] <- 1
  rst.aoicells <- mask(rst.aoicells, rst.exists, maskvalue = NA)
}
# Save AOI base raster
rst.aoicells <- writeRaster(rst.aoicells, paste("./schemas", SCHEMA_NAME, "baseraster.tif", sep = "/"), datatype = "INT2U", overwrite = TRUE)

cells.aoi <- values(rst.aoicells)
cells.aoi <- cells.aoi[!is.na(cells.aoi)]
dat.aoi <- data.frame(cell = cells.aoi, xyFromCell(rst.aoicells, cells.aoi))

## Simulation Period and Weather files ----

### Simulation Period ----
# Should go to rerun files. for SOILKILL='NO' Try STTIME = 1 and IYEAR=SCHEMA_STARTYR
# EMD = SCHEMA_PLANTDOY and EMYEAR = SCHEMA_STARTYEAR:SCHEMA_ENDYEAR
# Modeler problems...
if (sum(is.na(c(SCHEMA_STARTYEAR, SCHEMA_ENDYEAR, SCHEMA_PLANTDOY))) == 0) {
  sim.years <- SCHEMA_STARTYEAR:SCHEMA_ENDYEAR # Need to expand.grid this thing with EMDs
  dat.simdates <- expand.grid(sim.years, SCHEMA_PLANTDOY, KEEP.OUT.ATTRS = FALSE)
  colnames(dat.simdates) <- c("EMYR", "EMD")
  dat.simdates <- as.data.frame(dat.simdates)
  dat.simdates <- dat.simdates[with(dat.simdates, order(EMYR, EMD)), ]
  dat.simdates$IYEAR <- dat.simdates$EMYR

  if ("STTIME" %in% names(SCHEMA_RERUNPARAMS)) {
    if (length(SCHEMA_PLANTDOY) == length(SCHEMA_RERUNPARAMS$STTIME)) {
      # Assume one-one correspondence of STTIME and PLANTDOY/EMD
      dat.simdates$STTIME <- SCHEMA_RERUNPARAMS$STTIME[sapply(dat.simdates$EMD, match, SCHEMA_PLANTDOY)]
    } else {
      warning("STTIME parameter not equal to EMD. Setting STTIME = EMD.")
      dat.simdates$STTIME <- dat.simdates$EMD
    }
    SCHEMA_RERUNPARAMS <- SCHEMA_RERUNPARAMS[-match("STTIME", names(SCHEMA_RERUNPARAMS))]
  }

}
SCHEMA_RERUNPARAMS <- c(SCHEMA_RERUNPARAMS, list(simdates = dat.simdates))
#SCHEMA_SIMDATES <- dat.simdates

### Weather Files ----
if (file.exists(SCHEMA_WTHSRC)) {
  SCHEMA_WTHSRC <- normalizePath(SCHEMA_WTHSRC)
  wth.year <- as.numeric(tools::file_ext(SCHEMA_WTHSRC)) + 2000
  if (wth.year > 2800) wth.year <- wth.year - 1000

  SCHEMA_EXPPARAMS <- c(SCHEMA_EXPPARAMS,
                        WTRDIR = normalizePath(dirname(SCHEMA_WTHSRC)),
                        CNTR   = substr(basename(SCHEMA_WTHSRC), 1, regexpr("[0-9]", basename(SCHEMA_WTHSRC)) - 1),
                        ISTN   = substr(basename(SCHEMA_WTHSRC), regexpr("[0-9]", basename(SCHEMA_WTHSRC)), regexpr("\\.", basename(SCHEMA_WTHSRC)) - 1),
                        IYEAR  = wth.year,
                        STTIME = 1,
                        EMYR  = wth.year,
                        EMD = SCHEMA_PLANTDOY[1])

} else if (SCHEMA_WTHSRC == "agera5") {
  rst.wth <- rast()
  names(rst.wth) <- "cdsag" # TODO: Add option for user to specify this
  res(rst.wth) <- 0.1

  dat.aoi$wthcell <- cellFromXY(rst.wth, dat.aoi[, c("x", "y")])

  wth.dir <- normalizePath(paste(SIRIUS_HOME, AGERA5_ORYZADIR, SCHEMA_BUILTINS_SUBSET, sep = "/"), winslash = "\\")
  SCHEMA_EXPPARAMS <- c(SCHEMA_EXPPARAMS,
                        list(WTRDIR = wth.dir,
                             CNTR = "cdsag",
                             IYEAR = SCHEMA_STARTYEAR,
                             STTIME = SCHEMA_PLANTDOY[1] - 5))

  # If agera5 is the chosen weather source, simulation period will be defined by
  # specified SCHEMA_STARTYEAR, SCHEMA_ENDYEAR, SCHEMA_PLANT
}

# TODO: Should have a builder
#   - Experiment file for settings that doesn't change over time and location
#   - Rerun file for settings that change over time and location
if (SCHEMA_EXPFILE == "auto") {
  SCHEMA_BASEEXPFILE <- system.file(package = "SiRiUS", "./sample/experiment_template.exp") # ir_va01.exp" # Experiment file
  #dat.expparams <- readEXPFILE(SCHEMA_BASEEXPFILE)
}
#SCHEMA_EXPPARAMS <- c(SCHEMA_EXPPARAMS, IYEAR=SCHEMA_STARTYEAR, STTIME=SCHEMA_PLANTDOY[1])

# Jobs df
# Jobs distributes the SCHEMA by location
dat.schemaprog <- data.frame(dat.aoi, status = rep("available", nrow(dat.aoi)), run.sttime = as.POSIXct(NA), run.entime = as.POSIXct(NA))
dat.schemaprog$status <- factor(dat.schemaprog$status, levels = c("available", "delegated", "simulating", "done", "error", "discard"))

saveRDS(dat.schemaprog, file = paste("./schemas", SCHEMA_NAME, "progress_DF.rds", sep = "/"))


# Save Configuration
save(list = c("SIRIUS_HOME", "SPEED_STORAGE", grep("^SCHEMA", ls(), value = TRUE)), file = paste("./schemas", SCHEMA_NAME, "schemaconfig.rdata", sep = "/"))

# saveRDS(SIRIUS_HOME, file = paste("./schemas", SCHEMA_NAME, "sirius_home.rds", sep = "/"))
