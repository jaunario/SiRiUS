# USER SPECS
SOILGRID_DIR      <- "./data/soilgrids/raster/VNM/Can Tho"
HYDRAULICS_EXE    <- "./tools/soilhydrau.exe"
OUTPUT_DIR        <- "./oryza/soil/VNM/Can Tho2"
XY_FILEID         <- FALSE

SOILGRID_VARIABLE  <- c("bdod", "clay", "sand", "nitrogen", "soc", "phh2o")
SOILGRID_DEPTHS    <- c("0-5", "5-15", "15-30", "30-60", "60-100")
SOILGRID_SUBHWSDBD <- TRUE

# HWSD2, https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/
# FAO & IIASA. 2023. Harmonized World Soil Database version 2.0. Rome and Laxenburg. https://doi.org/10.4060/cc3823en
HWSD2_BIL         <- "./data/HWSD2/HWSD2.bil"
HWSD2_REFTABLE    <- "./data/HWSD2/HWSD2 Reference Table.xlsx"

AOI_SHPFILE       <- "gadm.1"
#AOI_SHPFILE      <- "./data/aoi/Can_Tho/CanTho.shp"
AOI_BUFFER        <- 1000

# GADM Support
AOI_ISO3          <- "KHM"
AOI_GADMLVL       <- 1
AOI_SELECT        <- c("Takeo")

#AOI_SHPFILE       = "gadm.1" # "./data/aoi/MRD/MRD_Provinces_WGS.shp"
AOI_BUFFER        <- 1000

# CONSTANTS
TKL <- c(rep(0.05, 6), 0.3, 0.4)

# RESAMPLING
TARGET_RESOLUTION  <- 0.25
OUT_COORDDIGITS    <- 2
CELLID_FORFILENAME <- TRUE
# For native soilgrids (250m)
#OUT_COORDDIGITS = 7

#library(terra)
#library(curl)
#library(geodata)
#library(readxl)

memberID <- function(grp, nums) {
  if (is.na(grp)) {
    ids <- 1:sum(is.na(nums))
  } else {
    ids <- 1:sum(nums == grp)
  }
  return(ids)
}

# TODO:  Remove unnecessary commands
calculateHydraulicParams <- function(num.soilparams, name, soilhydraulic_tool = HYDRAULICS_EXE) {
  names(num.soilparams) <- name
  txt.hydraulic <- paste("8, 1, 0")
  dat.clay <- num.soilparams[grep("clay", names(num.soilparams))]
  dat.clay <- dat.clay[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.clay), value = TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.03g", round(dat.clay, 3)), collapse = ","))
  dat.sand <- num.soilparams[grep("sand", names(num.soilparams))]
  dat.sand <- dat.sand[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.sand), value = TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.03g", round(dat.sand, 3)), collapse = ","))
  dat.soc <- num.soilparams[grep("soc", names(num.soilparams))]
  dat.soc <- dat.soc[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.soc), value = TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.09f", round(dat.soc / 0.58 / 100, 9)), collapse = ","))

  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.04f", c(rep(1, 4), rep(0, 4))), collapse = ","))
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.04f", 1 - c(seq(0.04, 0.01, by = -0.01), -0.1, rep(0, 3))), collapse = ","))

  writeLines(txt.hydraulic, con = "myhydraulic.txt")

  system(paste(HYDRAULICS_EXE, "myhydraulic.txt"))
  dat.hydra <- read.table("HYDR_PARAM.TXT", header = TRUE)
  dat.hyout <- data.frame(cbind(t(dat.hydra$WCST), t(dat.hydra$WCFC), t(dat.hydra$WCWP), t(dat.hydra$WCAD), t(dat.hydra$KST)))
  colnames(dat.hyout) <- sapply(c("WCST.", "WCFC.", "WCWP.", "WCAD.", "KST."), paste0, 1:8)
  return(dat.hyout)
}

numvectorToString <- function(num, sigdigits) {
  return(paste(sprintf(paste0("%0.0", sigdigits, "f"), round(num, sigdigits)), collapse = ","))
}

getSoilGridsRaster <- function(aoi, property = "bdod", depth = "0-5", value = "mean", buffer = 1, path = "./data/soilgrids") {
  message(property, "-", depth)
  if (is.numeric(aoi) && length(aoi) == 2) {
    xmn <- aoi[1] - buffer
    xmx <- aoi[1] + buffer
    ymn <- aoi[2] - buffer
    ymx <- aoi[2] + buffer
  } else if (is.numeric(aoi) && length(aoi) == 4) {
    xmn <- aoi[1]
    xmx <- aoi[2]
    ymn <- aoi[3]
    ymx <- aoi[4]
  } else if (isS4(aoi) && class(aoi) %in% c("SpatRaster", "SpatVector", "SpatExtent")) {
    xmn <- xmin(aoi)
    xmx <- xmax(aoi)
    ymn <- ymin(aoi)
    ymx <- ymax(aoi)
  } else {
    stop("Unsupported AOI input.")
  }

  outfile <- sprintf("%s/%s_%scm_x(%9.6f-%9.6f)_y(%9.6f-%9.6f).tif", path, property, depth, xmn, xmx, ymn, ymx)

  if (!file.exists(outfile)) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

    OPENGIS_EPSG4326 <- "http://www.opengis.net/def/crs/EPSG/0/4326"

    sgm.01mainurl <- sprintf("https://maps.isric.org/mapserv?map=/map/%s.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage", property)
    sgm.02coverage <- sprintf("COVERAGEID=%s_%scm_%s", property, depth, value)
    sgm.03fileformat <- "FORMAT=GEOTIFF_INT16"
    sgm.04x <- sprintf("SUBSET=X(%f,%f)", xmn, xmx)
    sgm.05y <- sprintf("SUBSET=Y(%f,%f)", ymn, ymx)
    sgm.06sscrs <- sprintf("SUBSETTINGCRS=%s", OPENGIS_EPSG4326)
    sgm.07ocrs <- sprintf("OUTPUTCRS=%s", OPENGIS_EPSG4326)

    sgm.request <- paste(sgm.01mainurl,
                         sgm.02coverage,
                         sgm.03fileformat,
                         sgm.04x,
                         sgm.05y,
                         sgm.06sscrs,
                         sgm.07ocrs, sep = "&")
    dl.file <- curl_download(sgm.request, destfile = outfile)
    message(sgm.request)
  } else {
    dl.file <- outfile
  }

  return(dl.file)
}

writeOryzaSoil <- function(soilparams, paramnames = NULL, TKL = c(rep(0.05, 6), 0.3, 0.4), path = tempdir(), xyprecision = 7, filename_prefix = "soilgrids", filename_xy = TRUE, overwrite = FALSE) {
  if (is.list(soilparams)) soilparams <- unlist(soilparams)

  if (filename_xy) {
    outfile <- paste0(path, "/", sprintf(paste0("%s_x%0.", xyprecision, "f_y%0.", xyprecision, "f"), filename_prefix, soilparams["lon"], soilparams["lat"]), ".sol")
  } else {
    outfile <- paste0(path, "/", sprintf(paste0("%s_cell%0", max(sapply(soilparams["cell"], nchar)), "g"), filename_prefix, soilparams["cell"]), ".sol")
  }

  if (!file.exists(outfile) || overwrite) {

    if (!is.null(paramnames)) {
      names(soilparams) <- paramnames
    } else {
      paramnames <- names(soilparams)
    }
    #if(file.exists(outfile)) next
    txt.soilfile <- c(
      "SCODE = 'PADDY'",
      "WL0MX = 100.0",
      "NL = 8",
      paste0("TKL = ", paste(sprintf("%0.03f", TKL), collapse = ",")),
      "ZRTMS = 1.0",
      "SWITPD = 1",
      "NLPUD = 5",
      paste0("WCSTRP = ", numvectorToString(soilparams[grep("WCST", paramnames)], 5)),
      "PFCR = 6.0",
      "DPLOWPAN = 0.2",
      "SWITGW = 1",
      "ZWTB = 1.0, 150., 366.0, 150.0",
      "ZWTBI = 100.0",
      "MINGW = 100.0",
      "MAXGW = 100.0",
      "ZWA   = 1.0",
      "ZWB   = 0.5",
      "SWITVP = -1",
      paste0("FIXPERC = ", numvectorToString(min(soilparams[grep("KST", paramnames)]), 7)),
      "PTABLE = 1.0, 1.0, 366.0, 1.0",
      "SWITKH  = 0",
      "SWITPF  = 0",
      paste0("CLAYX = ", numvectorToString(soilparams[grep("clay", paramnames, value = TRUE)], 4)),
      paste0("SANDX = ", numvectorToString(soilparams[grep("sand", paramnames, value = TRUE)], 4)),
      paste0("BD = ", numvectorToString(soilparams[grep("bdod", paramnames, value = TRUE)], 4)),
      paste0("SOC = ", numvectorToString(soilparams[grep("soc", paramnames, value = TRUE)], 3)),
      paste0("SON = ", numvectorToString(soilparams[grep("nitrogen", paramnames, value = TRUE)], 3)),
      paste0("KST = ", numvectorToString(soilparams[grep("KST", paramnames, value = TRUE)], 5)),
      paste0("WCST = ", numvectorToString(soilparams[grep("WCST", paramnames, value = TRUE)], 5)),
      paste0("WCFC = ", numvectorToString(soilparams[grep("WCFC", paramnames, value = TRUE)], 5)),
      paste0("WCWP = ", numvectorToString(soilparams[grep("WCWP", paramnames, value = TRUE)], 5)),
      paste0("WCAD = ", numvectorToString(soilparams[grep("WCAD", paramnames, value = TRUE)], 5)),
      "WL0I = 10.0",
      paste0("WCLI = ", numvectorToString(soilparams[grep("WCST", paramnames, value = TRUE)] - 0.02, 5)),
      "RIWCLI = 'YES'",
      "SATAV = 18.0",
      paste0("SOILT = ", numvectorToString(c(22, 21, 20, 19, 18, 17, 16, 16), 5)),
      paste0("WCLINT = ", paste(as.numeric(sapply(1:8, rep, 3)), collapse = ","))
    )

    writeLines(txt.soilfile, con = outfile)
  }
  return(outfile)
}

if (exists("AOI_SHPFILE") && (!is.null(AOI_SHPFILE) || grepl("gadm", AOI_SHPFILE))) {
  vec.aoi <- vect(AOI_SHPFILE)
} else {
  vec.aoi <- gadm(country = AOI_ISO3, level = AOI_GADMLVL, path = paste0("./data/aoi/", AOI_ISO3))
  idx.select <- mapply(grepl, as.list(AOI_SELECT), vec.aoi[[paste0(c("NAME_", "VARNAME_"), AOI_GADMLVL)]], SIMPLIFY = FALSE)
  idx.select <- do.call("|", idx.select)
  vec.aoi <- vec.aoi[idx.select]
}

if (AOI_BUFFER > 0) {
  vec.aoi <- terra::buffer(vec.aoi, AOI_BUFFER)
}
target_ext <- ext(vec.aoi)

if (exists("TARGET_RESOLUTION")) {
  rst.base <- rast()
  res(rst.base) <- TARGET_RESOLUTION
  rst.base[] <- 1:ncell(rst.base)
  rst.base <- crop(rst.base, vec.aoi, snap = "out")
  rst.base <- mask(rst.base, rasterize(vec.aoi, rst.base, touches = TRUE))
  target_ext <- ext(rst.base)
}

files.download <- vector()
for (i in seq_along(SOILGRID_VARIABLE)) {
  for (j in seq_along(SOILGRID_DEPTHS)) {
    this.dl <- try(getSoilGridsRaster(aoi = target_ext,
                                      property = SOILGRID_VARIABLE[i],
                                      depth = SOILGRID_DEPTHS[j],
                                      path = SOILGRID_DIR),
                   silent = TRUE)
    if (class(this.dl) != "try-error") files.download <- c(files.download, this.dl)
  }
}

# Create inventory of soil (raster) files in SOILGRDI_DIR
files.soilayers <- dir(SOILGRID_DIR, pattern = ".tif$", full.names = TRUE)
inv.soilgrids <- sapply(sub("\\.tif", "", basename(files.soilayers)), strsplit, split = "_")
inv.soilgrids <- do.call(rbind, inv.soilgrids)
colnames(inv.soilgrids) <- c("variable", "depth", "x", "y")
inv.soilgrids <- data.frame(inv.soilgrids, filename = files.soilayers)
inv.soilgrids <- inv.soilgrids[inv.soilgrids$variable %in% SOILGRID_VARIABLE, ]
inv.soilgrids$depth <- factor(inv.soilgrids$depth, levels = paste0(SOILGRID_DEPTHS, "cm"))
inv.soilgrids <- inv.soilgrids[with(inv.soilgrids, order(variable, depth)), ]

# Read raster files
rst.soilvar <- rast(inv.soilgrids$filename)
names(rst.soilvar) <- paste0(inv.soilgrids$variable, "_", inv.soilgrids$depth)
#rst.bdod <- rst.soilvar[[paste0("bdod_", SOILGRID_DEPTHS, "cm")]]
#rst.clay <- rst.soilvar[[paste0("clay_", SOILGRID_DEPTHS, "cm")]]
#dat.bdod <- values(sum(rst.bdod == 0))
#rst.soilvar <- mask(rst.soilvar, rst.soilmask, maskvalue = 5)
# Resample if TARGET_RES != native resolution
if (exists("TARGET_RESOLUTION") && xres(rst.soilvar) != TARGET_RESOLUTION) {
  rst.soilvar <- resample(rst.soilvar, rst.base, method = "average")
  rst.soilvar <- crop(rst.soilvar, rst.base)
}

rst.aoi <- rasterize(vec.aoi, rst.soilvar, touches = TRUE)

rst.soilvar <- mask(rst.soilvar, rst.aoi, maskvalue = NA)
#rst.soilvar <- crop(rst.soilvar, rst.aoi)
dat.soilvar <- values(rst.soilvar, dataframe = TRUE)

# Filter valid (with complete bdod data) pixels
# It seems when bdod is complete, every thing else is complete so, instead of checking every soil property, bdod was used
count.valid <- rowSums(!is.na(dat.soilvar) & (dat.soilvar > 0), na.rm = TRUE)
#rst.soilvar <- rst.soilvar[count.valid > 0, ]
dat.soilvar <- cbind(which(count.valid > 0), xyFromCell(rst.soilvar, which(count.valid > 0)), dat.soilvar[count.valid > 0, ])
colnames(dat.soilvar)[1:3] <- c("cell", "lon", "lat")

# Substituting Zero-BDOD data with data from HWSD2
if (SOILGRID_SUBHWSDBD && file.exists(HWSD2_BIL)) {
  rst.hwsd <- rast(HWSD2_BIL)
  rst.hwsd <- rast(HWSD2_BIL)
  dat.hwsd <- read_xlsx(HWSD2_REFTABLE)

  idx.tosub <- which(rowSums(dat.soilvar[, grep("bdod", colnames(dat.soilvar))], na.rm = TRUE) == 0)
  dat.tosub <- data.frame(dat.soilvar[idx.tosub, c("cell", "lon", "lat")])
  dat.tosub$hwsd <- unlist(rst.hwsd[cellFromXY(rst.hwsd, data.frame(x = dat.tosub$lon, y = dat.tosub$lat))])

  smu_id <- unique(dat.tosub$hwsd)
  for (i in seq_along(smu_id)){
    dat.thissmu <- subset(dat.hwsd, HWSD2_SMU_ID == smu_id[i] & !LAYER %in% c("D6", "D7"), select = c("LAYER", "SHARE", "TOPDEP", "BOTDEP", "BULK"))
    dat.thissmu$WEIGHTED_BULK <- dat.thissmu$BULK * dat.thissmu$SHARE / 100
    dat.thissmu <- aggregate(WEIGHTED_BULK ~ LAYER + TOPDEP + BOTDEP, data = dat.thissmu, FUN = sum)
    dat.t <- t(c(smu_id[i], dat.thissmu$WEIGHTED_BULK))
    colnames(dat.t) <- c("smu_id", paste0("d", dat.thissmu$BOTDEP))
    if (i == 1) dat.hwsdsub <- dat.t else dat.hwsdsub <- rbind(dat.hwsdsub, dat.t)
  }
}
# Save data frame to be able to review the extracted values
# write.csv(dat.soilvar[which(dat.soilvar$site %in% xx), c(1, 3, 2)], "./SoilGrid2ORYZA/verifysites.csv", row.names = FALSE)

# Conversion
dat.soilvar[, grep("bdod", colnames(dat.soilvar))] <- dat.soilvar[, grep("bdod", colnames(dat.soilvar))] / 100
dat.soilvar[, sapply(c("clay", "sand"), grep, colnames(dat.soilvar))] <- dat.soilvar[, sapply(c("clay", "sand"), grep, colnames(dat.soilvar))] / 1000
dat.soilvar[, grep("soc", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("soc", colnames(dat.soilvar), value = TRUE)], 1, "/", c(rep(300, 3), rep(100, 2))))
dat.soilvar[, grep("nitrogen", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("nitrogen", colnames(dat.soilvar))], 1, "/", c(7500, 7000, 6500, 3000, 3000)))
dat.soilvar[, grep("phh2o", colnames(dat.soilvar))] <- dat.soilvar[, grep("phh2o", colnames(dat.soilvar))] / 10

# Create duplicates of properties at depth 5-15cm and 15-30cm to have 8 depths for each property. Also used in soilhydrau.
dat.soilvar[, paste0(grep("5-15cm", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[, grep("5-15cm", colnames(dat.soilvar))]
dat.soilvar[, paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[, grep("15-30cm$", colnames(dat.soilvar))]
dat.soilvar[, paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".3")] <- dat.soilvar[, grep("15-30cm$", colnames(dat.soilvar))]

xx <- rast(rst.soilvar[[1]])
xx[dat.soilvar$cell] <- dat.soilvar[, "bdod_0-5cm"]
plot(xx)
# Substitute HWSD2 BDOD data
for (i in seq_along(smu_id)) {
  recs.tosub <- match(dat.tosub$cell[dat.tosub$hwsd == smu_id[i]], dat.soilvar[, "cell"])

  # Check if values are non-negative
  if (sum(dat.hwsdsub[dat.hwsdsub[, 1] == smu_id[i], -1] < 0) > 0) {
    # Remove pixels with no valid HWSD substitute
    dat.soilvar <- dat.soilvar[-recs.tosub, ]
    next
  }

  dat.soilvar[recs.tosub, c("bdod_0-5cm", "bdod_5-15cm", "bdod_5-15cm.2", "bdod_15-30cm")] <- dat.hwsdsub[dat.hwsdsub[, 1] == smu_id[i], "d20"]
  dat.soilvar[recs.tosub, c("bdod_15-30cm.2", "bdod_15-30cm.3")] <- dat.hwsdsub[dat.hwsdsub[, 1] == smu_id[i], "d40"]
  dat.soilvar[recs.tosub, "bdod_30-60cm"]  <- dat.hwsdsub[dat.hwsdsub[, 1] == smu_id[i], "d60"]
  dat.soilvar[recs.tosub, "bdod_60-100cm"] <- mean(dat.hwsdsub[dat.hwsdsub[, 1] == smu_id[i], c("d80", "d100")])
}

# Rearrange fields to properly group by property and sorted according to depth
cols <- unlist(sapply(SOILGRID_DEPTHS, grep, colnames(dat.soilvar), value = TRUE, USE.NAMES = FALSE))
cols <- unlist(sapply(SOILGRID_VARIABLE, grep, cols, value = TRUE, USE.NAMES = FALSE))
dat.soilvar <- dat.soilvar[, c(colnames(dat.soilvar)[!colnames(dat.soilvar) %in% cols], cols)]

# Calculate Soil Hydraulogical parameters using tool from Tao Li
dat.hydparams <- lapply(as.data.frame(t(dat.soilvar)), calculateHydraulicParams, name = colnames(dat.soilvar))
dat.hydparams <- do.call(rbind, dat.hydparams)

# Adjustments/Conversion of SOC and Nitrogen
dat.soc <- dat.soilvar[, grep("soc", colnames(dat.soilvar), value = TRUE)] * dat.soilvar[, grep("bdod", colnames(dat.soilvar), value = TRUE)]
dat.soilvar[, grep("soc", colnames(dat.soilvar), value = TRUE)] <- t(apply(dat.soc, 1, "*", TKL)) * 100000
dat.son <- dat.soilvar[, grep("nitrogen", colnames(dat.soilvar), value = TRUE)] * dat.soilvar[, grep("bdod", colnames(dat.soilvar), value = TRUE)]
dat.soilvar[, grep("nitrogen", colnames(dat.soilvar), value = TRUE)] <- t(apply(dat.son, 1, "*", TKL)) * 100000

# Combine soil properties and hydraulic parameters
dat.soilvar <- cbind(dat.soilvar, dat.hydparams)

# Rounding-off
idx.3dig <- sapply(c("soc", "nitrogen"), grep, colnames(dat.soilvar))
idx.4dig <- sapply(c("clay", "sand", "bdod"), grep, colnames(dat.soilvar))
idx.5dig <- sapply(c("KST", "WCST", "WCFC", "WCWP", "WCAD"), grep, colnames(dat.soilvar))

dat.oryzasoil <- c(lapply(dat.soilvar[, idx.3dig], round, 3), lapply(dat.soilvar[, idx.4dig], round, 4), lapply(dat.soilvar[, idx.5dig], round, 5))

# Create the oryza soil files
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
files.soil <- lapply(apply(dat.soilvar, 1, as.list, recurisve = FALSE), writeOryzaSoil, path = OUTPUT_DIR, filename_xy = FALSE)
