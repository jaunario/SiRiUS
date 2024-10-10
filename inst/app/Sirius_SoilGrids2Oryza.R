
# USER SPECS
DOWNLOAD_DIR      <- paste0(SOILGRIDS_TIFDIR, "/", SCHEMA_BUILTINS_SUBSET)
OUTPUT_DIR        <- paste0(SOILGRIDS_ORYZADIR, "/", SCHEMA_BUILTINS_SUBSET)
XY_FILEID         <- FALSE

SOILGRID_VARIABLE  <- c("bdod", "clay", "sand", "nitrogen", "soc", "phh2o")
SOILGRID_DEPTHS    <- c("0-5", "5-15", "15-30", "30-60", "60-100")
SOILGRID_SUBHWSDBD <- TRUE

# HWSD2, https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v20/en/
# FAO & IIASA. 2023. Harmonized World Soil Database version 2.0. Rome and Laxenburg. https://doi.org/10.4060/cc3823en
HWSD2_BIL         <- "./data/HWSD2/HWSD2.bil"
HWSD2_REFTABLE    <- "./data/HWSD2/HWSD2 Reference Table.xlsx"

# RESAMPLING
# TARGET_RESOLUTION  <- 0.25
# OUT_COORDDIGITS    <- 2
# CELLID_FORFILENAME <- TRUE
# For native soilgrids (250m)
# OUT_COORDDIGITS = 7

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

target_ext <- ext(rst.aoi)

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
                                      path = DOWNLOAD_DIR),
                   silent = TRUE)
    if (class(this.dl) != "try-error") files.download <- c(files.download, this.dl)
  }
}

# Create inventory of soil (raster) files in SOILGRDI_DIR
files.soilayers <- dir(DOWNLOAD_DIR, pattern = ".tif$", full.names = TRUE)
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

#rst.aoi <- rasterize(vec.aoi, rst.soilvar, touches = TRUE)

rst.soilvar <- mask(rst.soilvar, rst.aoicells, maskvalue = NA)
#rst.soilvar <- crop(rst.soilvar, rst.aoi)
dat.soilvar <- values(rst.soilvar, dataframe = TRUE)

# Get pixels (rows) with valid values 
count.valid <- rowSums(!is.na(dat.soilvar))
dat.soilvar <- cbind(which(count.valid > 0), xyFromCell(rst.soilvar, which(count.valid > 0)), dat.soilvar[count.valid > 0, ])
colnames(dat.soilvar)[1:3] <- c("cell", "lon", "lat")

# It seems when bdod is complete, every thing else is complete so, instead of checking every soil property, bdod was used
#rst.soilvar <- rst.soilvar[count.valid > 0, ]

# Substituting Zero-BDOD data with data from HWSD2
idx.tosub <- which(rowSums(dat.soilvar[, grep("bdod", colnames(dat.soilvar))], na.rm = TRUE) == 0)

if (SOILGRID_SUBHWSDBD && file.exists(HWSD2_BIL)) {
  rst.hwsd <- rast(HWSD2_BIL)
  dat.hwsd <- readxl::read_xlsx(HWSD2_REFTABLE)

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
} else if (length(idx.tosub) > 0) {
  dat.soilvar <- dat.soilvar[-idx.tosub, ]
}
# Save data frame to be able to review the extracted values
# write.csv(dat.soilvar[which(dat.soilvar$site %in% xx), c(1, 3, 2)], "./SoilGrid2ORYZA/verifysites.csv", row.names = FALSE)

# TODO: Do this before conversion
# Conversion
dat.soilvar[, grep("bdod", colnames(dat.soilvar))] <- dat.soilvar[, grep("bdod", colnames(dat.soilvar))] / 100
dat.soilvar[, sapply(c("clay", "sand"), grep, colnames(dat.soilvar))] <- dat.soilvar[, sapply(c("clay", "sand"), grep, colnames(dat.soilvar))] / 1000
dat.soilvar[, grep("soc", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("soc", colnames(dat.soilvar), value = TRUE)], 1, "/", c(rep(300, 3), rep(100, 2))))
dat.soilvar[, grep("nitrogen", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("nitrogen", colnames(dat.soilvar))], 1, "/", c(7500, 7000, 6500, 3000, 3000)))
dat.soilvar[, grep("phh2o", colnames(dat.soilvar))] <- dat.soilvar[, grep("phh2o", colnames(dat.soilvar))] / 10

# Create duplicates of properties at depth 5-15cm and 15-30cm to have 8 depths for each property. Also used in soilhydrau.
# To accomodate TKL for depths 5-10, 10-15, 15-20, 20-25, 25-30
dat.soilvar[, paste0(grep("5-15cm", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[, grep("5-15cm", colnames(dat.soilvar))]
dat.soilvar[, paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[, grep("15-30cm$", colnames(dat.soilvar))]
dat.soilvar[, paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".3")] <- dat.soilvar[, grep("15-30cm$", colnames(dat.soilvar))]

# xx <- rast(rst.soilvar[[1]])
# xx[dat.soilvar$cell] <- dat.soilvar[, "bdod_0-5cm"]
# plot(xx)
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
files.soil <- lapply(apply(dat.soilvar[, ], 1, list), oryza.soil, oryza.dir = OUTPUT_DIR, filename_xy = XY_FILEID)
