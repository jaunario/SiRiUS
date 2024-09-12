# Title: Plot ORYZA simulation output maps
# Description: This script creates maps based on the simulation outputs of a schema. It consolidates
# op.dat files into one data frame, and creates a raster object for each variable in SCHEMA_MAPVARS_OPDAT
# for each rerun. With tidyterra and maptiles, this script can also be used to layout the results into
# appealing, human-readable maps that can be attached to reports.

# Override SCHEMA_SELECTED if passed as command line argument
if (!interactive()) {
  cargs <- commandArgs(trailingOnly = TRUE)
  if (length(cargs) > 0) {
    SCHEMA_SELECTED <- cargs[1]
  }
} else {
  SCHEMA_SELECTED <- "CF_CanTho_250m_2017-2022_MVMedium_daily"
}

# library(terra)
# library(maptiles)
# library(ggplot2)
# library(tidyterra)

schema.dir <- paste0("./schemas/", SCHEMA_SELECTED)
load(paste0(schema.dir, "/config.RData"))
dat.schemaprog <- readRDS(paste0(schema.dir, "/progress_DF.rds"))

maps.dir <- paste0(schema.dir, "/maps")

if (grepl("gadm", SCHEMA_AOIFILE)) {
  lvl <- as.numeric(sub("gadm.", "", SCHEMA_AOIFILE))
  aoi.gis <- geodata::gadm(SCHEMA_COUNTRY_ISO, level = lvl, path = "./data/aoi")
  aoi.gis <- aoi.gis[data.frame(aoi.gis)[, paste0("VARNAME_", lvl)] == SCHEMA_AOIFILTER]
  vect.inset <- geodata::gadm(SCHEMA_COUNTRY_ISO, level = lvl, path = "./data/aoi")

} else if (grepl(".shp", SCHEMA_AOIFILE)) {
  aoi.gis <- vect(SCHEMA_AOIFILE)
} else {
  aoi.gis <- rast(paste0(schema.dir, "/baseraster.tif"))
}
dat.vars <- read.csv("./oryza/OryzaVariable_RefTable.csv")
rst.base <- rast(paste0(schema.dir, "/baseraster.tif"))
bg <- get_tiles(aoi.gis, provider = "CartoDB.Voyager")

#if (!dir.exists(maps.dir)) dir.create(maps.dir, recursive = TRUE)

#rst.base <- rast(paste0(schema.dir, "/baseraster.tif"))

files.opdat <- dir(path = paste0(schema.dir, "/out/seasonal"), pattern = "cell.*_OP.dat$", full.names = TRUE)
message(SCHEMA_SELECTED, ": Reading output files.", appendLF = TRUE)
bar.progress <- txtProgressBar(min = 1, max = length(files.opdat), initial = 0)
for (i in seq_along(files.opdat)) {
  dat.thiscell <- read.table(files.opdat[i], header = TRUE)
  dat.thiscell <- dat.thiscell[, c("RUNNUM", SCHEMA_MAPVARS_OPDAT)]
  dat.thiscell$cell <- as.numeric(sub("cell", "", sub("_OP.dat", "", basename(files.opdat[i]))))
  if (i == 1) {
    dat.op <- dat.thiscell
  } else {
    dat.op <- rbind(dat.op, dat.thiscell)
  }
  setTxtProgressBar(bar.progress, i)
}
close(bar.progress)

lst.maps <- list()
for (i in unique(dat.op$RUNNUM)) {
  dat.thisrun <- subset(dat.op, RUNNUM == i)
  for (j in seq_along(SCHEMA_MAPVARS_OPDAT)) {
    rst.thisfield <- rast(rst.base)
    rst.thisfield[dat.thisrun$cell] <- dat.thisrun[, SCHEMA_MAPVARS_OPDAT[j]]
    names(rst.thisfield) <- paste0(SCHEMA_RERUNPARAMS$simdates$EMYR[i], "-", SCHEMA_RERUNPARAMS$simdates$EMD[i], "_", SCHEMA_MAPVARS_OPDAT[j])
    if (j == 1) rst.thisrun <- rst.thisfield else rst.thisrun <- c(rst.thisrun, rst.thisfield)
    rm(rst.thisfield)
  }
  #names(rst.thisrun) <- SCHEMA_MAPVARS_OPDAT[-1]
  #rst.thisrun <- writeRaster(rst.thisrun, paste0(maps.dir, "/RUN-", sprintf("%02d", i), "_", names(rst.thisrun), ".tif"))
  if (interactive()) {
    plot(rst.thisrun)
    Sys.sleep(1)
  }

  lst.maps <- c(lst.maps, rst.thisrun)
  rm(rst.thisrun)
  tmpFiles(remove = TRUE, orphan = TRUE)
  gc(reset = TRUE)
}
#xx <- sapply(files.opdat, read.table, header = FALSE, skip = 1, nrows = 1)
#data.dates <- seq(manipulateR::dateFromDoy(SCHEMA_PLANTDOY[1], SCHEMA_STARTYEAR), as.Date(paste0("12-31-", SCHEMA_ENDYEAR), "%m-%d-%Y"), by = "day")
#ncfile <- paste0(maps.dir, "/", SCHEMA_SELECTED, ".nc")
#dimX <- ncdim_def("lon", units = "degrees", longname = "longitude", vals = c(xmin(rst.base), xmax(rst.base), xres(rst.base)))
#dimY <- ncdim_def("lat", units = "degrees", longname = "latitude", vals = c(ymin(rst.base), ymax(rst.base), yres(rst.base)))

if (SCHEMA_ANIMATE && require(magick)) {
  # Daily plots ----
  for (i in seq_along(SCHEMA_MAPVARS_RESDAT)){
    files.daily <- dir(paste0(maps.dir, "/daily/", SCHEMA_MAPVARS_RESDAT[i]), pattern = ".tif$", full.names = TRUE)
    #TODO: Standardize, probably best to remove scaling in converting (refer to ResdatToDF.R)
    scale_factor <- ifelse(SCHEMA_MAPVARS_RESDAT[i] %in% c("WRR14"), 1000000, 1000)
    varapp_palette <- ifelse(SCHEMA_MAPVARS_RESDAT[i] %in% c("CO2C", "CH4C", "N2ON"), "maori", "forest_cover")
    varapp_revpalette <- ifelse(SCHEMA_MAPVARS_RESDAT[i] %in% c("CO2C", "CH4C", "N2ON"), 1, 1)
    varapp_units <- ifelse(SCHEMA_MAPVARS_RESDAT[i] %in% c("CO2C", "CH4C", "N2ON"), "kg CO2e/ha", "tons/ha")
    varapp_limits <- switch(SCHEMA_MAPVARS_RESDAT[i], CO2C = list(limit = c(0, 100)), CH4C = list(limit = c(0, 4)), N2ON = list(limit = c(0, 0.4)), list(limit = c(0, 15)))
    varplot_dir <- paste0(schema.dir, "/plots/pngs/", SCHEMA_MAPVARS_RESDAT[i])
    if (!dir.exists(varplot_dir)) dir.create(varplot_dir, recursive = TRUE)
    # TODO: Complete reference table of ORYZA variables

    vartitle  <- dat.vars$Description[match(SCHEMA_MAPVARS_RESDAT[i], dat.vars$Variable)]
    for (j in seq_along(files.daily)){
      rst.day <- rast(files.daily[j])
      rst.day <- rst.day / scale_factor
      plt.vardate <- ggplot(aoi.gis) + theme_gray() +
        labs(title = vartitle,
             subtitle = format(as.Date(basename(files.daily[j]), paste0(SCHEMA_MAPVARS_RESDAT[i], "_%Y%j.tif")), "%d %b %Y")) +
        geom_spatraster_rgb(data = bg) +
        geom_spatraster(data = rst.day)

      if (SCHEMA_MAPVARS_RESDAT[i] %in% c("WRR14", "WAGT")) {
        plt.vardate <- plt.vardate + scale_fill_grass_c(palette = varapp_palette,
                                                        direction = varapp_revpalette,
                                                        limits = varapp_limits[[1]],
                                                        #breaks = quantile(rst.day[], na.rm = TRUE),
                                                        use_grass_range = FALSE,
                                                        guide = guide_colorbar(title = varapp_units, reverse = FALSE))
      } else if (SCHEMA_MAPVARS_RESDAT[i] %in% c("CO2C", "CH4C", "N2ON")) {
        plt.vardate <- plt.vardate + scale_fill_princess_c(palette = varapp_palette,
                                                           direction = varapp_revpalette,
                                                           limits = varapp_limits[[1]],
                                                           #use_grass_range = FALSE,
                                                           guide = guide_colorbar(title = varapp_units, reverse = FALSE))
      } else {

      }
      plt.vardate <- plt.vardate + geom_spatvector(fill = NA, color = "brown", linewidth = 0.5, linetype = "dashed") +
        theme(legend.position = "bottom",
              legend.title.position = "left",
              legend.text = element_text(vjust = -1),
              legend.key.spacing = unit(0, "cm"))

      ggsave(filename = sub(".tif", ".png", basename(files.daily[j])),
             width = 7, height = 5, units = "in",
             plt.vardate,
             path = paste0(schema.dir, "/plots/pngs/", SCHEMA_MAPVARS_RESDAT[i]))
    }
  }

  #TODO: inset(vect.inset, scale = 0.8, loc = "right", offset = 0.5)

  #dat.schemaprog$animate_status <- "waiting"
  #dat.schemaprog$animate_status <- factor(dat.schemaprog$animate_status, levels = c("waiting", "delegated", "pre-processed", "error"))
  plots.dir <- paste0(schema.dir, "/plots/pngs")
  animations.dir  <- paste0(schema.dir, "/plots/gif")
  if (!dir.exists(animations.dir)) dir.create(animations.dir, recursive = TRUE)

  vars.toanimate <- dir(plots.dir)

  for (i in seq_along(vars.toanimate)){
    #i=1
    if (file.exists(paste0(animations.dir, "/", SCHEMA_NAME, "_", vars.toanimate[i], "_1-200.gif"))) next
    files.png <- dir(paste0(plots.dir, "/", vars.toanimate[i]), full.names = TRUE)
    xx <- image_read(files.png[1:200])
    yy <- image_animate(xx, fps = 10, optimize = FALSE)
    image_write(yy, paste0(animations.dir, "/", SCHEMA_NAME, "_", vars.toanimate[i], "_1-200.gif"))
  }
  files.resdat <- dir(path = paste0(schema.dir, "/out/daily"), pattern = "cell.*_res.dat$", full.names = TRUE)
  message(SCHEMA_SELECTED, ": Reading output ", length(files.resdat), " files.", appendLF = TRUE)

  # Initiating Work session ----
  sys.totalram <- system2("wmic", args = "ComputerSystem get TotalPhysicalMemory", stdout = TRUE)
  sys.totalram <- as.numeric(trimws(sys.totalram[2])) / 1024^2
  #files.resdat <- files.resdat[-(1:2)]
}

# Merging files
dirs.tomerge <- dir(paste0("D:/", schema.dir, "/out"), full.names = TRUE)
for (i in seq_along(dirs.tomerge)){
  yr <- as.numeric(basename(dirs.tomerge[i]))
  for (j in 1:12){
    files.bycell <- dir(dirs.tomerge[i], pattern = sprintf("P%g_%02g_cell", yr, j), full.names = TRUE)
    if (length(files.bycell) == 0) next
    #if (file.exists(paste0(dirs.tomerge[i], "/", thisdoy, "/allcells.rds"))) next
    message(yr, "-", month.abb[j], ": Reading cell data files.")
    dat.yrmonth <- sapply(files.bycell, readRDS, simplify = FALSE, USE.NAMES = FALSE)
    message(yr, "-", month.abb[j], ": Merging to single data frame.")
    dat.yrmonth <- do.call(rbind, dat.yrmonth)
    vec.dates <- sort(unique(dat.yrmonth$date))
    message(yr, "-", month.abb[j], ": Mapping. ")
    for (k in seq_along(vec.dates)) {
      dat.thisdate <- subset(dat.yrmonth, date == vec.dates[k])
      for (l in seq_along(SCHEMA_MAPVARS_OPDAT)){
        out.filename <- paste0(schema.dir, "/maps/daily/", SCHEMA_MAPVARS_OPDAT[l], "/", SCHEMA_MAPVARS_OPDAT[l], format(vec.dates[k], "_%Y%j.tif"))
        if (!dir.exists(dirname(out.filename))) dir.create(dirname(out.filename), recursive = TRUE)
        rst.vardate <- rast(rst.base)
        rst.vardate[dat.thisdate$cell] <- dat.thisdate[, SCHEMA_MAPVARS_OPDAT[l]]
        rst.vardate <- writeRaster(rst.vardate, filename = out.filename, wopt = list(datatype = "INT4S", gdal = "COMPRESS=LZW"), overwrite = TRUE)
        rm(rst.vardate)
      }
    }
    message(yr, "-", month.abb[j], ": Done. ")
  }
}

files.thisdoy <- dir(paste0(dirs.tomerge[i], "/", thisdoy), pattern = ".rds$", full.names = TRUE)
info.files <- file.info(files.thisdoy)
dat.thisdoy <- sapply(files.thisdoy[info.files$size > 0], readRDS, simplify = FALSE, USE.NAMES = FALSE)

rownames(dat.thisdoy) <- NULL
saveRDS(dat.thisdoy, normalizePath(paste0(dirs.tomerge[i], "/", thisdoy, "/allcells.rds")))
file.remove(files.thisdoy)

dat.res <- readRDS(paste0(schema.dir, "/maps/allcells.rds"))
rownames(dat.res) <- NULL
dat.res[, -(1:2)] <- dat.res[, -(1:2)] / 1000
simdates <- unique(dat.res$date)
simdates <- sort(simdates)
simdates <- simdates[simdates >= as.Date("2012-01-01") & simdates < as.Date("2014-01-01")]
rr <- c(min(dat.res$CO2C), max(dat.res$CO2C))


for (i in seq_along(simdates)) {
  dd <- simdates[i]
  message(format(dd, "%Y-%b-%d"))
  bb <- subset(dat.res, date == dd)
  xx <- rast(rst.base)
  xx[bb$cell] <- bb$CO2C
  if (i == 1) rst.animate <- xx else rst.animate <- c(rst.animate, xx)
}
names(rst.animate) <- format(simdates, "%Y %b %d")
animate(rst.animate, range = rr, col = brewer.pal(9, "YlOrRd"))
plotRGB(bg)
plot(xx, range = rr, main = , col = brewer.pal(9, "YlOrRd"), add = TRUE, alpha = 0.7)
Sys.sleep(0.1)
dfs.tomerge <- dir(paste0("/mnt/hotdisk/", schema.dir, "/R"), full.names = TRUE, pattern = ".rds$", recursive = TRUE)
