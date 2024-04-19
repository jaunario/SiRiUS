
# USER SPECS
SOILGRID_DIR      = "./data/soilgrids/raster/Can Tho"
HYDRAULICS_EXE    = "./tools/soilhydrau.exe"
OUTPUT_DIR        = "./data/soil/Can Tho"
SOILGRID_VARIABLE = c("bdod", "clay", "sand", "nitrogen", "soc", "phh2o")
SOILGRID_DEPTHS   = c("0-5", "5-15", "15-30", "30-60", "60-100")

AOI_SHPFILE       = "./data/aoi/Can_Tho/CanTho.shp"

# CONSTANTS
TKL = c(rep(0.05,6), 0.3, 0.4)

library(terra)
library(curl)

memberID <- function(grp, nums){
  if(is.na(grp)){
    ids <- 1:sum(is.na(nums))
  } else {
    ids <- 1:sum(nums==grp) 
  }
  return(ids)
}

# TODO:  Remove unnecessary commands
calculateHydraulicParams <- function(num.soilparams, name, soilhydraulic_tool = HYDRAULICS_EXE){
  names(num.soilparams) <- name
  txt.hydraulic <- paste("8, 1, 0")
  dat.clay <- num.soilparams[grep("clay", names(num.soilparams))]
  dat.clay <- dat.clay[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.clay), value=TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.03g", round(dat.clay,3)), collapse = ","))
  dat.sand <- num.soilparams[grep("sand", names(num.soilparams))]
  dat.sand <- dat.sand[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.sand), value=TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.03g", round(dat.sand,3)), collapse = ","))
  dat.soc <- num.soilparams[grep("soc", names(num.soilparams))]
  dat.soc <- dat.soc[unlist(sapply(SOILGRID_DEPTHS, grep, names(dat.soc), value=TRUE))]
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.09f", round(dat.soc/0.58/100,9)), collapse = ","))
  
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.04f", c(rep(1,4), rep(0, 4))), collapse= ","))
  txt.hydraulic <- c(txt.hydraulic, paste(sprintf("%0.04f", 1- c(seq(0.04, 0.01, by= -0.01), -0.1, rep(0, 3))), collapse= ","))
  
  writeLines(txt.hydraulic, con="myhydraulic.txt")
  
  system(paste(HYDRAULICS_EXE, "myhydraulic.txt"))
  dat.hydra <- read.table("HYDR_PARAM.TXT", header = TRUE)
  dat.hyout <- data.frame (cbind(t(dat.hydra$WCST), t(dat.hydra$WCFC), t(dat.hydra$WCWP), t(dat.hydra$WCAD), t(dat.hydra$KST)))
  colnames(dat.hyout) <- sapply(c("WCST.", "WCFC.", "WCWP.", "WCAD.", "KST."), paste0, 1:8)
  return(dat.hyout)
}

numvectorToString <- function(num, sigdigits){
  return(paste(sprintf(paste0("%0.0", sigdigits,"f"), round(num, sigdigits)), collapse=","))
}

getSoilGridsRaster <- function(aoi, property = "bdod", depth="0-5", value="mean", buffer=1, path = "./data/soilgrids"){
  message(property, "-", depth)
  if(is.numeric(aoi) & length(aoi)==2){
    xmn <- aoi[1] - buffer
    xmx <- aoi[1] + buffer
    ymn <- aoi[2] - buffer
    ymx <- aoi[2] + buffer
  } else if(is.numeric(aoi) & length(aoi)==4){
    xmn <- aoi[1]
    xmx <- aoi[2]
    ymn <- aoi[3]
    ymx <- aoi[4]
  } else if(isS4(aoi) & class(aoi) %in% c("SpatRaster", "SpatVector")){
    xmn <- xmin(aoi)
    xmx <- xmax(aoi)
    ymn <- ymin(aoi)
    ymx <- ymax(aoi)
  } else {
    stop("Unsupported AOI input.")
  }
  
  outfile <- sprintf("%s/%s_%scm_x(%9.6f-%9.6f)_y(%9.6f-%9.6f).tif", path, property, depth, xmn, xmx, ymn, ymx)
  
  if(!file.exists(outfile)){
    if(!dir.exists(path)) dir.create(path, recursive = TRUE)
    
    OPENGIS_EPSG4326 <- "http://www.opengis.net/def/crs/EPSG/0/4326"
    
    sgm.01mainurl <- sprintf("https://maps.isric.org/mapserv?map=/map/%s.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage",property) 
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

write.OryzaSoil <- function(soilparams, paramnames = NULL, TKL = c(rep(0.05,6), 0.3, 0.4), path = tempdir(), label = "soilgrids", overwrite=FALSE){
  if(is.list(soilparams)) soilparams <- unlist(soilparams)
  
  outfile <- paste0(path, "/", sprintf( "%s_x%0.7f_y%0.7f", label, soilparams["lon"], soilparams["lat"]), ".sol")
  
  if(!file.exists(outfile) | overwrite){
    
    if(!is.null(paramnames)) {
      names(soilparams) <- paramnames
    } else paramnames <- names(soilparams)
    #if(file.exists(outfile)) next
    txt.soilfile <- c(
      "SCODE = 'PADDY'",
      "WL0MX = 100.0",
      "NL = 8",
      paste0("TKL = ", paste(sprintf("%0.03f", TKL), collapse=",")),
      "ZRTMS = 1.0",
      "SWITPD = 1",
      "NLPUD = 5",
      paste0("WCSTRP = ", numvectorToString(soilparams[grep("WCST", paramnames)],5)),
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
      paste0("FIXPERC = ", numvectorToString(min(soilparams[grep("KST", paramnames)]),7)),
      "PTABLE = 1.0, 1.0, 366.0, 1.0",
      "SWITKH  = 0",
      "SWITPF  = 0",
      paste0("CLAYX = ", numvectorToString(soilparams[grep("clay", paramnames, value=TRUE)],4)),
      paste0("SANDX = ", numvectorToString(soilparams[grep("sand", paramnames, value=TRUE)],4)),
      paste0("BD = ", numvectorToString(soilparams[grep("bdod", paramnames, value=TRUE)],4)),
      paste0("SOC = ", numvectorToString(soilparams[grep("soc", paramnames, value=TRUE)],3)),
      paste0("SON = ", numvectorToString(soilparams[grep("nitrogen", paramnames, value=TRUE)],3)),
      paste0("KST = ", numvectorToString(soilparams[grep("KST", paramnames, value=TRUE)],5)),
      paste0("WCST = ", numvectorToString(soilparams[grep("WCST", paramnames, value=TRUE)],5)),
      paste0("WCFC = ", numvectorToString(soilparams[grep("WCFC", paramnames, value=TRUE)],5)),
      paste0("WCWP = ", numvectorToString(soilparams[grep("WCWP", paramnames, value=TRUE)],5)),
      paste0("WCAD = ", numvectorToString(soilparams[grep("WCAD", paramnames, value=TRUE)],5)),  
      "WL0I = 10.0",
      paste0("WCLI = ", numvectorToString(soilparams[grep("WCST", paramnames, value=TRUE)] - 0.02,5)),
      "RIWCLI = 'YES'",
      "SATAV = 18.0",
      paste0("SOILT = ", numvectorToString(c(22,21,20,19,18,17,16,16),5)),
      paste0("WCLINT = ", paste(as.numeric(sapply(1:8, rep, 3)), collapse = ","))
    )
    
    writeLines(txt.soilfile, con = outfile)
  }
  return(outfile)
}


vec.aoi <- vect(AOI_SHPFILE)

files.download <- vector()
for(i in seq_along(SOILGRID_VARIABLE)){
  for(j in seq_along(SOILGRID_DEPTHS)){
    this.dl <- try(getSoilGridsRaster(aoi=vec.aoi, property = SOILGRID_VARIABLE[i], depth = SOILGRID_DEPTHS[j], path = SOILGRID_DIR), silent = TRUE)
    if(class(this.dl)!="try-error"){
      files.download <- c(files.download, this.dl)
    }
  }
}

# Create inventory of soil (raster) files in SOILGRDI_DIR
files.soilayers <- dir(SOILGRID_DIR, pattern=".tif$", full.names = TRUE)
inv.soilgrids <- sapply(sub("\\.tif", "", basename(files.soilayers)), strsplit, split = "_")
inv.soilgrids <- do.call(rbind, inv.soilgrids)
colnames(inv.soilgrids) <- c("variable", "depth", "x", "y")
inv.soilgrids <- data.frame(inv.soilgrids, filename = files.soilayers)
inv.soilgrids <- inv.soilgrids[inv.soilgrids$variable %in% SOILGRID_VARIABLE,]
inv.soilgrids$depth <- factor(inv.soilgrids$depth, levels = paste0(SOILGRID_DEPTHS,"cm"))
inv.soilgrids <- inv.soilgrids[with(inv.soilgrids, order(variable, depth)),]

# Read raster files
rst.soilvar <- rast(inv.soilgrids$filename)
names(rst.soilvar) <- paste0(inv.soilgrids$variable, "_", inv.soilgrids$depth)

rst.aoi <- rasterize(vec.aoi, rst.soilvar, touches = TRUE)

rst.soilvar <- mask(rst.soilvar, rst.aoi, maskvalue = NA)
rst.soilvar <- crop(rst.soilvar, rst.aoi)
dat.soilvar <- values(rst.soilvar)

# Filter valid (with complete bdod data) pixels 
# It seems when bdod is complete, every thing else is complete so, instead of checking every soil property, bdod was used
idx.withvalues <- which(rowSums(dat.soilvar[,paste0("bdod_",SOILGRID_DEPTHS, "cm")] > 0) == 5)
dat.soilvar <- data.frame(cell = 1:ncell(rst.soilvar), xyFromCell(rst.aoi,1:ncell(rst.soilvar)), dat.soilvar)
dat.soilvar <- dat.soilvar[idx.withvalues,]
colnames(dat.soilvar) <- c("site", "lon", "lat", paste0(inv.soilgrids$variable, "_", inv.soilgrids$depth))


# Save data frame to be able to review the extracted values
# write.csv(dat.soilvar[which(dat.soilvar$site %in% xx), c(1, 3, 2)], "./SoilGrid2ORYZA/verifysites.csv", row.names = FALSE)

# Conversion
dat.soilvar[,grep("bdod", colnames(dat.soilvar))] <- dat.soilvar[,grep("bdod", colnames(dat.soilvar))]/100
dat.soilvar[,sapply(c("clay","sand"), grep, colnames(dat.soilvar))] <- dat.soilvar[,sapply(c("clay","sand"), grep, colnames(dat.soilvar))]/1000
dat.soilvar[,grep("soc", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("soc", colnames(dat.soilvar), value=TRUE)], 1, "/", c(rep(300,3), rep(100,2))))
dat.soilvar[,grep("nitrogen", colnames(dat.soilvar))] <- t(apply(dat.soilvar[, grep("nitrogen", colnames(dat.soilvar))], 1, "/", c(7500, 7000, 6500, 3000, 3000)))
dat.soilvar[,grep("phh2o", colnames(dat.soilvar))] <- dat.soilvar[, grep("phh2o", colnames(dat.soilvar))]/10

# Create duplicates of properties at depth 5-15cm and 15-30cm to have 8 depths for each property. Also used in soilhydrau.
dat.soilvar[,paste0(grep("5-15cm", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[,grep("5-15cm", colnames(dat.soilvar))]
dat.soilvar[,paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".2")] <- dat.soilvar[,grep("15-30cm$", colnames(dat.soilvar))]
dat.soilvar[,paste0(grep("15-30cm$", colnames(dat.soilvar), value = TRUE), ".3")] <- dat.soilvar[,grep("15-30cm$", colnames(dat.soilvar))]

# Rearrange fields to properly group by property and sorted according to depth
cols <- unlist(sapply(SOILGRID_DEPTHS, grep, colnames(dat.soilvar), value = TRUE, USE.NAMES = FALSE))
cols <- unlist(sapply(SOILGRID_VARIABLE, grep, cols, value=TRUE, USE.NAMES = FALSE))
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


# Create the oryza soil files
files.soil <- lapply(apply(dat.soilvar, 1, as.list, recurisve=FALSE), write.OryzaSoil, path = OUTPUT_DIR)

