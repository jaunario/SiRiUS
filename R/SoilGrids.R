#' Get SoilGrids data for a point location
#'
#' This function fetches soil properties for a specific coordinate from the SoilGrids REST API.
#' It caches the results in a JSON file to avoid repeated API calls for the same location.
#'
#' @param x The longitude of the location.
#' @param y The latitude of the location.
#' @param property A character vector of soil properties to fetch.
#' @param depth A character vector of soil depths to fetch.
#' @param value A character string specifying the type of value to fetch (e.g., "mean").
#' @param path The path to the directory where the cached JSON files are stored.
#' @return A list containing a data frame of property information and a data frame of soil property values.
#' @export
soilgridOfXY <- function(x, y, property = c("bdod", "clay", "sand", "nitrogen", "soc", "phh2o"), depth = c("0-5", "5-15", "15-30", "30-60", "60-100"), value="mean", path="./data/soilgrids"){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  cachefile <- sprintf("%s/sgrd_x%09.6f_y%09.6f.json", path, x, y)

  if (!file.exists(cachefile)) {
    api_baseurl <- "https://rest.isric.org/soilgrids/v2.0/properties/query?"
    api_coords <- sprintf("lon=%0.4f&lat=%0.4f", x, y)
    api_properties <- paste("property=", property, sep = "", collapse = "&") 
    api_depths <- paste0("depth=", depth, "cm", sep = "", collapse = "&")
    api_value <- paste0("value=", value, sep = "", collapse = "&")
    api_requesturl <- paste(api_baseurl, api_coords, api_properties,api_depths, api_value, sep="&", collapse="&")
    response <- curl::curl_fetch_memory(api_requesturl)
    if(response$status_code == 200){
      json.soilgrids <- jsonlite::fromJSON(rawToChar(response$content))
      writeBin(response$content, con = cachefile)
    } else {
      json.soilgrids <- NA
    }
  } else {
    json.soilgrids <- jsonlite::fromJSON(rawToChar(readBin(cachefile, what = "raw", n=file.size(cachefile))))
  }
  # TODO: Find a better way to do below, i.e. no warning
  dat.soilgrids <- mapply("/", lapply(json.soilgrids$properties$layers$depths, "[", ,3), as.list(json.soilgrids$properties$layers$unit_measure$d_factor))
  names(dat.soilgrids) <- property
  dat.soilgrids <- data.frame(depth=paste(depth, "cm"), as.data.frame(dat.soilgrids))
  out.soilgrids <- list(data.frame(property, unit=json.soilgrids$properties$layers$unit_measure$target_units), dat.soilgrids)
  names(out.soilgrids)  <- c("property", value)
  return(out.soilgrids)
}

# json.soilgrids <- getSoilGrids(x=121.15, y=14.12)

#' Get SoilGrids data as a raster
#'
#' This function downloads a raster of a specified soil property for a given area of interest (AOI) from the SoilGrids WCS service.
#'
#' @param aoi An area of interest, which can be a numeric vector of two (x, y) or four (xmin, xmax, ymin, ymax) coordinates, or a SpatRaster, SpatVector, or SpatExtent object.
#' @param property The soil property to download. Default is "bdod".
#' @param depth The soil depth to download. Default is "0-5".
#' @param value The type of value to download (e.g., "mean"). Default is "mean".
#' @param buffer A buffer to add around the AOI if it is a point. Default is 1.
#' @param path The path to the directory where the downloaded raster will be saved.
#' @return The path to the downloaded GeoTIFF file.
#' @export
getSoilGridsRaster <- function(aoi, property = "bdod", depth = "0-5", value = "mean", buffer = 1, path = "./data/soilgrids"){
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
    xmn <- terra::xmin(aoi)
    xmx <- terra::xmax(aoi)
    ymn <- terra::ymin(aoi)
    ymx <- terra::ymax(aoi)
  } else {
    stop("Unsupported AOI input.")
  }

  outfile <- sprintf("%s/%s_%scm_x(%9.6f-%9.6f)_y(%9.6f-%9.6f).tif", path, property, depth, xmn, xmx, ymn, ymx)
  if (!dir.exists(dirname(outfile))) dir.create(dirname(outfile))
  if (!file.exists(outfile)) {
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)

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
    dl.file <- curl::curl_download(sgm.request, destfile = outfile)
    message(sgm.request)
  } else {
    dl.file <- outfile
  }

  return(dl.file)
}
