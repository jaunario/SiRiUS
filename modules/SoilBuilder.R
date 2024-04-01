

HYDRAULICS_EXE     = "./tools/soilhydrau.exe"

soilgridOfXY <- function(x, y, property = c("bdod", "clay", "sand", "nitrogen", "soc", "phh2o"), depth = c("0-5", "5-15", "15-30", "30-60", "60-100"), value="mean", path="./data/soilgrids"){
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)
  cachefile <- sprintf("%s/sgrd_x%09.6f_y%09.6f.json", path, x,y)
 
  if(!file.exists(cachefile)){
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
  dat.soilgrids <- mapply("/", lapply(json.soilgrids$properties$layers$depths, "[", ,3), as.list(json.soilgrids$properties$layers$unit_measure$d_factor))
  names(dat.soilgrids) <- property
  dat.soilgrids <- data.frame(depth=paste(depth, "cm"), as.data.frame(dat.soilgrids))
  out.soilgrids <- list(data.frame(property, unit=json.soilgrids$properties$layers$unit_measure$target_units), dat.soilgrids)
  names(out.soilgrids)  <- c("property", value)
  return(out.soilgrids)
}

json.soilgrids <- getSoilGrids(x=121.15, y=14.12)

