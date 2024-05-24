# RERUN_BASIC = c("IYEAR", "STTIME", "EMD", "CO2A") 
# 
writeRerun <- function (..., schema_name, filename){
  params <- as.list(...) # mapply(paste, as.list(names(...)), ..., sep="=", SIMPLIFY = FALSE)
  
  if (sum(names(params)!="simdates") == 0) {
    dat.params <- params$simdates
  } else {
    dat.simdates <- params$simdates
    params <- c(params[-which(names(params) == "simdates")], list(simdates = 1:nrow(params$simdates)))
    dat.params <- expand.grid(params, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    dat.params <- cbind(dat.params[-which(colnames(dat.params)=="simdates")], dat.simdates[dat.params$simdates,])
  }
  
  # items in a data.frame means each row come together and should not be mapped out individually 
  # to other elements through expand.grid 
  # param.class <- sapply(params, class)
  # params.grouped <- params[param.class == "data.frame"]
  # params.individual <- params[param.class == "vector"]
  
  
  #colnames(dat.params) <- names(...)
  #dat.params <- dat.params[with(dat.params, order(IYEAR, STTIME)),]
  #dat.params$EMYR <- sub("IYEAR", "EMYR", as.character(dat.params$IYEAR))
  #dat.params$EMD <- sub("\\.", "", sub("STTIME", "EMD", as.character(dat.params$STTIME)))
  dat.params <- cbind(run=paste0("* run: ", 1:nrow(dat.params)), dat.params)
  load(paste0(SIRIUS_HOME,"/schemas/", schema_name, "/exp_params.RData"))
  
  for (i in 2:ncol(dat.params)){
    datatype <- as.character(dat.paraminfo$datatype[match(colnames(dat.params)[i], dat.paraminfo$name)])
    switch(datatype, character = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = '", dat.params[,i], "'")
    }, float = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = ", dat.params[,i])
      dat.params[!grepl("\\.", dat.params[, i]), i] <- paste0(dat.params[!grepl("\\.", dat.params[, i]), i], ".")
    }, integer = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = ", dat.params[,i])
    })
  }
  # Reorder columns according to how the ayre sequenced in the experiment file
  dat.params <- dat.params[,c(1,order(match(colnames(dat.params)[-1], dat.paraminfo$name))+1)]
  # if("IYEAR" %in% names(...) & "STTIME" %in% names(...)){
  #   idx.timeparams <- match(c("IYEAR", "STTIME"), colnames(dat.params))
  #   
  #   dat.params <- dat.params[,c(colnames(dat.params)[-idx.timeparams], colnames(dat.params)[idx.timeparams])]
  # }
  #mat.params <- cbind(paste(run:1:nrow(mat.params), mat.params)
  writeLines(apply(dat.params, 1, paste, sep="\n", collapse="\n"), con = filename)
  return(dat.params)
}

attachLabel <- function(x,...){
  return(paste(names(x), x, ...))
}

setMethod("attachLabel", "data.frame",
          function(x,...){
            paste(colnames(x), x, ...) 
          } 
)


buildRerun <- function (..., filename){
  # Attach parameter name to parameter value
  params <- mapply(paste, as.list(names(...)), ..., sep="=", SIMPLIFY = FALSE)
  param.grp  <- lapply(..., t)  
  param.grp  <- lapply(param.grp, data.frame)
  
  dat.params <- expand.grid(params, KEEP.OUT.ATTRS = FALSE)
  colnames(dat.params) <- names(...)
  dat.params <- dat.params[with(dat.params, order(IYEAR, STTIME)),]
  dat.params$EMYR <- sub("IYEAR", "EMYR", as.character(dat.params$IYEAR))
  dat.params$EMD <- sub("\\.", "", sub("STTIME", "EMD", as.character(dat.params$STTIME)))
  dat.params <- cbind(run=paste0("* run: ", 1:nrow(dat.params)), dat.params)
  # if("IYEAR" %in% names(...) & "STTIME" %in% names(...)){
  #   idx.timeparams <- match(c("IYEAR", "STTIME"), colnames(dat.params))
  #   
  #   dat.params <- dat.params[,c(colnames(dat.params)[-idx.timeparams], colnames(dat.params)[idx.timeparams])]
  # }
  #mat.params <- cbind(paste(run:1:nrow(mat.params), mat.params)
  writeLines(apply(dat.params, 1, paste, sep="\n", collapse="\n"), con = filename)
  return(dat.params)
}
