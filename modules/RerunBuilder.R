# RERUN_BASIC = c("IYEAR", "STTIME", "EMD", "CO2A") 
# 
writeRerun <- function (..., filename){
  params <- mapply(paste, as.list(names(...)), ..., sep="=", SIMPLIFY = FALSE)
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
