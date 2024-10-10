# TODO: SIRIUS

schema.list <- function(sirius.home = SIRIUS_HOME, envir = parent.env(environment())) {
  return(list.dirs(paste0(sirius.home, "/schemas"), recursive = FALSE, full.names = FALSE))
}

schema.select <- function(schema = "", load.schema = TRUE) {
  #assign("SCHEMA_SELECTED", schema, envir = parent.env(environment()))
  schema.dir <- paste0("./schemas/", schema)
  if (load.schema) {
    load(paste0(schema.dir, "/schemaconfig.rdata"))
    schema.config <- ls()
    schema.config <- grep("SCHEMA_", schema.config, value = TRUE)
    mapply(assign, as.list(schema.config), lapply(as.list(schema.config), get), MoreArgs = list(envir = parent.env(environment())))
    assign("rst.base", rast(paste0(schema.dir, "/baseraster.tif")), envir = parent.env(environment()))
  }

  return(schema)
}
