# TODO: SIRIUS

#' List available schemas
#'
#' This function lists the available simulation schemas in the SiRiUS home directory.
#'
#' @param sirius.home The path to the SiRiUS home directory. Defaults to `SIRIUS_HOME`.
#' @param envir The environment in which to evaluate the function. Default is the parent environment.
#' @return A character vector of schema names.
#' @export
schema.list <- function(sirius.home = SIRIUS_HOME, envir = parent.env(environment())) {
  return(list.dirs(paste0(sirius.home, "/schemas"), recursive = FALSE, full.names = FALSE))
}

#' Select a schema
#'
#' This function selects a schema and loads its configuration into the environment.
#'
#' @param schema The name of the schema to select.
#' @param load.schema A boolean indicating whether to load the schema configuration. Default is TRUE.
#' @return The name of the selected schema.
#' @export
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
