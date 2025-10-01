#' Create a worker directory
#'
#' This function creates a directory for a worker, copies necessary resources, and sets the worker's status.
#'
#' @param worker.id The ID of the worker.
#' @param workplace The directory where the worker's directory will be created.
#' @param resources A character vector of paths to the resource files to be copied to the worker's directory.
#' @return The ID of the worker.
#' @export
createWorker <- function(worker.id, workplace, resources) {
  # Create Worker dir and copy ORYZA files
  if (!dir.exists(paste0(workplace, "/", worker.id))) {
    if (dir.create(paste0(workplace, "/", worker.id), recursive = TRUE)) {
      file.copy(resources, paste0(workplace, "/", worker.id, "/", basename(resources)))
      # Set Worker status
      saveRDS("ready", file = paste0(workplace, "/", worker.id, "/", "status.rds"))
    }
  }
  return(worker.id)
}

#' Create a raster from ORYZA results
#'
#' This function creates a raster layer from a data frame of ORYZA simulation results.
#'
#' @param baseraster A base raster to define the extent and resolution of the output raster.
#' @param result.df A data frame containing the simulation results, with a 'cell' column and a column for the variable to be mapped.
#' @param var The name of the variable to map to the raster. If NA, the first column after 'cell' is used. Default is NA.
#' @param daily A boolean indicating if the data is daily. Not currently used in the function.
#' @param envir The environment in which to evaluate the function. Default is the parent environment.
#' @param ... Additional arguments (not used).
#' @return A SpatRaster object.
#' @export
oryzaResultRaster <- function(baseraster, result.df, var = NA, daily = FALSE, envir = parent.env(environment()), ...) {
  if (is.na(var) && length(colnames(result.df)[colnames(result.df) != "cell"])) var <- colnames(result.df)[colnames(result.df) != "cell"][1]
  rst.out <- rast(baseraster)
  rst.out[result.df$cell] <- result.df[, var]
  names(rst.out) <- var
  return(rst.out)
}

#' Map schema results to raster files
#'
#' This function processes the results of a simulation schema, mapping them to raster files.
#'
#' @param env The environment from which to get schema settings. Default is the parent environment.
#' @param remap A boolean indicating whether to remap the results. Not currently used. Default is FALSE.
#' @param ... Additional arguments (not used).
#' @return A list of paths to the created raster files.
#' @export
mapSchemaResults <- function(env = parent.env(environment()), remap = FALSE, ...) {
  if (!exists("SCHEMA_SELECTED", envir = env)) stop("No selected schema")
  oryza.outdir <- get("SCHEMA_INTERMDIR", envir = env)
  rst.base <- get("rst.base", envir = env)
  if (!dir.exists(oryza.outdir)) stop("Result directory not found. Kindly check schema progress.")
  results.dir <- list.dirs(oryza.outdir, recursive = FALSE, full.names = TRUE)
  files.list <- list()
  for (i in seq_along(results.dir)){
    results.subdir <- list.dirs(results.dir[i], recursive = FALSE, full.names = TRUE)
    if(grepl("daily", results.dir[i])) {
      vars <- get("SCHEMA_MAPVARS_RESDAT", envir = env)
    } else {
      vars <- get("SCHEMA_MAPVARS_OPDAT", envir = env)
    }

    files.maps <- vector()
    for (j in seq_along(results.subdir)) {
      if (j %in% 1:2) next
      files.rds <- dir(results.subdir[j], pattern = ".rds$", full.names = TRUE, recursive = TRUE)
      if (grepl("daily", results.dir[i])) {
        year <- basename(results.subdir[j])
        months <- sprintf("P%s_%02g", year, 1:12)
        list.filesbymonth <- lapply(as.list(months), grep, files.rds, value = TRUE)
        list.filesbymonth <- list.filesbymonth[sapply(list.filesbymonth, length) > 0]

        for (k in seq_along(list.filesbymonth)) {
          dat.rds <- sapply(list.filesbymonth[[k]], readRDS, USE.NAMES = FALSE, simplify = FALSE)
          dat.rds <- do.call(rbind, dat.rds)

          dat.rds <- split(dat.rds, format(dat.rds$date, "D%Y%m%d"))
          rst.out <- lapply(dat.rds,
                            FUN = function(df, lst.var, rst.b) {
                              return(sapply(lst.var, FUN = oryzaResultRaster, result.df = df, baseraster = rst.b))
                            },
                            lst.var = vars,
                            rst.b = rst.base)
          rst.out <- unlist(rst.out)
          ym.dir <- substr(names(dat.rds)[1], 1, 7)
          out.dir <- sprintf("./schemas/%s/maps/%s/%s", get("SCHEMA_SELECTED", envir = env), basename(results.dir[i]), ym.dir)
          if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)
          rst.out <- mapply(writeRaster, rst.out, filename = as.list(sprintf("./schemas/%s/maps/%s/%s/%s.tif", get("SCHEMA_SELECTED", envir = env), basename(results.dir[i]), ym.dir, sub("\\.", "_", names(rst.out)))),
                            MoreArgs = list(overwrite = TRUE))
          files.maps <- c(files.maps, sapply(rst.out, sources, USE.NAMES = FALSE))
        }
      } else {
        dat.rds <- sapply(files.rds, readRDS, USE.NAMES = FALSE, simplify = FALSE)
        dat.rds <- do.call(rbind, dat.rds)
        run.dir <- basename(results.subdir[j])
        out.dir <- sprintf("./schemas/%s/maps/%s/%s", get("SCHEMA_SELECTED", envir = env), basename(results.dir[i]), run.dir)
        if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)

        rst.out <- sapply(vars, FUN = oryzaResultRaster, baseraster = rst.base, result.df = dat.rds)
        rst.out <- mapply(writeRaster, rst.out, filename = as.list(sprintf("./schemas/%s/maps/%s/%s/%s_%s.tif", get("SCHEMA_SELECTED", envir = env), basename(results.dir[i]), run.dir, run.dir, vars)),
                          MoreArgs = list(overwrite = TRUE))
        files.maps <- c(files.maps, sapply(rst.out, sources, USE.NAMES = FALSE))
      }
    }
    names(files.maps) <- NULL
    files.list[[i]] <- files.maps
  }
  names(files.list) <- basename(results.dir)
  return(files.rds)
}
