# Title: ORYZA Input Data files

#' Create ORYZA CONTROL.DAT file
#'
#' This function creates the main control file (CONTROL.DAT) for an ORYZA simulation.
#'
#' @param oryza.dir The directory where the ORYZA simulation files are located.
#' @param FILEIT The name of the experimental data file.
#' @param FILEI1 The name of the crop data file.
#' @param FILEI2 The name of the soil data file.
#' @param FILEIR The name of the rerun file.
#' @param SOILKILL A character string ('YES' or 'NO') indicating whether soil processes should continue after crop maturity. Default is 'NO'.
#' @param strun The starting run number. Default is 1.
#' @param endrun The ending run number. Default is 100000.
#' @param PRDEL The output time step in days. Default is 1.
#' @param DELTMP A character string ('Y' or 'N') indicating whether to delete temporary output files. Default is 'N'.
#' @return The full path to the created CONTROL.DAT file.
#' @export
oryza.control <- function(oryza.dir, FILEIT, FILEI1, FILEI2, FILEIR, SOILKILL = "NO", strun = 1, endrun = 100000, PRDEL = 1, DELTMP = "N") {
  #controldat.params <- args(as.list(environment()))
  #vars.int <- c("strun", "endrun", "IPFORM", "IFLAG")
  #vars.str <- c("FILEIT", "FILEI1", "FILEI2", "FILEIR")
  #controldat.params[vars.str] <- lapply(controldat.params[vars.str], sQuote)
  txt.controldat <- c(
    "CONTROLFILE = 'CONTROL.DAT'",
    paste0("strun = ", strun),
    paste0("endrun = ", endrun),
    "FILEON   = 'res.dat'      ! Output file",
    "FILEOL   = 'model.log'    ! Log file",
    paste0("FILEIT   = '", FILEIT, "'   ! Experimental data"),
    paste0("FILEI1   = '", FILEI1, "'   ! Crop data"),
    paste0("FILEI2   = '", FILEI2, "'   ! Soil File"),
    paste0("SOILKILL = '", SOILKILL, "' !* 'NO' soil processes continue after crop maturity."),
    paste0("FILEIR   = '", FILEIR, "'   ! Rerun file"),
    paste0("PRDEL    = ", trunc(PRDEL), ".  ! Output time step (day)"),
    "IPFORM   = 5     ! Code for output table format:",
    "                 ! 4 = spaces between columns",
    "                 ! 5 = TAB's between columns (spreadsheet output)",
    "                 ! 6 = two column output",
    "COPINF   = 'N'   ! Switch variable whether to copy the input files",
    "                 ! to the output file ('N' = do not copy,",
    "                 ! 'Y' = copy)",
    paste0("DELTMP   = '", DELTMP, "'   ! Switch variable what should be done with the"),
    "                 ! temporary output file ('N' = do not delete,",
    "                 ! 'Y' = delete)",
    "IFLAG    = 1100  ! Indicates where weather error and warnings",
    "                 ! go (1101 means errors and warnings to log",
    "                 ! file, errors to screen, see FSE manual)"
  )

  writeLines(txt.controldat, con = paste(oryza.dir, "CONTROL.DAT", sep = "/"))
  return(paste(oryza.dir, "CONTROL.DAT", sep = "/"))
}

#' Create an ORYZA experiment file
#'
#' This function creates an experiment file (.exp) for an ORYZA simulation.
#'
#' @param oryza.dir The directory where the ORYZA simulation files will be written.
#' @param param.list A list of parameters to be included in the experiment file. The names of the list elements should correspond to the parameter names in the ORYZA model.
#' @param expbase.df A data frame containing the base experiment file parameters. The default is `oryzaparams.exp`.
#' @param filename The name of the experiment file to be created. Default is "default.exp".
#' @return The full path to the created experiment file.
#' @export
oryza.experiment <- function(oryza.dir, param.list, expbase.df = oryzaparams.exp, filename = "default.exp") {#exp.params, filename, skip.desc = TRUE) {
  #expbase.df  <- readEXPFILE(base.expfile, reftable = TRUE)
  dat.paraminfo <- attr(expbase.df, "reftable")

  # Format new param values ----
  # expparams.class <- lapply(param.list, class)
  idx.thisexperiment <- match(names(param.list), dat.paraminfo$name)

  # character parameters (need single quotes)
  param.list[which(dat.paraminfo$datatype[idx.thisexperiment] == "character")] <- sQuote(param.list[which(dat.paraminfo$datatype[idx.thisexperiment] == "character")])

  # float parameters (need decimal point if not there)
  flt.params  <- which(dat.paraminfo$datatype[idx.thisexperiment] == "float" & dat.paraminfo$nval[idx.thisexperiment] == "scalar")
  param.list[flt.params[!grepl("\\.", param.list[flt.params])]] <- paste0(param.list[flt.params[! grepl("\\.", param.list[flt.params])]], ".")

  matrix.params  <- which(dat.paraminfo$nval[idx.thisexperiment] == "matrix")
  for (i in matrix.params) {
    mat.dims <- dim(param.list[[i]])
    if (dat.paraminfo$datatype[idx.thisexperiment][i] == "float") {
      param.list[[i]] <- sprintf("%.1f", param.list[[i]])
    }
    paramtext <- ifelse(!grepl("\\.", param.list[[i]]), paste0(param.list[[i]], "."), as.character(param.list[[i]]))
    paramtext <- matrix(paramtext, nrow = mat.dims[1], ncol = mat.dims[2])
    paramtext <- apply(paramtext, MARGIN = 1, FUN = paste, collapse = ", ")
    param.list[[i]] <- paste(paramtext, collapse = "\n")
  }

  expbase.df $value[match(names(param.list), expbase.df $parameter)] <- as.character(param.list)

  expbase.df $exptxt <- paste0(expbase.df $parameter, " = ", trimws(gsub("[[:space:]]*\\;[[:space:]]*", "\n", expbase.df $value)))

  writeLines(expbase.df $exptxt, con = paste0(oryza.dir, "/", filename))
  return(paste0(oryza.dir, "/", filename))
}

#' Create an ORYZA soil file
#'
#' This function creates a soil file (.sol) for an ORYZA simulation based on soil parameters.
#'
#' @param oryza.dir The directory where the soil file will be created.
#' @param soilparams A named vector or list of soil parameters.
#' @param paramnames An optional character vector of parameter names, to be used if `soilparams` is not a named vector.
#' @param TKL A numeric vector representing the thickness of soil layers.
#' @param xyprecision An integer specifying the precision for longitude and latitude in the filename. Default is 7.
#' @param filename_prefix A character string to be used as a prefix for the output filename. Default is "soilgrids".
#' @param filename_xy A boolean indicating whether to include x/y coordinates in the filename. Default is TRUE.
#' @param overwrite A boolean indicating whether to overwrite the file if it already exists. Default is FALSE.
#' @return The full path to the created soil file.
#' @export
oryza.soil <- function(oryza.dir, soilparams, paramnames = NULL, TKL = c(rep(0.05, 6), 0.3, 0.4), xyprecision = 7, filename_prefix = "soilgrids", filename_xy = TRUE, overwrite = FALSE) {
  if (is.list(soilparams)) soilparams <- unlist(soilparams)

  if (filename_xy) {
    outfile <- paste0(oryza.dir, "/", sprintf(paste0("%s_x%0.", xyprecision, "f_y%0.", xyprecision, "f"), filename_prefix, soilparams["lon"], soilparams["lat"]), ".sol")
  } else {
    outfile <- paste0(oryza.dir, "/", sprintf(paste0("%s_cell%0", max(sapply(soilparams["cell"], nchar)), "g"), filename_prefix, soilparams["cell"]), ".sol")
  }

  if (!file.exists(outfile) || overwrite) {

    if (!is.null(paramnames)) {
      names(soilparams) <- paramnames
    } else {
      paramnames <- names(soilparams)
    }
    #if(file.exists(outfile)) next
    txt.soilfile <- c(
      "SCODE = 'PADDY'",
      "WL0MX = 100.0",
      "NL = 8",
      paste0("TKL = ", paste(sprintf("%0.03f", TKL), collapse = ",")),
      "ZRTMS = 1.0",
      "SWITPD = 1",
      "NLPUD = 5",
      paste0("WCSTRP = ", numvectorToString(soilparams[grep("WCST", paramnames)], 5)),
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
      paste0("FIXPERC = ", numvectorToString(min(soilparams[grep("KST", paramnames)]), 7)),
      "PTABLE = 1.0, 1.0, 366.0, 1.0",
      "SWITKH  = 0",
      "SWITPF  = 0",
      paste0("CLAYX = ", numvectorToString(soilparams[grep("clay", paramnames, value = TRUE)], 4)),
      paste0("SANDX = ", numvectorToString(soilparams[grep("sand", paramnames, value = TRUE)], 4)),
      paste0("BD = ", numvectorToString(soilparams[grep("bdod", paramnames, value = TRUE)], 4)),
      paste0("SOC = ", numvectorToString(soilparams[grep("soc", paramnames, value = TRUE)], 3)),
      paste0("SON = ", numvectorToString(soilparams[grep("nitrogen", paramnames, value = TRUE)], 3)),
      paste0("KST = ", numvectorToString(soilparams[grep("KST", paramnames, value = TRUE)], 5)),
      paste0("WCST = ", numvectorToString(soilparams[grep("WCST", paramnames, value = TRUE)], 5)),
      paste0("WCFC = ", numvectorToString(soilparams[grep("WCFC", paramnames, value = TRUE)], 5)),
      paste0("WCWP = ", numvectorToString(soilparams[grep("WCWP", paramnames, value = TRUE)], 5)),
      paste0("WCAD = ", numvectorToString(soilparams[grep("WCAD", paramnames, value = TRUE)], 5)),
      "WL0I = 10.0",
      paste0("WCLI = ", numvectorToString(soilparams[grep("WCST", paramnames, value = TRUE)] - 0.02, 5)),
      "RIWCLI = 'YES'",
      "SATAV = 18.0",
      paste0("SOILT = ", numvectorToString(c(22, 21, 20, 19, 18, 17, 16, 16), 5)),
      paste0("WCLINT = ", paste(as.numeric(sapply(1:8, rep, 3)), collapse = ","))
    )

    writeLines(txt.soilfile, con = outfile)
  }
  return(outfile)
}

# TODO: Make consistent with other functions, i.e. use of oryza.dir parameter
#' Create an ORYZA rerun file
#'
#' This function creates a rerun file (.rer) for running multiple ORYZA simulations with different parameter sets.
#'
#' @param oryza.dir The directory where the rerun file will be created.
#' @param rerun.param.list A list of parameters for the rerun file.
#' @param param.reference A data frame containing reference information for the parameters. The default is `attr(oryzaparams.exp, "reftable")`.
#' @param filename The name of the rerun file. Default is "default.rer".
#' @return The full path to the created rerun file.
#' @export
oryza.rerun <- function(oryza.dir, rerun.param.list, param.reference = attr(oryzaparams.exp, "reftable"), filename = "default.rer") {

  # Items in a data.frame means each row come together and should not be mapped out individually
  # to other elements through expand.grid as in the case of simdates. Should be expanded to any other
  # data frame in rerun.param.list

  if ((length(rerun.param.list) == 1) && ("simdates" == names(rerun.param.list))) {
    dat.params <- rerun.param.list$simdates
  } else if ("simdates" %in% names(rerun.param.list)) {
    dat.simdates <- rerun.param.list$simdates
    rerun.param.list <- c(rerun.param.list[-which(names(rerun.param.list) == "simdates")], list(simdates = seq_len(nrow(rerun.param.list$simdates))))
    dat.params <- expand.grid(rerun.param.list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    dat.params <- cbind(dat.params[-which(colnames(dat.params) == "simdates")], dat.simdates[dat.params$simdates, ])
  }

  # Attach parameter name to parameter value and format
  for (i in 1:ncol(dat.params)){
    datatype <- as.character(param.reference$datatype[match(colnames(dat.params)[i], param.reference$name)])
    switch(datatype, character = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = '", dat.params[, i], "'")
    }, float = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = ", dat.params[, i])
      dat.params[!grepl("\\.", dat.params[, i]), i] <- paste0(dat.params[!grepl("\\.", dat.params[, i]), i], ".")
    }, integer = {
      dat.params[, i] <- paste0(colnames(dat.params)[i], " = ", dat.params[, i])
    })
  }

  # Add rerun number
  dat.params <- cbind(run = paste0("* run: ", seq_len(nrow(dat.params))), dat.params)

  # Reorder columns according to how they are sequenced in the experiment file
  dat.params <- dat.params[, c(1, order(match(colnames(dat.params)[-1], param.reference$name)) + 1)]
  writeLines(apply(dat.params, 1, paste, sep = "\n", collapse = "\n"), con = paste0(oryza.dir, "/", filename))
  return(paste0(oryza.dir, "/", filename))
}

#' Build an ORYZA rerun file
#'
#' A helper function to build a rerun file for ORYZA simulations.
#'
#' @param ... A set of named arguments representing the parameters to be varied in the rerun file.
#' @param filename The name of the output rerun file.
#' @return A data frame containing the parameters for each run.
#' @export
buildRerun <- function(..., filename) {
  # Attach parameter name to parameter value
  rerun.param.list <- mapply(paste, as.list(names(...)), ..., sep = "=", SIMPLIFY = FALSE)
  param.grp  <- lapply(..., t)
  param.grp  <- lapply(param.grp, data.frame)

  dat.params <- expand.grid(rerun.param.list, KEEP.OUT.ATTRS = FALSE)
  colnames(dat.params) <- names(...)
  dat.params <- dat.params[with(dat.params, order(IYEAR, STTIME)), ]
  dat.params$EMYR <- sub("IYEAR", "EMYR", as.character(dat.params$IYEAR))
  dat.params$EMD <- sub("\\.", "", sub("STTIME", "EMD", as.character(dat.params$STTIME)))
  dat.params <- cbind(run = paste0("* run: ", seq_len(nrow(dat.params))), dat.params)

  writeLines(apply(dat.params, 1, paste, sep = "\n", collapse = "\n"), con = filename)
  return(dat.params)
}
