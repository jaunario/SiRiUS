experiment_basefile <- ifelse(!exists("SCHEMA_BASEEXPFILE"), "./oryza/experiment_template.exp", SCHEMA_BASEEXPFILE)

# CREATE Experiment File Parameters Reference Tables
txt.file <- readLines(experiment_basefile)
txt.expheader <- txt.file[1:12]
txt.file <- txt.file[-(1:12)]

idx.sections <- grep("^\\* [0-9]{1}\\.[[:space:]]+[[:alpha:]]+[[:space:]]*$", txt.file)
idx.activeparams <- unlist(sapply(c("^[A-Z0-9]+[[:space:]]*\\=", "^[A-Z0-9]+_[A-Z0-9]+[[:space:]]*\\="), grep, txt.file))
idx.inactiveparams <- unlist(sapply(c("^\\*[A-Z0-9]+[[:space:]]*\\=", "^\\*[A-Z0-9]+_[A-Z0-9]+[[:space:]]*\\="), grep, txt.file))

dat.idx <- data.frame(idx = c(idx.activeparams, idx.inactiveparams),
                      active = c(rep(TRUE, length(idx.activeparams)),
                                 rep(FALSE, length(idx.inactiveparams))))

dat.idx <- dat.idx[order(dat.idx$idx), ]
dat.idx$txt <- txt.file[dat.idx$idx]

if (!exists("dat.idxbk")) dat.idxbk <- dat.idx
dat.idxbk$done <-  FALSE
# dat.idxbk -> dat.idx

seq.tracker <- 0
if (exists("dat.options")) rm(dat.options)
if (exists("dat.paraminfo")) rm(dat.paraminfo)
if (exists("lst.active")) rm(lst.active)

# TODO: Simplify this while-loop
while (nrow(dat.idx) > 0) {

  this.param <- trimws(unlist(strsplit(txt.file[dat.idx$idx[1]], split = "=", fixed = TRUE)))
  thisparam.name <- this.param[1]
  thisparam.name <- sub("\\*", "", thisparam.name)

  if (exists("dat.param.info") && thisparam.name %in% dat.paraminfo$name) next
  seq.tracker <- seq.tracker + 1

  # Find if there are other lines mentioning the parameter
  #idx.parammarkers <- grep(thisparam.name, txt.file)
  idx.parammarkers <- c(grep(paste0("^", thisparam.name, "[[:space:]]*\\="), txt.file),
                        grep(paste0("^\\*", thisparam.name, "[[:space:]]*\\="), txt.file))

  if (nrow(dat.idx) > 1) {
    dat.tmp <- dat.idx[-which(dat.idx$idx %in% idx.parammarkers), ]
    # Check if more lines in between the next param
    if (nrow(dat.tmp) > 0) {
      if (max(idx.parammarkers) > (dat.tmp$idx[1])) {
        idx.parammarkers <- idx.parammarkers[idx.parammarkers < dat.tmp$idx[1]]
      }
      idx.fullparamtxt <- min(idx.parammarkers):(dat.tmp$idx[1] - 1)
    } else {
      idx.fullparamtxt <- min(idx.parammarkers):max(idx.parammarkers)
    }
    rm(dat.tmp)
  } else {
    idx.fullparamtxt <- min(idx.parammarkers):max(idx.parammarkers)
  }

  thisparam.txt <- txt.file[idx.fullparamtxt]

  # Identify vartype based on flags
  if (sum(grepl("!@", thisparam.txt)) > 0) {
    vartype <- factor("switch", levels = c("required", "switch", "optional"))
  } else if (sum(grepl("!\\*", thisparam.txt)) > 0) {
    vartype <- factor("optional", levels = c("required", "switch", "optional"))
  } else {
    vartype <- factor("required", levels = c("required", "switch", "optional"))
  }

  # Multiline-parameters are bound by blank lines
  param.group <- which(thisparam.txt != "")
  txt.groups <- mapply("[", list(thisparam.txt), manipulateR::consecutive.groups(param.group), SIMPLIFY = FALSE)
  if (length(txt.groups) > 1) {
    groupmarked <- which(sapply(mapply(grep, list(paste0("^", thisparam.name, "[[:space:]]*\\=")), txt.groups), length) > 0)
    if (length(groupmarked) == 0) {
      groupmarked <- which(sapply(mapply(grep, list(paste0("^\\*", thisparam.name, "[[:space:]]*\\=")), txt.groups), length) > 0)
    }
    txt.groups <- txt.groups[[groupmarked]]
  } else {
    txt.groups <- txt.groups[[1]]
  }
  thisparam.txt <- txt.groups

  file.loc <- match(thisparam.txt, txt.file) # Determine location to find where to replace

  # Local only to param txt
  idx.paramoptions <- grep(paste0(thisparam.name, "[[:space:]]*\\="), thisparam.txt)
  if (length(idx.paramoptions) > 1) {
    idx.paramend <- c(idx.paramoptions[2:length(idx.paramoptions)] - 1, length(thisparam.txt))
    idx.options <- mapply(":", as.list(idx.paramoptions), as.list(idx.paramend), SIMPLIFY = FALSE)
    param.options <- mapply("[", list(thisparam.txt), idx.options, SIMPLIFY = FALSE)
  } else {
    param.options <- list(thisparam.txt)
  }
  idx.selvalue <- unlist(which(sapply(sapply(param.options, grep, pattern = paste0("^", thisparam.name)), length) > 0))
  param.options.desc <- param.options.vals <- vector()
  for (i in seq_along(param.options)) {
    this.option <- param.options[[i]]

    # Separate the description
    desc.marker <- grep("!", this.option)
    if (length(desc.marker) > 0) {
      thisparamoption.desc <- strsplit(this.option[desc.marker], split = "!")
      thisparamoption.desc <- do.call(rbind, thisparamoption.desc)
      thisparamoption.desc <- sub("\\*", "", thisparamoption.desc)
      this.option[desc.marker] <- thisparamoption.desc[, 1]
      param.options.desc <- c(param.options.desc, trimws(paste(thisparamoption.desc[, 2], collapse = "; ")))
    } else {
      param.options.desc <- c(param.options.desc, NA)
    }

    # Parse parameter values
    thisparamoption.val <- sub(paste0(thisparam.name, "[[:space:]]*\\="), "", this.option)
    thisparamoption.val <- sub("\\*", "",  thisparamoption.val)
    thisparamoption.val <- thisparamoption.val[thisparamoption.val != ""]
    param.options.vals <- c(param.options.vals, trimws(paste(thisparamoption.val, collapse = "; ")))
  }

  param.options <- data.frame(parameter = thisparam.name, value = param.options.vals, description = param.options.desc)

  param.options$description <- sub("@", "", param.options$description)
  param.options$description <- sub("\\*", "", param.options$description)
  param.options$description <- trimws(param.options$description)

  if (ncol(param.options) == 3) {
    param.options <- data.frame(parameter = thisparam.name, value = param.options[, 2], description = param.options[, 3])
  } else {
    param.options <- data.frame(parameter = thisparam.name, value = param.options[, 2], description = NA)
  }

  if (thisparam.name == "SWITIR") switir.marker <- param.options[idx.selvalue, ]

  if (length(idx.selvalue) > 0) {
    if (!exists("lst.active")) {
      lst.active <- list(param.options[idx.selvalue, ])
      names(lst.active) <- thisparam.name
    } else {
      old.names <- names(lst.active)
      lst.active <- c(lst.active, list(param.options[idx.selvalue, ]))
      names(lst.active) <- c(old.names, thisparam.name)
    }
  }

  idx.desc <- grep(paste0("^\\*--[[:space:]]+", thisparam.name), txt.file)
  if (length(idx.desc) > 0) {
    idx.desc <- idx.desc:(min(idx.parammarkers) - 1)
    txt.desc <- paste(txt.file[idx.desc], collapse = "; ")
    txt.desc <- gsub("\\*", "", txt.desc)
    txt.desctemp <- trimws(unlist(strsplit(txt.desc, ":", fixed = TRUE))[2])
    if (is.na(txt.desctemp)) txt.desc <- trimws(unlist(strsplit(txt.desc, "is"))[2]) else txt.desc <- txt.desctemp
  } else {
    txt.desc <- paste(param.options$description, collapse = "; ")
  }

  if (!exists("dat.options")) dat.options <- param.options else dat.options <- rbind(dat.options, param.options)

  if (sum(grepl("\\,", param.options$value)) > 0) {
    nval <- "matrix"
  } else {
    nval <- "scalar"
  }

  if (sum(grepl("'", param.options$value)) > 0) {
    datatype <- factor("character", levels = c("character", "integer", "float"))
  } else if (sum(grepl("[[:digit:]]+\\.", param.options$value)) > 0) {
    datatype <- factor("float", levels = c("character", "integer", "float"))
  } else {
    datatype <- factor("integer", levels = c("character", "integer", "float"))
  }

  dat.thisparam <- data.frame(name = thisparam.name,
                              vartype = vartype,
                              datatype = datatype,
                              description = txt.desc,
                              nval = nval,
                              sequence = seq.tracker,
                              line_st = min(idx.fullparamtxt),
                              line_en = max(file.loc))

  if (!exists("dat.paraminfo")) dat.paraminfo <- dat.thisparam else dat.paraminfo <- rbind(dat.paraminfo, dat.thisparam)

  dat.idx <- dat.idx[-which(dat.idx$idx %in% idx.fullparamtxt), ]
  dat.idxbk$done[dat.idxbk$idx %in% idx.fullparamtxt] <- TRUE

}
save(dat.paraminfo, file = paste0("./schemas/", SCHEMA_NAME, "/exp_params.RData"))
save(dat.options, file = paste0("./schemas/", SCHEMA_NAME, "/exp_options.RData"))

dat.thisexperiment <- do.call(rbind, lst.active)

#dat.thisexperiment$include <- dat.thisexperiment$vartype!="optional"

# Format new param values
idx.thisexperiment <- match(names(SCHEMA_EXPPARAMS), dat.paraminfo$name)

# character parameters (need single quotes)
SCHEMA_EXPPARAMS[which(dat.paraminfo$datatype[idx.thisexperiment] == "character")] <- paste0("'", SCHEMA_EXPPARAMS[which(dat.paraminfo$datatype[idx.thisexperiment] == "character")], "'")

# float parameters (need decimal point if not there)
flt.params  <- which(dat.paraminfo$datatype[idx.thisexperiment] == "float")

SCHEMA_EXPPARAMS[flt.params[! grepl("\\.", SCHEMA_EXPPARAMS[flt.params])]] <- paste0(SCHEMA_EXPPARAMS[flt.params[! grepl("\\.", SCHEMA_EXPPARAMS[flt.params])]], ".")

dat.thisexperiment$value[match(names(SCHEMA_EXPPARAMS), dat.thisexperiment$parameter)] <- as.character(SCHEMA_EXPPARAMS)

dat.thisexperiment$exptxt <- paste0(dat.thisexperiment$parameter, " = ", trimws(gsub("[[:space:]]*\\;[[:space:]]*", "\n", dat.thisexperiment$value)))

writeLines(dat.thisexperiment$exptxt, con = paste("./schemas", SCHEMA_NAME, paste0(SCHEMA_NAME, ".exp"), sep = "/"))
