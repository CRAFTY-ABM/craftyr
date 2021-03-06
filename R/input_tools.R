#' Determines the model input folder for the given datadir
#' 
#' @param simp SIMulation Properties
#' @param datatype one of c("capitals", "demand", "agentparams", "productivities", "competition", "runs")
#' @return model output directory as string 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputDir <- function(simp, datatype = NULL) {
	return <- paste(simp$dirs$data,
					if (is.null(datatype)) { 
						simp$sim$folder
					} else if (datatype %in% c("capitals")) {
						paste(simp$sim$folder,"worlds", simp$sim$worldname,
							if(!is.null(simp$sim$regionalisation)) paste("regionalisations", 
									simp$sim$regionalisation, sep="/"), "capitals", sep="/")
					} else if (datatype %in% c("demand")) {
						paste(simp$sim$folder, "worlds", simp$sim$worldname,
								if(!is.null(simp$sim$regionalisation)) paste("regionalisations", 
											simp$sim$regionalisation, simp$sim$scenario, sep="/"), sep="/")
					} else if (datatype %in% c("agentparams")) {
						paste(simp$sim$folder, "worlds", simp$sim$worldname, sep="/")
					} else if (datatype %in% c("productivities")) {
						paste(simp$sim$folder, "production", sep="/")
					} else if (datatype %in% c("competition")) {
						paste(simp$sim$folder, "competition", sep="/")
					} else if (datatype %in% c("runs")) {
						simp$sim$folder
					},
					sep="/")
}
#' Determines the model output folder(s) for the given \eqn{simp}
#' 
#' If one of the parameters \eqn{simp$sim$version}, \eqn{simp$sim$world}, 
#' \eqn{simp$sim$regionalisation}, \eqn{simp$sim$scenario}, \eqn{simp$sim$runids},
#' or \eqn{simp$sim$regions} is a vector, a vector of directories is returned.
#' @param simp SIMulation Properties
#' 			\itemise{
#' 				\item{simp$dirs$output$simulation}
#' 				\item{simp$sim$world}
#' 				\item{simp$sim$regionalisation -can be \code{NULL}}
#' 				\item{simp$sim$scenario}
#' 				\item{simp$sim$runids}
#' 				\item{simp$sim$regions}
#' 				\item{simp$sim$hasregiondir}
#' 				\item{simp$sim$regions}}
#' @return String or vector of output folder(s)
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelOutputDir <- function(simp) {
	result = do.call(paste, c(expand.grid(simp$dirs$output$simulation,
							simp$sim$world,
							if(!is.null(simp$sim$regionalisation)) simp$sim$regionalisation else "",
							if(!is.null(simp$sim$scenario)) simp$sim$scenario else "",
							if(!is.null(simp$sim$runids)) simp$sim$runids else "",
							if(!is.null(simp$sim$regions) & simp$sim$hasregiondir) simp$sim$regions else 
										rep("", times=max(1,length(simp$sim$regions)))), sep="/"))
	
	if (length(result) == 0) {
		R.oo::throw.default(paste("No output dir(s) constructed. Check simp parameters\n\t", 
					"dirs$output$simulation (",  simp$dirs$output$simulation, ")\n\t", 
					"sim$world (",  simp$sim$world, ")\n\t",
					"sim$scenario (",  simp$sim$scenario, ")\n\t",
					"and sim$runids (",  simp$sim$runids, ")!", sep=""))
	}
	
	result
}
#' Determine a vector of ticks for the files in the given directory that match the given pattern
#' 
#' If pattern is not specified it is assumed that all requested files in the directory are of the form 
#' *<scenario>-<runid>-<region>-<datatype>-<dataname>-<tick>.<extension>.
#' If extension is not given, any extension is considered. If datatype and dataname are 
#' not given, the according parts of the filename is assumed not to exist (also the associated '-').
#' 
#' @inheritParams input_tools_getModelOutputDir
#' @param dir directory to search in
#' @param pattern pattern for matching considered files including the tick (as TICK) and excluding the extension. 
#' 		  If NULL, the default is used (\\.*<scenario>-<runid>-<region>-<datatype>-<dataname>-TICK).
#' @param datatype (e.g. "Capital")
#' @param dataname (e.g. "Cap1")
#' @param extension used to restrict filenames by extension (without "."!)
#' @param starttick start tick of requested range of ticks
#' @param endtick end tick of requested range of ticks
#' @param tickinterval interval of requested range of ticks
#' @return vector of ticks
#' @author Sascha Holzhauer
#' @export
input_tools_getAvailableTicks <- function(simp, dir, pattern = NULL, 
		datatype = NULL,
		dataname = NULL,
		extension = NULL,
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick, 
		tickinterval = 1) {
	# pattern = NULL
	
	# TODO use input_tools_constructFilenameList
	if (is.null(pattern)) pattern <- paste(".*",
			simp$sim$scenario, "-",
			simp$sim$runids, "-",
			simp$sim$regions, "-",
			datatype, if(!is.null(datatype)) "-",
			dataname, if(!is.null(dataname)) "-", ".", sep="")

	if (length(grep("TICK", pattern)) ==  0) {
		R.oo::throw.default(paste("Pattern must contain 'TICK' (pattern: ", pattern, "). Check simp$sim$filepartorder!", sep=""))
	}

	patternRegex 	<- sub("TICK", "\\d*", pattern, fixed = TRUE)
	patternPre		<- stringr::str_sub(pattern, 1, regexpr(pattern ='TICK', pattern)[[1]] - 1)
	patternPost		<- stringr::str_sub(pattern, regexpr(pattern ='TICK', pattern)[[1]] + 4, -1)

	futile.logger::flog.info("Look for files in dir %s", dir, name="craftyr.input.tools")
	
	files <- grep(paste(patternRegex, ".", if (!is.null(extension)) extension else "*", sep=""),
			list.files(path=dir),value=T)

	futile.logger::flog.debug("Found files in %s\nfor pattern'%s.%s'\n%s",
			dir,
			patternRegex,
			if (!is.null(extension)) extension else "",
			paste("\t", files, collapse = "\n"),
			name="craftyr.input.tools")
	
	
	ticks <- as.numeric(sub(paste(patternPost, ".", if (!is.null(extension)) extension else "*", sep=""), "", 
					sub(patternPre, "", files)))
		
	if (length(ticks) == 0) {
		R.oo::throw.default(paste("No ticks found in ", dir, "\nfor pattern '", patternRegex, ".", if (!is.null(extension)) 
											extension else "*", "' (available files:\n", 
						paste(list.files(path=dir), collapse = "\n"), ")", sep=""))
	}
	filteredTicks <- ticks[ticks %in% seq(from=starttick, to=endtick, by=tickinterval)]
	
	futile.logger::flog.debug("Available ticks: %s\nRequested Ticks: %s",
			paste(ticks, collapse =", "),
			paste(seq(from=starttick, to=endtick, by=tickinterval), collapse =", "),
			name="crafy.input.raster")
	
	if (length(ticks) == 0) {
		R.oo::throw.default("None of found ticks matched parameters starttick (", starttick, "), endtick (", enttick, 
				") and interval (", tickinterval, "), \nAvailable ticks: ", ticks)
	}
	filteredTicks
}
#' Determines the model output filenames for the given simp and datatype, dataname, ticks settings
#' 
#' Able to read multiple scenarios (as in \code{simp$sim$scenario})/regions (as in \code{simp$sim$regions})/
#' runids (as in \code{simp$sim$runids}).
#' Attaches infos (region, runid, scenario).
#' 
#' @inheritParams input_tools_getAvailableTicks
#' @param folders 
#' @param simp 
#' @param folders 
#' @param datatype 
#' @param dataname 
#' @param extension 
#' @param returnfileinfo 
#' @param pertick  if TRUE the filename will be complemented by all available ticks
#' 					(using \code{\link{input_tools_getAvailableTicks}})
#' @param starttick only required when \code{pertick == TRUE}
#' @param endtick only required when \code{pertick == TRUE}
#' @param tickinterval only required when \code{pertick == TRUE}
#' 
#' @return list of vector of filenames (list elements rerpesent files of one folder)
#' 
#' @see input_tools_getModelOutputDir
#' @see input_tools_getAvailableTicks
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelOutputFilenames <- function(simp, 
		folders = input_tools_getModelOutputDir(simp),
		datatype = NULL,
		dataname = NULL,
		extension = NULL,
		returnfileinfo = TRUE,
		pertick = FALSE,
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick, 
		tickinterval = 1) {
	
	# generate file info matrix by combining various vectors:
	fileinfogrid <- c(expand.grid(				
					Scenario = 	if (is.null(simp$sim$scenario)) "" else simp$sim$scenario,
					Runid = 	if (is.null(simp$sim$runids)) "" else simp$sim$runids,
					Region = 	if (is.null(simp$sim$regions)) "" else simp$sim$regions))
	
	# attach filename:
	# Folder = filename need to be concatenated here because of lapply when pertick == FALSE 
	# (difficulties for vector of folders)
	fileinfogrid <- data.frame(fileinfogrid, Filename = paste(folders, 
					input_tools_getFilenameListRepetitions(simp, datatype = datatype, dataname = dataname, 
							folders, considertick = pertick),
					sep="/"))
	
	if (pertick) {
		fileinfogrid <- mapply(function(foldername, scenario, runid, region, filename)
										expand.grid(
											Scenario = scenario,
											Runid = runid,
											Region = region,
											Tick = input_tools_getAvailableTicks(simp = simp,
													dir = foldername, 
													pattern = paste(substr(filename,
																	gregexpr(pattern ='/', filename)[[1]][length(gregexpr(pattern ='/', filename)[[1]])] + 1, 
															nchar(as.character(filename))), sep=""),
													datatype = datatype,
													dataname = dataname,
													extension = extension,
													starttick = starttick,
													endtick = endtick, 
													tickinterval = tickinterval),
											Filename = filename),
										folders,
										fileinfogrid$Scenario, fileinfogrid$Runid, fileinfogrid$Region, fileinfogrid$Filename,
										SIMPLIFY = FALSE)

		fileinfogrid <- sapply(fileinfogrid, function(infogrid) {
					infogrid$Filename = mapply(function(tick, filename)
								paste(sub("TICK", tick, filename), ".", extension, sep=""),
							infogrid$Tick, infogrid$Filename)
					#rownames(infogrid) <- 
					infogrid},
				simplify = FALSE)
	} else {
		# attach filename:
		fileinfogrid <- sapply(list(fileinfogrid), function(infogrid) {
					infogrid$Filename = paste(infogrid$Filename, ".", extension, sep="")
					infogrid},
							simplify = FALSE)
	}
	
	lapply(fileinfogrid$filename, shbasic::sh.checkFilename)
	
	if (returnfileinfo) {
		return = fileinfogrid
	} else {
		return = do.call(cbind, fileinfogrid)$Filename
	}
	return
}
input_tools_getFilenameListRepetitions <- function(simp, 
		datatype = "",
		dataname = "",
		folders,
		considertick = TRUE) {
	# TODO check & test
	filenameList <- expand.grid(input_tools_constructFilenameList(simp, datatype = datatype, dataname = dataname,
					considertick = considertick), stringsAsFactors = FALSE)
	reps <- length(folders) / length(filenameList[,1])
	reps <- if (reps < 1) 1 else reps
	rep(do.call(paste, c(filenameList, sep="")), each=reps)
}
#' Construct a list of filenames
#' 
#' @param simp considered elements: \itemize{
#' 				\item{\code{simp$sim$filepartorder}}
#' 				\item{\code{simp$sim$scenario}}
#' 				\item{\code{simp$sim$regionalisation}}
#' 				\item{\code{simp$sim$runids}}
#' 				\item{\code{simp$sim$regions}}}
#' @param datatype 
#' @param dataname 
#' @param order 
#' @param considertick 
#' @return 
#' 
#' @author Sascha Holzhauer
input_tools_constructFilenameList <- function(simp, datatype = NULL, dataname = NULL, 
		order = simp$sim$filepartorder, considertick = TRUE) {
	vectors <- list("scenario" = simp$sim$scenario,
				"regionalisation" = simp$sim$regionalisation, 	
				"runid" 	= simp$sim$runids,
				"regions"	= simp$sim$regions,
				"datatype"	= datatype,
				"dataname"	= dataname,
				"tick"		= "TICK",
				"D"			= "-"	,
				"U"			= "_")
	
	if (!considertick & "tick" %in% order) {
		order <- order[-(c(-1,0) + (which(order == "tick")))]
	}
	
	l <- vectors[order]
	
	if (any(unlist(lapply(l, is.null)))) {
		futile.logger::flog.warn("simp$sim$filepartorder contains an element which is not defined (%s).
				Removing that element and preceeding one (assuming it's a separator).",
				order[which(unlist(lapply(l, is.null))==TRUE)],
				name="craftyr.input.tools")
		
		l <- l[-c(which(unlist(lapply(l, is.null))==TRUE), which(unlist(lapply(l, is.null))==TRUE) - 1)]
	}
	if (length(l) == 0) {
		R.oo::throw.default("Cannot construct filenamelist. Something's wrong with simp$sim$filepartorder (",
				paste(simp$sim$filepartorder, collapse=","), ")\n")
	}
	l
}
#' Determines the model input filenames for the given simp and datatype, dataname, ticks settings
#' 
#' @inheritParams input_tools_getModelOutputFilenames
#' @return  list of vector of filenames (list elements rerpesent files of one folder)
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputFilenames <- function(simp, folders = simp$dirs$param$getparamdir(simp, datatype),
		datatype = NULL,
		dataname = NULL,
		extension = NULL,
		pertick = FALSE,
		starttick = NULL,
		endtick = NULL, 
		tickinterval = NULL,
		returnfileinfo = TRUE) {
	input_tools_getModelOutputFilenames(simp = simp, folders = folders, datatype = datatype, dataname = dataname,
			extension = extension, pertick = pertick, starttick = starttick, endtick = endtick, 
			tickinterval = tickinterval, returnfileinfo = returnfileinfo)
}
#' Wrapper for save
#' 
#' @param simp 
#' @param object object name as character
#' 
#' @family storing data
#' @author Sascha Holzhauer
#' @export
input_tools_save <- function(simp, object) {
	futile.logger::flog.info("Saving object %s to %s",
				object,
				paste(simp$dirs$output$rdata, simp$sim$id, "/", object, "_", 
					if(is.null(simp$sim$id)) simp$sim$version else simp$sim$id, ".RData", sep=""),
				name = "craftyr.input.tools")
	
	shbasic::sh.ensurePath(paste(simp$dirs$output$rdata, simp$sim$id, "/", sep=""))
	save(list=object, file = paste(simp$dirs$output$rdata, simp$sim$id, "/", object, "_", 
					if(is.null(simp$sim$id)) simp$sim$version else simp$sim$id, ".RData", sep=""), 
			envir = parent.frame())
}
#' Checks whether an object with the given object name has been stored under the given simp configuration
#' 
#' @param simp 
#' @param objectName 
#' @return TRUE if object already stored
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_checkexists <- function(simp, objectName) {
	return(file.exists(paste(simp$dirs$output$rdata, simp$sim$id, "/", objectName, "_", 
							if(is.null(simp$sim$id)) simp$sim$version else simp$sim$id, ".RData", sep="")))
}
#' Wrapper for load
#' 
#' @param simp 
#' \itemize{
#' 	\item \code{simp$sim$id}
#'  \item \code{simp$dirs$output$rdata}
#' 	\item \code{simp$sim$version} (if \code{is.null(simp$sim$id)})
#' }
#' @param objectName 
#' 
#' @family storing data
#' @author Sascha Holzhauer
#' @export
input_tools_load <- function(simp, objectName, ...) {
	
	futile.logger::flog.info("Loading object %s from %s",
			objectName,
			paste(simp$dirs$output$rdata, simp$sim$id, "/", objectName, "_", 
					if(is.null(simp$sim$id)) simp$sim$version else simp$sim$id, ".RData", sep=""),
			name = "craftyr.input.tools")
	
	load(file = paste(simp$dirs$output$rdata, simp$sim$id, "/", objectName, "_", 
					if(is.null(simp$sim$id)) simp$sim$version else simp$sim$id, ".RData", sep=""),
			envir = parent.frame(), ...)
}
#' Construct a list of SimP for given runs
#' 
#' @param runs first part of runid
#' @param randomseed second part of runid
#' @return list of SimPs
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_buildsimplist <- function(runs, randomseed = 0) {
	simps <- list()
	for (run in runs) {
		anothersimp <- simp
		anothersimp$sim$runids 	<- c(paste(run, "-", randomseed, sep=""))		# run to deal with
		anothersimp$sim$id		<- paste(run, "-", randomseed, sep="")			# ID to identify specific data collections (e.g. regions)
		anothersimp$sim$task	<- paste(run, "-", randomseed, sep="")			# Name of surounding folder, usually a description of task 
		anothersimp$sim$shortid	<- run
		simps <- append(simps, list(anothersimp))
	}
	simps
}
#' Rerieves param value for a param expression.
#' Consults Links.csv and Runs.csv as appropriate (recursively).
#' 
#' @param simp 
#' @param rawvalue  
#' @return parameter value
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getParamValue <- function(simp, rawvalue) {
	# TODO consider pre- and postfixes!
	rawvalue = as.character(rawvalue)
	if (grepl("@@", rawvalue)) {
		paramname =  stringr::str_trim(strsplit(rawvalue, split="[;\\)]")[[1]][2]) #  grep(",", elementname, value=F)
		value =  input_tools_getParamValue(simp, input_tools_getLinksValue(simp, id=paramname))
	} else if (grepl("@", rawvalue)) {
		rundata <- rundata <- input_tools_getrundata(simp) 
		paramname =  stringr::str_trim(strsplit(rawvalue, split="[,\\)]")[[1]][2]) #  grep(",", elementname, value=F)
		if (!paramname %in% colnames(rundata)) {
			R.oo::throw.default("Rundata does not contain column ", paramname, "!")
		}
		value =  input_tools_getParamValue(simp, rundata[, paramname])
	} else {
		value = rawvalue
	} 
	return(value)
}
#' Read a value from Links.cav according to simp and the given ID.
#' @param simp 
#' @param id  
#' @return value in Links.csv
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getLinksValue <- function(simp, id = "CapitalFolder") {
	# TODO consider pre- and postfixes!
	linksdata <- read.csv(file=paste(simp$dirs$output$data, "/", simp$sim$folder, "/Links.csv", sep=""))
	if (!id %in% linksdata$ID) {
		R.oo::throw.default("Links.csv does not contain column ", id, "!")
	}
	value <- as.character(linksdata[linksdata$ID == id, "Value"])
	return(value)
}
#' Load, combine, and store data objects
#' 
#' @param simp used for saving
#' @param simps list of simp 
#' @param fromdataname character
#' @param todataname character
#' @return stored RData object
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_loadstorecombined <- function(simp, simps, fromdataname, todataname = fromdataname) {
	combined <- data.frame()
	for (s in simps) {
		input_tools_load(s, objectName=fromdataname)
		combined <- rbind(combined, get(fromdataname))
	}
	assign(todataname, combined)
	input_tools_save(simp, todataname)
}
#' Get parameters in Runs.csv for the defined Run ID (in simp)
#' @param simp  
#' @return data.frame of parameters in Runs.csv for the defined Run ID.
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getrundata <-  function(simp) {
	runid = as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]})
	
	rundata <- read.csv(file=paste(simp$dirs$data, simp$sim$folder, "Runs.csv", sep="/"))
	rundata <- rundata[rundata$run == runid,]
	return(rundata)
}