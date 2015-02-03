#' Determines the model input folder for the given datadir
#' 
#' @param simp SIMulation Properties
#' @param datatype one of c("capitals", "demand")
#' @return model output directory as string 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputDir <- function(simp, datatype) {
	if (is.null(datatype)) {
		R.oo::throw.default("Parameter 'datatype' may not be null!")
	}
	return <- paste(simp$dirs$data,
					if (datatype %in% c("capitals", "demand")) {
						paste("worlds", simp$sim$worldname,
							if(!is.null(simp$sim$regionalisation)) paste("regionalisations", 
									simp$sim$regionalisation, sep="/"), sep="/")
					},
					if (datatype %in% c("allocation")) {
						"allocation"
					},
					sep="/")
}
#' Determines the model output folder(s) for the given \eqn{simp}.
#' If one of the parameters \eqn{simp$sim$version}, \eqn{simp$sim$world}, 
#' \eqn{simp$sim$regionalisation}, \eqn{simp$sim$scenario}, \eqn{simp$sim$runid},
#' or \eqn{simp$sim$regions} is a vector a vector of directories is returned.
#' @param simp SIMulation Properties
#' @return String or vector of output folder(s)
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelOutputDir <- function(simp) {
	result = do.call(paste, c(expand.grid(simp$dirs$outputdir,
							simp$sim$version,
							simp$sim$world,
							if(!is.null(simp$sim$regionalisation)) simp$sim$regionalisation,
							simp$sim$scenario,
							simp$sim$runid,
							if(!is.null(simp$sim$regions)) simp$sim$regions), sep="/"))
	result
}
#' Determine a vector of ticks for the files in the given directory that match the given pattern.
#' If pattern is not specified it is assumed that all requested files in the directory are of the form 
#' *<scenario>-<runid>-<region>-<datatype>-<dataname>-<tick>.<extension>.
#' If extension is not given, any extension is considered. If datatype and dataname are 
#' not given, the according parts of the filename is assumed not to exist (also the associated '-').
#' 
#' @inheritParams input_tools_getModelOutputDir
#' @param dir directory to search in
#' @param pattern pattern for matching considered files excluding the tick and extension. If NULL, the default is used (\\.*<scenario>-<runid>-<region>-<datatype>-<dataname>-).
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
		starttick = 0,
		endtick = simp$tech$maxtick, 
		tickinterval = 1) {
	# pattern = NULL
	if (is.null(pattern)) pattern <- paste(".*",
			simp$sim$scenario, "-",
			simp$sim$runid, "-",
			simp$sim$regions, "-",
			datatype, if(!is.null(datatype)) "-",
			dataname, if(!is.null(dataname)) "-", sep="")

	files <- grep(paste(pattern, "\\d*\\.", if (!is.null(extension)) extension else "*", sep=""),
			list.files(path=dir),value=T)
	ticks <- as.numeric(sub(paste("\\.", if (!is.null(extension)) extension else "*", sep=""), "", 
					sub(pattern, "", files)))
		
	if (length(ticks) == 0) {
		R.oo::throw.default(paste("No ticks found in ", dir, "\nfor pattern '", pattern, paste("\\d*\\.", if (!is.null(extension)) 
											extension else "*", sep=""), "' (available files:\n", 
						paste(list.files(path=dir), collapse = "\n"), ")", sep=""))
	}
	filteredTicks <- ticks[ticks %in% seq(from=starttick, to=endtick, by=tickinterval)]
	
	if (!is.null(simp$debug$input) & simp$debug$input > 0) {
		cat("Available ticks:", ticks, "\n", sep=" ")
		cat("Requested ticks:", seq(from=starttick, to=endtick, by=tickinterval), "\n", sep=" ")
	}
	if (length(ticks) == 0) {
		R.oo::throw.default("None of found ticks matched parameters starttick (", starttick, "), endtick (", enttick, 
				") and interval (", tickinterval, "), \nAvailable ticks: ", ticks)
	}
	filteredTicks
}
#' Determines the model output filenames for the given simp and datatype, dataname, ticks settings.
#' Able to read multiple scenarios/regions/runids.
#' Attaches infos (region, runid, scenario).
#' 
#' @inheritParams input_tools_getAvailableTicks
#' @param folders 
#' @param pertick if TRUE the filename will be complemented by all available ticks
#' @return vector of filenames
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
		starttick = 0,
		endtick = simp$tech$maxtick, 
		tickinterval = 1) {
	
	
	# generate file info matrix by combining various vectors:
	fileinfogrid <- c(expand.grid(				
					Scenario = simp$sim$scenario,
					Runid = simp$sim$runid,
					Region = simp$sim$regions))
	
	# attach filename:
	# Folder = filename need to be concatenated here because of lapply when pertick == FALSE (difficulties for vector of folders)
	fileinfogrid <- data.frame(fileinfogrid, Filename = paste(folders, do.call(paste, c(expand.grid(				
									simp$sim$scenario, "-",
									simp$sim$runid, "-",
									simp$sim$regions, "-",
									if (!is.null(datatype)) paste(datatype, "-", sep="") else "",
									dataname, stringsAsFactors = FALSE), sep="")), sep="/"))
	
	if (pertick) {
		fileinfogrid <- mapply(function(foldername, scenario, runid, region, filename)
										expand.grid(
											Scenario = scenario,
											Runid = runid,
											Region = region,
											Tick = input_tools_getAvailableTicks(simp = simp,
													dir = foldername, 
													pattern = paste(substr(filename, gregexpr(pattern ='/', 
																	filename)[[1]][length(gregexpr(pattern ='/', filename)[[1]])] + 1, nchar(as.character(filename[1]))), "-", sep=""),
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
					infogrid$Filename = paste(infogrid$Filename, "-", infogrid$Tick, ".", extension, sep="")
					infogrid},
				simplify = FALSE)
	} else {
		# attach filename:
		fileinfogrid <- as.data.frame(sapply(list(fileinfogrid), function(infogrid) {
					infogrid$Filename = paste(infogrid$Filename, ".", extension, sep="")
					infogrid},
							simplify = FALSE))
	}
	
	lapply(fileinfogrid$filename, shbasic::sh.checkFilename)
	
	if (returnfileinfo) {
		return = fileinfogrid
	} else {
		return = fileinfogrid$Filename
	}
	return
}
#' Determines the model input filenames for the given simp and datatype, dataname, ticks settings.
#' @inheritParams input_tools_getModelOutputFilenames
#' @return vector of filenames
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputFilenames <- function(simp, folders = input_tools_getModelInputDir(simp, datatype),
		datatype = NULL,
		dataname = NULL,
		extension = NULL,
		pertick = FALSE,
		starttick = NULL,
		endtick = NULL, 
		tickinterval = NULL) {
	input_tools_getModelOutputFilenames(simp, folders, datatype, dataname,	extension,
			pertick, starttick, endtick, tickinterval)
}
#' Wrapper for save
#' @param simp 
#' @param object 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_save <- function(simp, object) {
	if (simp$debug$input > 0) {
		cat("Saving object", object, "to",
				paste(simp$dirs$output$rdata, simp$sim$id, "/", object, "_", 
						simp$sim$id, ".RData", sep=""), "\n")
	}
	shbasic::sh.ensurePath(paste(simp$dirs$output$rdata, simp$sim$id, "/", sep=""))
	save(list=object, file = paste(simp$dirs$output$rdata, simp$sim$id, "/", object, "_", 
					simp$sim$id, ".RData", sep=""), envir = parent.frame())
}
#' #' Wrapper for load
#' @param simp 
#' @param objectName 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_load <- function(simp, objectName,...) {
	if (simp$debug$input > 0) {
		cat("Loading object", objectName, "from",
				paste(simp$dirs$output$rdata, simp$sim$id, "/", objectName, "_", 
						simp$sim$id, ".RData", sep=""), "\n")
	}
	load(file = paste(simp$dirs$output$rdata, simp$sim$id, "/", objectName, "_", 
					simp$sim$id, ".RData", sep=""), envir = parent.frame(), ...)
}