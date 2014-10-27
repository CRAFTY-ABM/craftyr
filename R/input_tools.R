#' Determines the model input folder for the given datadir
#' @param simp SIMulation Properties
#' @param dataType one of c("capitals", "demand")
#' @return model output directory as string 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputDir <- function(simp, datatype) {
	if (is.null(datatype)) {
		R.oo::throw("Parameter 'datatype' may not be null!")
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
#' Determines the model output folder for the given simp
#' @param simp SIMulation Properties
#' @return String of output folder
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelOutputDir <- function(simp) {
	result = paste(	simp$dirs$outputdir,
					simp$sim$version,
					simp$sim$world,
					if(!is.null(simp$sim$regionalisation)) simp$sim$regionalisation,
					simp$sim$scenario,
					simp$sim$runid,
					if(!is.null(sim$sim$region)) simp$sim$region, sep="/")
}
#' Determine a vector of ticks for which the requested data (of type dataType and name dataName)
#' is available in dir. Assumes that all files in dir of the form *<datatype>-<dataname>-<tick>.<extension>
#' are requested. If extension is not given, any extension is considered. If datatype and dataname are 
#' not given, the according parts of the filename is assumed not to exist (also the associated '-').
#' @param dir directory to search in
#' @param datatype (e.g. "Capital")
#' @param dataname (e.g. "Cap1")
#' @param extension used to restrict filename extension
#' @param starttick start tick of requested range of ticks
#' @param endtick end tick of requested range of ticks
#' @param tickinterval interval of requested range of ticks
#' @return vector of ticks
#' @author Sascha Holzhauer
#' @export
input_tools_getAvailableTicks <- function(dir, 
		datatype = NULL,
		dataname = NULL,
		extension = NULL,
		starttick = NULL,
		endtick = NULL, 
		tickinterval = NULL) {
	
	pattern <- paste(".*",
			datatype, if(!is.null(datatype)) "-",
			dataname, if(!is.null(dataname)) "-", sep="")
	ticks <- as.numeric(sub(paste(".", if (!is.null(extension)) extension else "*", sep=""), "", sub(pattern, 
							"", grep(paste(datatype, "-", dataname, sep=""),
									list.files(path=dir),value=T))))
	if (length(ticks) == 0) {
		R.oo::throw(paste("No ticks found in ", dir, "\nfor pattern '", pattern,paste(".", if (!is.null(extension)) 
											extension else "*", sep=""), "' (available files:\n", 
						paste(list.files(path=dir), collapse = "\n"), ")", sep=""))
	}
	filteredTicks <- ticks[ticks %in% seq(from=starttick, to=endtick, by=tickinterval)]
	if (length(ticks) == 0) {
		R.oo::throw("None of found ticks matched parameters starttick (", starttick, "), endtick (", enttick, 
				") and interval (", tickinterval, "), \nAvailable ticks: ", ticks)
	}
	filteredTicks
}