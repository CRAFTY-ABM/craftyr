#' Determines the model input folder for the given datadir
#' @param simp SIMulation Properties
#' @param dataType one of c("capitals", "demand")
#' @return model output directory as string 
#' 
#' @author Sascha Holzhauer
#' @export
input_tools_getModelInputDir <- function(simp, datatype) {
	if (is.null(datatype)) {
		throw("Parameter 'datatype' may not be null!")
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