#' Reads capital levels for the specified capitals from CSV data for potentially multiple regions
#' @param simp SIMulation Properties
#' @param capitals vector of strings of requested capital levels
#' @return list of data.frames containing capital levels  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_capitals <- function(simp, capitals = simp$mdata$capitals) {
	filenames <- input_tools_getModelOutputFilenames(simp, datatype="Capitals", 
			folders = simp$dirs$param$getparamdir(simp, datatype="capitals"),
			pertick = FALSE, extension = "csv", returnfileinfo = FALSE)
	
	lapply(filenames, shbasic::sh.checkFilename)
	capitalData <- lapply(filenames, utils::read.csv)
	capitalData <- lapply(capitalData, function(x) x[, c(simp$csv$cname_x, simp$csv$cname_y, capitals)])
}
#' Reads aft allocation from CSV data for potentially multiple regions
#' @param simp SIMulation Properties
#' @param aftColumn header of AFT column
#' @return list of data.frames containing aft allocation (And x and y coordinates)  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_initialAllocation <- function(simp, aftColumn = "LandUseIndex", 
		filename = paste(simp$sim$regions, "-Cell.csv", sep="")) {
	filenames <- paste(simp$dirs$alloc, '/',
			if(!is.null(simp$sim$allocversion)) paste(simp$sim$allocversion, "/", sep=""),
			simp$sim$worldname, "/",
			if(!is.null(simp$sim$regionalisation)) paste(simp$sim$regionalisation, "/", sep=""),
			filename,
			sep="")
	lapply(filenames, shbasic::sh.checkFilename)
	capitalData <- lapply(filenames, utils::read.csv)
	capitalData <- lapply(capitalData, function(x) x[, c(simp$csv$cname_x, simp$csv$cname_y, aftColumn)])
}
#' Reads demand values from CSV data for potentially multiple regions
#' @param simp SIMulation Properties
#' @return list of data.frames containing demand values, and the filename of their origin  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_demand <- function(simp) {
	filenames <- paste(input_tools_getModelInputDir(simp, datatype="demand"), '/',
			if(!is.null(simp$sim$regionalisation)) paste(simp$sim$regionalisation, "_", sep=""),  
			simp$sim$scenario, "_",
			simp$sim$regions, "_",
			"Demand.csv", sep="")
	
	filenames = do.call(paste, c(input_tools_getModelInputDir(simp, datatype="demand"), '/', 
					expand.grid(input_tools_constructFilenameList(simp, datatype = "demands",,
									order = simp$sim$filepartorder_demands), stringsAsFactors = FALSE), ".csv", sep=""))
	futile.logger::flog.debug("Filenames for collecting demand: %s",
			paste(filenames, collapse="\n "),
			name="crafty.input.csv.param")
	
	lapply(filenames, shbasic::sh.checkFilename)
	demandData <- lapply(filenames, function(filename) {
				result = utils::read.csv(filename)
				result$filename = filename
				result
			})
}
#' Reads productivities for each capital and each service for the given aft
#' @param simp SIMulation Properties
#' @param aft
#' @return data.frame containing productivities  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_productivities <- function(simp, aft, filenameprefix = "AftProduction_",
		filenamepostfix = "_multi_medium") {
	filename <- paste(simp$dirs$param$getparamdir(simp, datatype="productivities"), "/", aft, 
			"/", filenameprefix, aft, filenamepostfix, ".csv", sep="")
	
	shbasic::sh.checkFilename(filename)
	capitalData <- utils::read.csv(filename)
	capitalData
}
#' Read agent specifc parameters
#' @param simp 
#' @param aft 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @return agent param data
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_agents <- function(simp, aft, filenameprefix = "AftParams_",
		filenamepostfix = "") {
	filename <- paste(simp$dirs$param$getparamdir(simp, datatype="agentparams"), 
			"/", filenameprefix, aft, filenamepostfix, ".csv", sep="")
	
	shbasic::sh.checkFilename(filename)
	paramData <- utils::read.csv(filename)
	paramData
}