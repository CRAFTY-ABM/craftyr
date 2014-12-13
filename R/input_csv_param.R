#' Reads capital levels for the specified capitals from CSV data for potentially multiple regions
#' @param simp SIMulation Properties
#' @param capitals vector of strings of requested capital levels
#' @return list of data.frames containing capital levels  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_capitals <- function(simp, capitals = simp$mdata$capitals) {
	filenames <- paste(input_tools_getModelInputDir(simp, "capitals"), '/',
			if(!is.null(simp$sim$regionalisation)) paste(simp$sim$regionalisation, "_", sep=""),  
			simp$sim$scenario, "_",
			simp$sim$regions, "_",
			"Capitals.csv", sep="")
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
input_csv_param_initialAllocation <- function(simp, aftColumn = "LandUseIndex") {
	filenames <- paste(input_tools_getModelInputDir(simp, "allocation"), '/',
			if(!is.null(simp$sim$allocversion)) paste(simp$sim$allocversion, "/", sep=""),
			simp$sim$worldname, "/",
			if(!is.null(simp$sim$regionalisation)) paste(simp$sim$regionalisation, "/", sep=""),
			simp$sim$regions, "-Cell.csv",
			sep="")
	lapply(filenames, shbasic::sh.checkFilename)
	capitalData <- lapply(filenames, utils::read.csv)
	capitalData <- lapply(capitalData, function(x) x[, c(simp$csv$cname_x, simp$csv$cname_y, aftColumn)])
}