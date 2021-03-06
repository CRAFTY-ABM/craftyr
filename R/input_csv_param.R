#' Read capital data from input CSV data
#' 
#' Reads capital levels for the specified capitals from CSV data for potentially multiple regions
#' 
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
	
	futile.logger::flog.info("Read capital data from %s (only first filename shown)...",
				filenames[[1]],
				name = "craftyr.input_csv_param.R")
		
	lapply(filenames, shbasic::sh.checkFilename)
	capitalData <- lapply(filenames, utils::read.csv)
	if (any(!c(simp$csv$cname_x, simp$csv$cname_y, capitals) %in% colnames(capitalData[[1]]))) {
		requested <- c(simp$csv$cname_x, simp$csv$cname_y, capitals)
		R.oo::throw.default("Requested columns (" , 
				paste(requested[!requested %in% colnames(capitalData[[1]])], collapse=","), ") not in data (",
				paste(colnames(capitalData[[1]]), collapse=","), ")!")
	}
	capitalData <- lapply(capitalData, function(x) x[, c(simp$csv$cname_x, simp$csv$cname_y, capitals)])
}
#' Determines number of cells from CSV data for potentially multiple regions
#' 
#' TODO apply simp$sim$filepartordercapitals to extract region instead of regionpartfromend and regionpartdevider
#' 
#' @param simp SIMulation Properties
#' @param regionpartfromend index of region part counted from end of filename (used to extract region from filename)
#' @param regionpartdevider character that separated the region filename parts (used to extract region from filename)
#' @return data.frame with columns Region and Cells
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_capitals_cellnumbers <- function(simp,
		regionpartfromend = 2, regionpartdevider = "_") {
	
	cellnum <- shbasic::sh_tools_loadorsave(SIP = simp, 
			OBJECTNAME = paste("cellNum", simp$sim$world, simp$sim$regionalisation, sep="-"),
			PRODUCTIONFUN = function(simp, regionpartfromend, regionpartdevider) {
	
		filepartorderstored <- simp$sim$filepartorder
		simp$sim$filepartorder	<- simp$sim$filepartordercapitals
		filenames <- input_tools_getModelInputFilenames(simp, datatype="Capitals", 
				folders = simp$dirs$param$getparamdir(simp, datatype="capitals"),
				pertick = FALSE, extension = "csv", returnfileinfo = FALSE)
		simp$sim$filepartorder <- filepartorderstored
		
		lapply(filenames, shbasic::sh.checkFilename)
		cellnums <- lapply(filenames, function(file) {
					data <- utils::read.csv(file)
					df <- data.frame(
						Region =  strsplit(file, paste("[/", regionpartdevider,"]"))[[1]][length(strsplit(file, 
												paste("[/", regionpartdevider,"]"))[[1]])- regionpartfromend + 1], 
						Cells  =  nrow(data))
				})
		cellnums <-  do.call(rbind, cellnums)
	},
	simp = simp, 
	regionpartfromend = regionpartfromend,
	regionpartdevider = regionpartdevider)

}
#' Assign regions to slots for parallel computing
#' 
#' @param simp 
#' @param capitals 
#' @param numslots
#' @param regionpartfromend 
#' @param regionpartdevider  
#' @return list of groups of regions 
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_capitals_slotassignment <- function(simp, capitals = simp$mdata$capitals, numslots = 16,
		regionpartfromend = 2, regionpartdevider = "_") {
	
	cellnumdf <- input_csv_param_capitals_cellnumbers(simp = simp, capitals = capitals, 
			regionpartfromend = regionpartfromend, regionpartdevider = regionpartdevider)
	
	cellnums <- setNames(cellnumdf$Cells, cellnumdf$Region)
	assign <- cellnums[order(cellnums, decreasing = TRUE)]
	assignments <- seq(1,length(assign))
	names(assignments) <- names(assign)
	
	assignments[(numslots + 1):length(assignments)] <- numslots : (numslots - (length(assignments)-(numslots + 1)))
	
	groups <- split(assign, assignments)
	grp <- mapply(c, groups, lapply(groups, sum), SIMPLIFY=FALSE)
	
	# check whether all cluster sums are below the largest region
	if (any(lapply(groups, sum) > assign[1])) {
		warning("A group of regions is larger than the largest single region!")
	}
	grp
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
#' @param simp SIMulation Properties. Considered elements:\itemize{
#' 				\item{\code{simp$sim$filepartorder}}
#' 				\item{\code{simp$sim$scenario}}
#' 				\item{\code{simp$sim$regionalisation}}
#' 				\item{\code{simp$sim$runids}}
#' 				\item{\code{simp$sim$regions}}
#' 				\item{\code{simp$dirs$param$getparamdir}}
#' 				\item{\code{simp$sim$filepartorder_demands}}}
#' @return list of data.frames containing demand values, and the filename of their origin  
#'
#' @author Sascha Holzhauer
#' @export
input_csv_param_demand <- function(simp) {
	if(is.null(simp$sim$filepartorder_demands)) {
		R.oo::throw.default("simp$sim$filepartorder_demands is NULL!")
	}
	filenames = do.call(paste, c(simp$dirs$param$getparamdir(simp, datatype="demands"), '/', 
					expand.grid(input_tools_constructFilenameList(simp, datatype = "demands",,
									order = simp$sim$filepartorder_demands), stringsAsFactors = FALSE), ".csv", sep=""))
	futile.logger::flog.debug("Filenames for collecting demand: %s",
			paste(filenames, collapse="\n "),
			name="craftyr.input.csv.param")
	
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
input_csv_param_productivities <- function(simp, aft, filenameprefix = NULL,
		filenamepostfix = NULL, filenameprefix_aftparams = "AftParams_",
		filenamepostfix_aftparams = "", aftwisefolder = TRUE, servicesasrownames = FALSE) {
	# try to retrieve filename from AFT param CSV:
	aftParamIds <- hl_getAgentParamId(simp)
	aftparamdata <- input_csv_param_agents(simp, aft, filenameprefix = filenameprefix_aftparams,
			filenamepostfix = filenamepostfix_aftparams)
	filename <- aftparamdata[match(aftParamIds,aftparamdata$aftParamId), "productionCsvFile"]
	
	filename <- paste(simp$dirs$param$getparamdir(simp), hl_getBaseDirAdaptation(simp), 
			as.character(filename), sep="/")
	
	futile.logger::flog.debug("Agent productivities > Filename retrieved from AFT param CSV: %s",
				filename,
				name = "craftyr.input_csv_param.R")
	if (!file.exists(filename) || !is.null(filenameprefix) || !is.null(filenamepostfix)) {
		filename <- paste(simp$dirs$param$getparamdir(simp, datatype="productivities"), "/", 
				if (aftwisefolder) aft, "/", filenameprefix, aft, filenamepostfix, ".csv", sep="")
	}
	
	shbasic::sh.checkFilename(filename)
	capitalData <- utils::read.csv(filename)
	if (servicesasrownames) {
		rownames(capitalData) <- capitalData[,1]
		capitalData <- capitalData[, -c(1)]
	}
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
#' Read Runs data (Runs.csv)
#' 
#' @param simp 
#' @param paramid if \code{TRUE} return row for run ID in \code{simp$sim$runids[1]} only
#' @return run data
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_param_runs <- function(simp, paramid = FALSE) {
	filename <- paste(simp$dirs$param$getparamdir(simp, datatype="runs"), 
			"/Runs.csv", sep="")
	
	shbasic::sh.checkFilename(filename)
	paramData <- utils::read.csv(filename)
	if (paramid) {
		runid = as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
							simp$sim$runids[1]})
		paramData <- paramData[paramData$run == runid,]
	}
	paramData
}