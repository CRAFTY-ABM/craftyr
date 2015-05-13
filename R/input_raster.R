#' Concenate the filename from basedir, scenario, world, run_id, year as well as datatype and dataname.
#' Region, datatype, and dataname are only considered if given as parameters.
#' 
#' @param scenario
#' @param world
#' @param run_id
#' @param year
#' @param basedir
#' @param datatype (e.g. "Capital")
#' @param dataname (e.g. "Cap1")
#' @param useregiondir (output data may or may not be stored in region specific folders)
#' @return String of filename
#'
#' @author Calum Brown
#' @author Sascha Holzhauer
#' @export
read_raster_getMainFilename <- function(basedir, scenario, world, run_id, region = NULL,
		dataType = NULL, dataName = NULL, year, useregiondir = FALSE) {
	return(paste(read_getOutputDir(scenario, world, run_id, basedir, region = if(useregiondir) region else NULL),
					"/", scenario,"-",run_id,"-", region, if(!is.null(region)) "-", dataType, if(!is.null(dataType)) "-",
					dataName, if(!is.null(dataName)) "-", year,".asc",sep=""))
}
#' Read a single raster file for the given scenario/world/run_id/year combination in the given base dir.
#' Data type and data name a required to construct the filename properly.
#' 
#' DEPRECATED!
#' 
#' @param scenario
#' @param world
#' @param basedir
#' @param run_id
#' @param year
#' @param dataType (e.g. "Capital")
#' @param dataName (e.g. "Cap1")
#' @param useRegionDir (output data may or may not be stored in region specific folders)
#' @return rasterData
#'
#' @author Sascha Holzhauer
#' @export
read_raster_single <- function(scenario = lix$scenario, world = pix$world,
		basedir =  pix$path_names["dataSource"][[1]], run_id = lix$run_id,
		year = lix$year, region = NULL, dataType = NULL, dataName = NULL, useRegionDir = FALSE) {
	
	if (is.null(year)) {
		R.oo::throw.default("No parameter year given and lix$year not set!")
	}
	
	
	fn <- read_raster_getMainFilename(basedir=basedir, scenario=scenario, world=world, run_id=run_id,
			region=region, dataType=dataType, dataName=dataName, year=year, useRegionDir=useRegionDir)
	
	print(paste("Read raster file ", fn, sep=""))
	
	if(!file.exists(fn)) {
		R.oo::throw.default(paste("File ", fn, "\n does not exist!\nCheck basedir, scenario, world, runid and year!", sep=""))
	}
	
	r <- raster(fn)
	# r[is.na(r)] <- 0.0
	r
}
#' Get list (scenarios/regions/runids) of lists (ticks) of raster data from files whose source is taken from simp.
#' 
#' @param simp SIMulation Properties
#' @param datatype category of raster data (e.g. "Capital")
#' @param dataname name of data of category (e.g. "Cap1") 
#' @param starttick first tick to consider 
#' @param endtick  last tick to consider
#' @param tickinterval Note: tickinterval is based on starttick which defaults to 0!
#' @return list of lists of raster data
#' 
#' @author Sascha Holzhauer
#' @export
input_raster_output <- function(simp,
		datatype = NULL ,
		dataname, 
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick,
		tickinterval = 1,
		encapsefileinfo = TRUE) {
	
	files <- input_tools_getModelOutputFilenames(simp = simp, 
			datatype = datatype,
			dataname = dataname,
			extension = "asc",
			pertick = TRUE,
			starttick = starttick,
			endtick = endtick, 
			tickinterval = tickinterval)

	listnames <- names(files)
	mapply(function(fileinfovector, name) {
				futile.logger::flog.debug("Handle directory %s...\n",
						name,
						name="crafy.input.raster")
				
				rasters <- apply(fileinfovector, MARGIN=1, function(fileinfo) {
							futile.logger::flog.debug("Read year %s...\n",
									fileinfo["Tick"],
									name="crafy.input.raster")
						
							r <- raster::raster(fileinfo["Filename"])
							data <- raster::values(r)
							data[is.na(data)] <- 0.0
							r
						})
				if (encapsefileinfo) {
					fileinfovector$Raster <- rasters
					result = fileinfovector
				} else {
					result = rasters
				}
				result
			}, files, listnames, SIMPLIFY = FALSE)
}
#' Read a number of raster files and combine them in a list of data.
#' @param scenario
#' @param world
#' @param basedir
#' @param run_id
#' @param startYear
#' @param endYear
#' @param everyYear
#' @param region
#' @param dataType
#' @param dataName
#' @param useRegionDir (output data may or may not be stored in region specific folders)
#' @return list of data over years
#'
#' @author Sascha Holzhauer
#' @export
readRasterDataYears <- function(scenario = lix$scenario, world = pix$world,
		basedir =  pix$path_names["dataSource"][[1]], run_id = lix$run_id,
		startYear = lix$startTick, endYear = lix$endTick, everyYear = lix$everyTick, region = lix$region,
		dataType = NULL, dataName = "Agent-SerialID", useRegionDir = FALSE) {
	rasterData <- list()
	years <- c()
	for (i in seq(startYear, endYear, by=everyYear)) {
		
		rasterData <- c(rasterData, setNames(read_raster_single(
								scenario = scenario,
								world =    world,
								basedir =  basedir,
								run_id =  run_id,
								year = i,
								region = region,
								dataType = dataType,
								dataName = dataName,
								useRegionDir = useRegionDir),i))
		
#		rasterData[[as.character(i)]] <- read_raster_single(
#				scenario = scenario,
#				world =    world,
#				basedir =  basedir,
#				run_id =  run_id,
#				year = i,
#				region = region,
#				dataType = dataType,
#				dataName = dataName,
#				useRegionDir = useRegionDir)
		years <- c(years, i)
	}
	rasterData
}
#' Read raster
#' @param scenario
#' @param world
#' @param basedir
#' @param region
#' @param num_runs the number of runs to consider
#' @param dataType
#' @param dataName
#' @param runIDrange
#' @param useRegionDir (output data may or may not be stored in region specific folders)
#' @param func
#' @param startyear
#' @param endYear
#' @param everyYear
#' @return data.frame
#'
#' @author Sascha Holzhauer
#' @export
readingRasterData_collateRuns <- function(scenario = lix$scenario, world = pix$world,
		basedir = pix$path_names["dataSource"][[1]], region = NULL,
		num_runs = NULL, dataType, dataName, aggregate = FALSE, runIDrange = NULL, useRegionDir = FALSE,
		func = NULL, startYear = NULL, endYear = NULL, everyYear = NULL)
{
	output <- NULL
	runs <- readingData_getRunIDs(scenario, world, basedir, runIDrange)
	if (length(runs) == 0) {
		throw(paste("No runs in ", basedir, "/", scenario, "-", world, sep=""))
	}
	if (is.null(num_runs)) num_runs = length(runs)
	ids<-runs[1:num_runs]
	
	id = runs[1]
	for( id in runs)
	{
		print(paste("Reading run: ",id))
		data <- collateYearsRaster(scenario, world, id, basedir, region, useRegionDir, dataType, dataName, aggregate, func = func,
				startYear, endYear, everyYear)
		output <- rbind( output, data)
	}
	output
}

#' Do title
#' @param scenario
#' @param world
#' @param basedir
#' @param region
#' @param num_runs
#' @param dataTypes vector of dataTypes
#' @param dataNames according vector of dataName with same length as dataType
#' @param aggregate
#' @param runIDrange
#' @param useRegionDir (output data may or may not be stored in region specific folders)
#' @param func
#' @param startyear
#' @param endYear
#' @param everyYear
#' @return data.frame
#'
#' @author Sascha Holzhauer
#' @export
readingRasterData_collateRunsAndMeasures <- function(scenario = lix$scenario, world = pix$world,
		basedir = pix$path_names["dataSource"][[1]], region = NULL,
		num_runs = NULL, dataTypes, dataNames, aggregate = TRUE, runIDrange = NULL, useRegionDir = FALSE,
		funcs = NULL, startYear = lix$startTick, endYear = lix$endTick, everyYear = lix$everyTick) {
	r <- data.frame()
	
	for (i in 1:length(dataTypes)) {
		data <- readingRasterData_collateRuns(scenario=scenario, world=world, basedir= basedir,
				num_runs=num_runs, region = region, useRegionDir = useRegionDir, dataType = dataTypes[i],
				dataName = dataNames[i], aggregate=aggregate,
				runIDrange = runIDrange, if (!is.null(funcs)) funcs[i], startYear, endYear, everyYear)
		data$Measure <- paste(dataTypes[i],".", dataNames[i], if(paste(dataTypes[i],dataNames[i],sep=".") %in%
						r$Measure) paste(".",i,sep=""), sep="")
		r <- rbind(r, data)
	}
	r
}