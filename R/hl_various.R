#' Reads marginal utilties and prepares the data for plotting
#' 
#' @param simp 
#' @param filename the file that contain marginal utility data. Default is \code{MarginalUtilities.csv} in RData folder.
#' @param returnplot if true the ggplot object is returned
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, simp$sim$id, "Marginal_Utilities.csv", sep="/"),
		storerdata = TRUE, returnplot = FALSE) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape2::melt(utilities, variable.name="Service", 
			id.vars= c("Year"), 
			direction="long")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$Runid <- 0
	
	if (storerdata) {
		input_tools_save(simp, "csv_MarginalUtilitites_melt")
	}
	
	p1 <- visualise_lines(simp = simp, data = csv_MarginalUtilitites_melt, y_column = "value",
			title = "Marginal Utilities",
			colour_column = "Service",
			filename = paste("MarginalUtilities", sep=""),
			alpha=0.7,
			returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Return data in RunInfo table for runid defined in simp.
#' Considers \code{}
#' 
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
hl_getRunInfo <-  function(simp, filename = simp$dirs$output$runinfo) {
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]}) 
	randomseed <- if(grepl('-', simp$sim$runids[1])) as.numeric(strsplit(simp$sim$runids[1], '-')[[1]][2]) else NULL
	
	if (tools::file_ext(filename) == "ods") {
		require(readODS)
		runinfo <- read.ods(filename, sheet = 1)[,1:simp$tech$runinfocolnumber]
		colnames(runinfo) <- runinfo[2,]
		runinfo <- runinfo[-c(1,2),]
		rinfo <- runinfo[runinfo["Version"] == simp$sim$version &
						runinfo["First Run ID"] == paramid & (if(!is.null(randomseed))
								(if ("Random Seed" %in% colnames(runinfo))
									runinfo["Random Seed"] == randomseed  else
									runinfo["First Random Seed"] <= randomseed & runinfo["Last Random Seed"] >= randomseed)
										else TRUE), ]
		
		if (length(rinfo[,1]) == 0) {
			rinfo <- runinfo[runinfo["Version"] == simp$sim$version & runinfo["First Run ID"] <= paramid &
							runinfo["Last Run ID"] >= paramid & (is.null(randomseed) || (if ("Random Seed" %in% colnames(runinfo))
										runinfo["Random Seed"] == randomseed  else
										runinfo["First Random Seed"] <= randomseed & runinfo["Last Random Seed"] >= randomseed)), ]
		}
	} else if(tools::file_ext(filename) == "csv") {
		runinfo <- read.csv(filename, skip = 1)
		rinfo <- runinfo[runinfo$Version == simp$sim$version,]
	} else {
		R.oo::throw.default("File extension ", tools::file_ext(filename)," not supported!")
	}
	
	if (length(rinfo[,1]) == 0) {
		R.oo::throw.default("Runinfo table ", filename," does not contain a row for version ", 
				simp$sim$version, " and paramID ", paramid, " (random seed: ", randomseed, ")!", sep="")
	}
	return(rinfo)
}
#' Generates AFT key as CSV with columns 'Index' and 'LandUse'
#' 
#' @param simp  
#' \itemize{
#' 	\item \code{simp$mdata$aftNames}
#'  \item \code{simp$dirs$output$data}
#' }
#' 
#' @return csv file
#' 
#' @author Sascha Holzhauer
#' @export
hl_landindiceskey_csv <- function(simp) {
	filename <- sprintf("%s/LandUseIndicesKey.csv",
			simp$dirs$output$data)
	data <- data.frame("Index" = names(simp$mdata$aftNames), "LandUse" = simp$mdata$aftNames)
	shbasic::sh.ensurePath(filename, stripFilename = TRUE)
	write.csv(data, file = filename, row.names = FALSE)
}
#' Fetch agent param ID from Runs.csv for paramId in given simp
#' 
#' @param simp 
#' @param agentParamColumn 
#' @param runIdColumn 
#' @return agent Param ID
#' 
#' @author Sascha Holzhauer
#' @export
hl_getAgentParamId <- function(simp, agentParamColumn = "aftParamId", runIdColumn="run") {
	futile.logger::flog.debug("Fetch agent param ID",
				name = "craftyr.hl_various.R")
		
	return(hl_getRunParameter(simp, parameter = agentParamColumn, runIdColumn=runIdColumn))
}
#' Fetch agent param ID from Runs.csv for paramId in given simp
#' 
#' @param simp 
#' @param parameter parameter (colname) to fetch 
#' @param runIdColumn 
#' @return parameter value for param ID defined in given \code{simp$sim$runids}.
#' 
#' @author Sascha Holzhauer
#' @export
hl_getRunParameter <- function(simp, parameter, runIdColumn="run") {
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]})
	
	futile.logger::flog.debug("Fetch Run parameter for param ID %d",
			paramid,
			name = "craftyr.hl_various.R")
	
	runData <- input_csv_param_runs(simp)
	return(runData[runData[, runIdColumn] == paramid, parameter])
}
#' Extracts BaseDirAdaptation from Scenario.xml (empty string if not defined)
#' 
#' @param simp  
#' @return BaseDirAdaptation
#' 
#' @author Sascha Holzhauer
#' @export
hl_getBaseDirAdaptation <- function(simp) {
	
	xmlParsed <- XML::xmlParse(file=paste(simp$dirs$param$getparamdir(simp), "Scenario.xml", sep="/"))
	xml_data  <- XML::xmlToList(xmlParsed)
	basediradaptation <- xml_data[[".attrs"]]["basedirAdaptation"]
	
	return(if(!is.na(basediradaptation)) basediradaptation else "")
}
#' List required capital files for the given configuration
#' 
#' @param simp 
#' @param ID 
#' @param cellInitialiserColName 
#' @param relativeToDataDir  
#' @return vector of filenames
#' 
#' @author Sascha Holzhauer
#' @export
hl_getCapitalDataFile <- function(simp, ID=NULL, cellInitialiserColName = "CellInitialisers",
		relativeToDataDir = TRUE) {
	# get worldLoaderFile
	xml_data <- XML::xmlToList(XML::xmlParse(file=paste(simp$dirs$param$getparamdir(simp), "Scenario.xml", sep="/")))
	worldLoaderFile <- paste(simp$dirs$param$getparamdir(simp), craftyr:::hl_getBaseDirAdaptation(simp),
			xml_data[["worldLoaderFile"]], sep="/")
	
	# get Region CSV
	world_xml_data <- XML::xmlToList(XML::xmlParse(worldLoaderFile))
	world_xml_data <- unlist(world_xml_data)
	world_data_filename <- paste(simp$dirs$param$getparamdir(simp), craftyr:::hl_getBaseDirAdaptation(simp),
			get_xmlfunction_parameter(unlist(world_xml_data), "regionCSV", ""), sep="/")
	world_data <- read.csv(world_data_filename)
	
	# get CellInitialisers XML
	if (is.null(ID)) ID = world_data$ID[1]
	
	if (!cellInitialiserColName %in% colnames(world_data)) {
		R.oo::throw.default("CSV file ", world_data_filename, " does not contain column >", cellInitialiserColName, "<!")
	}
	cellinit_xml <- paste(simp$dirs$param$getparamdir(simp), craftyr:::hl_getBaseDirAdaptation(simp),
			input_tools_getParamValue(simp, world_data[world_data$ID == ID, cellInitialiserColName]), sep="/")

	# get file name(s)
	cellinit_xml_data <- XML::xmlToList(XML::xmlParse(file=cellinit_xml))
	
	filenames = paste(if(!relativeToDataDir) simp$dirs$param$getparamdir(simp) else simp$sim$folder, 
			craftyr:::hl_getBaseDirAdaptation(simp), cellinit_xml_data["csvFile"], sep="/")
	
	# subsitute
	filenames <- gsub("%w", simp$sim$world, filenames, fixed=TRUE)
	filenames <- gsub("%k", simp$sim$regionalisation, filenames, fixed=TRUE)
	
	return(sapply(simp$sim$regions, function(region) gsub("%r", region, filenames, fixed=TRUE)))
}
#' List required input files for the given configuration.
#' 
#' @param simp  
#' @return LaTeX formated table
#' 
#' @author Sascha Holzhauer
#' @export
hl_printRequiredInputFilesTable <- function(simp) {
	files <- data.frame()
	files <- rbind(files, data.frame(Type = "Services", Data = paste(simp$mdata$services, collapse=", ")))
	files <- rbind(files, data.frame(Type = "Capitals", Data = paste(simp$mdata$capitals, collapse=", ")))
	
	capData <- hl_getCapitalDataFile(simp)
	rownames(capData) <- NULL
	files <- rbind(files, data.frame(Type = "Capital Data", Data = capData))
	files <- rbind(files, data.frame(Type = "Capital changes", Data = "see 'Capital Changes'"))
	files <- rbind(files, data.frame(Type = "Demand", Data = "see 'Service demand'"))
	files <- rbind(files, data.frame(Type = "Benefit functions", Data = "see 'Benefit Functions'"))
	files <- rbind(files, data.frame(Type = "Allocation model", Data = "see 'Run parameters'"))
	files <- rbind(files, data.frame(Type = "Institutions", Data = "see table 'Run parameters'"))
	files <- rbind(files, data.frame(Type = "AFT production", Data = "see 'Agent Production Parameters'"))
	files <- rbind(files, data.frame(Type = "AFT sensitivities", Data = "see 'Agent Production Parameters'"))
	files <- rbind(files, data.frame(Type = "Social Network", Data = "see 'Run parameters'"))
	
	table <- xtable::xtable(files,
			label="model.input.files", 
			caption="Model input files",
			align=c("r", "r", "p{13cm}")
	)
	
	print(table, sanitize.colnames.function = identity,
			sanitize.rownames.function = identity,
			include.rownames = FALSE,
			table.placement = "H")
}
#' List required input files for the given configuration.
#' 
#' @param simp  
#' @return LaTeX formated table
#' 
#' @author Sascha Holzhauer
#' @export
hl_printCapitalChangesTable <- function(simp) {
	# find relevant factors:
	# Get (relevant) institutions
	
	# TODO parse institution XMLs - tricky to identify the right institution(s)
	
	capchanges <- read.csv(paste(simp$dirs$param$getparamdir(simp, datatype="capitaldyns"),
					as.character(input_csv_param_runs(simp, paramid = TRUE)[1,"capitalFactorsCSV"]), sep="/"))
		
	table <- xtable::xtable(capchanges,
			label="model.input.capitalchanges", 
			caption="Factors of change for capitals")
	
	print(table, sanitize.colnames.function = identity,
			sanitize.rownames.function = identity,
			include.rownames = FALSE,
			table.placement = "H")
}
#' Retrieve information about the number of cells.
#' 
#' @param simp 
#' @param dataname  
#' @return list of data
#' 
#' @author Sascha Holzhauer
#' @export
hl_getCellNumbers <- function(simp, dataname = "csv_LandUseIndex_rbinded") {
	input_tools_load(simp, objectName="csv_LandUseIndex_rbinded")
	output <- list()
	output$x_min <- min(get(dataname)$X)
	output$x_max <- max(get(dataname)$X)
	output$y_min <- min(get(dataname)$Y)
	output$y_max <- max(get(dataname)$Y)
	output$squareNumberCells <- output$x_max * output$y_max
	output$ratioDefinedCells <- length(get(dataname)[get(dataname)$Tick==get(dataname)$Tick[1], "Y"])*
			100/output$squareNumberCells
	return(output)
}
#' Fill given data with \code{doNothingAction} for missing entries of the combinations
#' Tick/Runid/Region/Agent
#' 
#' @param simp 
#' @param data 
#' @param doNothingAction 
#' @param score the score value that is assigned to added DoNothing actions.
#' @return manipulated data.frame
#' 
#' @author Sascha Holzhauer
#' @export
hl_actions_fillDoNothing <- function(simp, data, doNothingAction =  "DoNothing", score = 0) {
	ticks <- data.frame(Tick=seq(min(data$Tick), max(data$Tick)))
	data <- dplyr::full_join(expand.grid(
					Tick=seq(min(data$Tick), max(data$Tick)), 
					Runid = unique(data$Runid), 
					Region = unique(data$Region),
					Agent = unique(data$Agent)),
			data)
	data$Action[is.na(data$Action)] <- doNothingAction
	data$Score[is.na(data$Score)] <- score
	data$Selected[is.na(data$Selected)] <- 1
	return(data)
}