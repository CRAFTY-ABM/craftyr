#' Reads marginal utilties and prepares the data for plotting
#' 
#' Stores data and returns.
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
hl_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, "MarginalUtilities.csv", sep="/"),
		storerdata = TRUE) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape2::melt(utilities, variable.name="Service", 
			id.vars= c("Year"), 
			direction="long")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$Runid <- 0
	
	if (storerdata) {
		input_tools_save(simp, "csv_MarginalUtilitites_melt")
	}
	
	visualise_lines(simp, csv_MarginalUtilitites_melt, "value", title = "Marginal Utilities",
			colour_column = "Service",
			filename = paste("MarginalUtilities", sep=""),
			alpha=0.7)
}
#' Print LaTeX table including run information
#' 
#' Considers the \code{simp$sim$version} in column version and the 
#' the \code{simp$sim$runids[1]} in column '1st Run ID' or between '1st Run ID' and 'Last Run ID'. 
#' @param simp 
#' @param filename 
#' @param rows
#' @return xtable plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_compileruninfos <- function (simp, filename = simp$dirs$output$runinfo, rows = NULL) {
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
				simp$sim$runids[1]}) 
	randomseed <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][2] else NULL)
				
	if (tools::file_ext(filename) == "ods") {
		require(readODS)
		runinfo <- read.ods(filename)[[1]][,1:simp$tech$runinfocolnumber]
		colnames(runinfo) <- runinfo[2,]
		runinfo <- runinfo[-c(1,2),]
		rinfo <- runinfo[runinfo["Version"] == simp$sim$version &
						runinfo["1st Run ID"] == paramid & (is.null(randomseed) | runinfo["Random Seed"]== randomseed), ]
		
		if (length(rinfo[,1]) == 0) {
			rinfo <- runinfo[runinfo["Version"] == simp$sim$version & runinfo["1st Run ID"] <= paramid &
							runinfo["Last Run ID"] >= paramid & (is.null(randomseed) | runinfo["Random Seed"]== randomseed), ]
		}
	} else if(tools::file_ext(filename) == "csv") {
		runinfo <- read.csv(filename, skip = 1)
		rinfo <- runinfo[runinfo$Version == simp$sim$version,]
	} else {
		R.oo::throw.default("File extension ", tools::file_ext(filename)," not supported!")
	}
	
	if (length(rinfo[,1]) == 0) {
		R.oo::throw.default("Runinfo table ", filename," does not contain a row for version ", 
						simp$sim$version, "!", sep="")
	}
	
	if (!is.null(rows)) {
		rinfo <- rinfo[,1:rows]
	}
	table <- xtable::xtable(t(rinfo),
			label="model.run.information", 
			caption="Model run information",
			align=c("r", "p{13cm}")
	)
	
	print(table, sanitize.colnames.function = identity,
			sanitize.rownames.function = identity,
			table.placement = "H")
}
#' Print LaTeX table including run parameters
#' 
#' Considers the \code{simp$sim$version} in column run. 
#' @param simp 
#' @param runidcolumnname
#' @return xtable plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_compilerunparams <- function (simp, runidcolumnname = "run") {
	
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]})
	
	runData <- input_csv_param_runs(simp)
	
	runData <- runData[runData[runidcolumnname] == paramid, ]
		
	
	if (length(runData[,1]) == 0) {
		R.oo::throw.default("Run parameter table does not contain a row for version " + 
						paramid, "!", sep="")
	}
	
	table <- xtable::xtable(t(runData),
			label="model.run.parameters", 
			caption="Model run parameters",
			align=c("r", "p{13cm}")
	)
	
	print(table, sanitize.colnames.function = identity,
			table.placement = "H")
}
#' Generates AFT key as CSV with columns 'Index' and 'LandUse'
#' 
#' @param simp  
#' \begin{itemize}
#' 	\item \code{simp$mdata$aftNames}
#'  \item \code{simp$dirs$output$data}
#'	\end{itemize}
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
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]})
	
	futile.logger::flog.debug("Fetch agent param ID for param ID %d",
				paramid,
				name = "craftyr.hl_various.R")
		
	runData <- input_csv_param_runs(simp)
	agentParamId <- runData[runData[, runIdColumn] == paramid, agentParamColumn]
	return(agentParamId)
}