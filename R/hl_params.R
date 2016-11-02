#' Print AFT productivities
#' @param simp 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @return LaTeX subsections and tables
#' 
#' @author Sascha Holzhauer
#' @export
hl_printAgentProductionParameters <- function(simp, filenameprefix = "AftProduction_",
		filenamepostfix = "_multi_medium", aftwisefolder = TRUE, heading = "subsubsection",
		filenameprefix_aftparams = "AftParams_", filenamepostfix_aftparams = "") {
	
	# for each AFT
	for (aft in simp$mdata$aftNames[-1]) {
		# aft = simp$mdata$aftNames[2]
		# get productivity table 
		data <- input_csv_param_productivities(simp, aft, filenameprefix = filenameprefix,
				filenamepostfix = filenamepostfix, aftwisefolder = aftwisefolder,
				filenameprefix_aftparams = filenameprefix_aftparams,
				filenamepostfix_aftparams = filenamepostfix_aftparams, )
		
		# print table
		table <- xtable::xtable(data,
				label= paste("param.productivities.", aft, sep=""), 
				caption= paste("Productivities and Capital Sensitivities for ", tikzDevice::sanitizeTexString(aft), sep="")
		)
		
		cat(paste("\\", heading, "{Productivities and Capital Sensitivities: ", 
						tikzDevice::sanitizeTexString(aft), "}", sep=""))
		print(table, sanitize.colnames.function = identity,
				sanitize.rownames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	}
}
#' Plot AFT's production as bar plot
#' 
#' @param simp 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @param heading 
#' @param returnplot if true the ggplot object is returned
#' @return bar plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_plotAgentProductionParameters <- function(simp, filenameprefix = "AftProduction_",
		filenamepostfix = "_multi_medium", ggplotaddons = NULL, returnplot = FALSE) {
	
	# for each AFT
	prodData <- data.frame()
	for (aft in simp$mdata$aftNames[-1]) {
		# aft = simp$mdata$aftNames[2]
		# get productivity table 
		data <- input_csv_param_productivities(simp, aft, filenameprefix = filenameprefix,
				filenamepostfix = filenamepostfix)
		prodData <- rbind(prodData, data.frame(data[, c("X", "Production")], AFT = aft, row.names= NULL))
	}
	colnames(prodData)[colnames(prodData) == "X"] <- "Service"
	p1 <- visualise_bars(simp, prodData, y_column="Production", title = "",
		fill_column = "Service", fill_legenditemnames = NULL,
		facet_column = "AFT", facet_ncol = length(unique(prodData$AFT)), filename = "AFTproductions",
		alpha=1.0, ggplotaddons = ggplotaddons, x_column = "Service", position = "dodge",
		returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Print agent parameters
#' @param simp 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @param aftParamId
#' @param if \code{TRUE} no LaTeX table is printed but the data.frame returned.
#' @return print xtable
#' 
#' @author Sascha Holzhauer
#' @export
hl_printAgentParameters <- function(simp, filenameprefix  = "AftParams_",
		filenamepostfix = "", aftParamId = NULL, columnindices = c(2:7, 9), returnDataFrame = FALSE) {
	
	if (is.null(aftParamId)) {
		aftParamId = hl_getAgentParamId(simp)
	}
	
	futile.logger::flog.info("Print agent parameter table for agent param ID %d",
				aftParamId,
				name = "craftyr.hl_params.R")
		
	agentparams <- data.frame()
	# for each AFT
	for (aft in simp$mdata$aftNames[-1]) {
		# get productivity table 
		data <- input_csv_param_agents(simp, aft, filenameprefix, filenamepostfix)
		names(data) <- gsub("Distribution", "Dist", names(data), fixed = TRUE)
		agentparams <- rbind(agentparams, cbind(AFT = aft, data[data["aftParamId"] == aftParamId, 
								columnindices]))
	}
	# print table
	colnames(agentparams) <- gsub("givingUp", "GU", colnames(agentparams), fixed = TRUE)
	colnames(agentparams) <- gsub("givingIn", "GI", colnames(agentparams), fixed = TRUE)
	colnames(agentparams) <- gsub("serviceLevelNoise", "prodNoise", colnames(agentparams), fixed = TRUE)
	
	if (returnDataFrame) {
		return(agentparams)
	} else {
		table <- xtable::xtable(agentparams,
				label= "param.agents",
				caption= "Agent Parameters"
		)
		
		print(table, sanitize.colnames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	}
}
#' Read and plot competition functions
#' @param simp 

#' @param srcfilename XML filename without extension (must be .xml) 
#' @param srcfilepath
#' @param filename filename for figure
#' @param returnplot if true the ggplot object is returned
#' @return plot
#' 
#' @inheritParams hl_getCompetitionFunctions visualise_competition_funcs
#' 
#' @author Sascha Holzhauer
#' @export
hl_printCompetitionFunctions <- function(simp, srcfilename = NULL, 
		srcfilepath = dirname(paste(simp$dirs$output$data, simp$sim$folder,
						hl_getBaseDirAdaptation(simp),
						hl_getRunParameter(simp, "Competition_xml"), 
				sep="/")),
		xrange = c(-3,3), yrange = c(-1,1),
		 filename = "competitionFunctions", runidcolumnname="run", returnplot = FALSE) {
	
	functions <- hl_getCompetitionFunctions(simp, srcfilename = srcfilename, srcfilepath = srcfilepath,
			runidcolumnname = runidcolumnname)
	p1 <- visualise_competition_funcs(simp, functions, xrange, yrange, filename = filename, returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Read competition functions
#' 
#' @param simp 
#' @param srcfilename competition XML file. Will be generated from \code{simp} if missing.
#' @param srcfilepath obtained from Links.csv by default. Assign \code{NULL} to apply
#' \code{simp$dirs$param$getparamdir}.
#' @param runidcolumnname 
#' @return functions
#' 
#' @author Sascha Holzhauer
#' @export
hl_getCompetitionFunctions <- function(simp, srcfilename = NULL, 
		srcfilepath = dirname(paste(simp$dirs$output$data, simp$sim$folder,
						hl_getBaseDirAdaptation(simp),
						hl_getRunParameter(simp, "Competition_xml"), 
						sep="/")),
		runidcolumnname="run", returntext = FALSE) {
	
	if(is.null(srcfilename)) {
		paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
							simp$sim$runids[1]})
		runData <- input_csv_param_runs(simp)
		
		if (is.null(runData[runData[runidcolumnname] == paramid, 
										"Competition_xml"])) {
							R.oo::throw.default("The run CSV file does not contain the column >Competition_xml< for",
									" paramid ", paramid, "!")
						}
		
		srcfilename <- tools::file_path_sans_ext(basename(as.character(runData[runData[runidcolumnname] == paramid, 
										"Competition_xml"])))
	}
	return(input_xml_param_competition(simp, srcfilename = srcfilename, 
					srcfilepath = srcfilepath, returntext = returntext))
}
#' Print LaTeX table including run information
#' 
#' Considers the \code{simp$sim$version} in column version and the 
#' the \code{simp$sim$runids[1]} in column '1st Run ID' or between '1st Run ID' and 'Last Run ID'. 
#' Prints the last entry in case there are multiple matches.
#' 
#' @param simp 
#' @param filename 
#' @param rows
#' @return xtable plot
#' 
#' @seealso hl_getRunInfo
#' 
#' @author Sascha Holzhauer
#' @export
hl_compileruninfos <- function (simp, filename = simp$dirs$output$runinfo, rows = NULL) {
	
	rinfo <- hl_getRunInfo(simp, filename = simp$dirs$output$runinfo)
	
	if (!is.null(rows)) {
		rinfo <- rinfo[,1:rows]
	}
	rinfo <- rinfo[nrow(rinfo),]
	
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
#' @seealso input_csv_param_runs
#' 
#' @author Sascha Holzhauer
#' @export
hl_compilerunparams <- function (simp, runidcolumnname = "run") {
	
	paramid <- as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
						simp$sim$runids[1]})
	
	futile.logger::flog.info("Print run parameter table for run ID %d",
			paramid,
			name = "craftyr.hl_params.R")
	
	runData <- input_csv_param_runs(simp)
	
	runData <- runData[runData[runidcolumnname] == paramid, ]
	
	
	if (length(runData[,1]) == 0) {
		R.oo::throw.default("Run parameter table does not contain a row for version " + 
						paramid, "!", sep="")
	}
	
	table <- xtable::xtable(t(runData),
			label="model.run.parameters", 
			caption="Model run parameters",
			align=c("r", "p{12cm}")
	)
	
	print(table, sanitize.colnames.function = identity,
			table.placement = "H")
}
#' Returns matrix to define allocation restrictions.
#' 
#' Fills the matrix with "0", meaning no restriction
#' @return matrix with agent types as rows and columns 
#' 
#' @author Sascha Holzhauer
#' @export
hl_getAllocationRestrictionMatrix <- function() {
	data <- matrix(data=c(0), nrow = length(simp$mdata$aftNames), ncol = length(simp$mdata$aftNames))
	rownames(data) <- colnames(data) <-  simp$mdata$aftNames
	return(data)	
}