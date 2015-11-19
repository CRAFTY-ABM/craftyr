#' Print AFT productivities
#' @param simp 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @return LaTeX subsections and tables
#' 
#' @author Sascha Holzhauer
#' @export
hl_printAgentProductionParameters <- function(simp, filenameprefix = "AftProduction_",
		filenamepostfix = "_multi_medium", heading = "subsubsection") {
	
	# for each AFT
	for (aft in simp$mdata$aftNames[-1]) {
		# get productivity table 
		data <- input_csv_param_productivities(simp, aft, filenameprefix = filenameprefix,
				filenamepostfix = filenamepostfix)
		
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
#' Print agent parameters
#' @param simp 
#' @param filenameprefix 
#' @param filenamepostfix 
#' @param aftParamId
#' @return print xtable
#' 
#' @author Sascha Holzhauer
#' @export
hl_printAgentParameters <- function(simp, filenameprefix  = "AftParams_",
		filenamepostfix = "", aftParamId = NULL, columnindices = c(2:7, 9)) {
	
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
	
	table <- xtable::xtable(agentparams,
			label= "param.agents",
			caption= "Agent Parameters"
	)
	
	print(table, sanitize.colnames.function = identity,
			include.rownames = FALSE,
			table.placement = "H")
}
#' Read and plot compotition functions
#' @param simp 
#' @param srcfilename competition XML file
#' @return plot
#' 
#' @inheritParams visualise_competition_funcs
#' 
#' @author Sascha Holzhauer
#' @export
hl_printCompetitionFunctions <- function(simp, srcfilename = "Competition_linear", xrange = c(-3,3), yrange = c(-1,1),
		filename = "competitionFunctions") {
	functions <- input_xml_param_competition(simp, srcfilename = srcfilename)
	visualise_competition_funcs(simp, functions, xrange, yrange, filename = filename)
} 