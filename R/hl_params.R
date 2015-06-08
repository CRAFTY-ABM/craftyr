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
				table.placement = "h!")
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
		filenamepostfix = "", aftParamId = 0) {
	agentparams <- data.frame()
	# for each AFT
	for (aft in simp$mdata$aftNames[-1]) {
		# get productivity table 
		data <- input_csv_param_agents(simp, aft, filenameprefix, filenamepostfix)
		names(data) <- gsub("Distribution", "Dist", names(data), fixed = TRUE)
		agentparams <- rbind(agentparams, cbind(AFT = aft, data[data["aftParamId"] == aftParamId, 
								-c(1,length(data[1,])-2,length(data[1,])-1,length(data[1,]))]))
	}
	# print table
	table <- xtable::xtable(agentparams,
			label= "param.agents",
			caption= "Agent Parameters"
	)
	
	print(table, sanitize.colnames.function = identity,
			sanitize.rownames.function = identity,
			include.rownames = FALSE,
			table.placement = "h!")
}
#' Read and plot compotition functions
#' @param simp 
#' @param filename 
#' @param xrange Vector of two. The x range to plot 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_printCompetitionFunctions <- function(simp, filename = "Competition_linear", xrange = c(-3,3)) {
	functions <- input_xml_param_competition(simp, filename = filename)
	visualise_competition_funcs(simp, functions, xrange)
} 