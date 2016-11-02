#' Load, aggregate and visualise AFT composition data of several runs to compare
#' 
#' @param simp
#' @param simps list of simp that shall be compared. See \code{\link{input_tools_load}} for relevant entries in
#' 			\code{simp}). \code{simp$sim$shortid} is combined with \code{Runid} in data to distinguish data!
#' @param dataname name of aggregated data from cell data
#' @param title title for plot
#' @param returnplot if true the ggplot object is returned
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_cell_aftcomposition <- function(simp, simps, dataname = "csv_cell_aggregated",
		returnplot = FALSE) {
	
	data <- data.frame()
	for (p in simps) {
		input_tools_load(p, dataname)
		d <- get(dataname)
		d$Runid <- paste(p$sim$shortid, d$Runid, sep="-")
		data <- rbind(data, d)		
	}

	aftData <- data[, colnames(data) %in% c("Tick", "LandUseIndex", "Runid", "Region", "AFT")]
	aftData$AftNumbers <- aftData$AFT
	aftData <- aggregate(subset(aftData, select=c("AftNumbers")),
			by = list(ID = aftData[,"Runid"],
					Tick=aftData[, "Tick"],  AFT=aftData[,"LandUseIndex"]),
			FUN=sum)
	aftData$AFT <- as.factor(aftData$AFT)
	aftData$Proportion <- ave(aftData$AftNumbers, aftData$ID, aftData$Tick, FUN =  function(.x) .x/sum(.x))
	aftData$Number <- NULL
	
	# Add 0 for missing entries:
	# http://stackoverflow.com/questions/25054174/data-standardization-for-all-group-data-frame-in-r
	## does not work
	#reshape2::melt(reshape2::dcast(aftData, Tick~AFT, value.var="Proportion",fill=0), id.var="Date")
	
	p1 <- visualise_lines(simp = simp, data = aftData, y_column = "Proportion", title = "Total AFT composition",
			colour_column = "AFT",
			colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "ID",
			linetype_legendtitle = simp$sim$rundesclabel,
			linetype_legenditemnames = simp$sim$rundesc,
			filename = paste("TotalAftComposition", 
					shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
			alpha=0.7,
			returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Compare Yearly aggregated AFT composition
#' 
#' @param simp 
#' @param dataname 
#' @param returnplot if true the ggplot object is returned
#' @return timelien plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_aggregate_aftcompositions <- function(simp, simps, dataname = "csv_aggregateAFTComposition", 
		filename = paste("AftComposition_", 
			if(!is.null(simp$sim$rundesc))paste(simp$sim$rundesc, collapse = "-") else simp$sim$id, sep=""),
			title = "Aft Composition", returnplot = FALSE)	 {
	
	dataComp <- data.frame()
	for (p in simps) {
		input_tools_load(p, dataname)
		d <- get(dataname)
		d$Runid <- paste(p$sim$shortid, d$Runid, sep="-")
		if (ncol(d) != nocl(dataComp)) {
			R.oo::throw.default("Number of columns does not match between runs. Most likely the set of AFTs (%s vs. %s) is not the same!",
					colnames(d)[! colnames(d) %in% c("Region", "Tick", "Scenario", "Runid")],
					colnames(dataComp)[! colnames(dataComp) %in% c("Region", "Tick", "Scenario", "Runid")])
		}
		dataComp <- rbind(dataComp, d)		
	}
	
	colnames(dataComp) <- gsub("AFT.", "", colnames(dataComp))
	
	data <- reshape2::melt(dataComp, variable.name="Agent", id.vars= c("Region", "Tick", "Runid", "Scenario"), 
			direction="long")
	
	d <- aggregate(subset(data, select=c("value")), by = list(AFT = data$Agent, 
					Tick= data$Tick, Runid=data$Runid, Scenario=data$Scenario), 
			"sum", na.rm = TRUE)
	
	# substitute AFT names by AFT ID
	aftNumbers <- names(simp$mdata$aftNames)
	names(aftNumbers) <- simp$mdata$aftNames
	d$AFT <- aftNumbers[as.character(d$AFT)]
	
	p1 <- visualise_lines(simp = simp, data = d, y_column = "value", title = title,
			colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "Runid",
			filename = filename,
			alpha=0.7,
			returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Read supply and demand and plot for given runid
#' 
#' Change IDs to meaningful names by setting \code{simp$sim$shortid} for \code{simps}
#' 
#' @param simp Considered:\itemize{
#'		\item{\code{simp$fig$averagedemand}}
#' 		\item{\code{simp$sim$rundesc}}
#' 		\item{\code{simp$sim$id}}
#' 		\item{\code{\link{visualise_lines}}}}
#'	}
#' @param simps list of simp that shall be compared. Relevant entries in
#' 			\code{simp}:\itemize{
#' 				\item{\code{p$sim$shortid}}
#' 				\item{\code{\link{convert_aggregate_demand}}}
#' 				\item{\code{\link{convert_aggregate_supply}}}
#' 				\item{\code{\link{input_tools_load}}}}
#' 			\code{simp$sim$shortid} is combined with \code{Runid} in data to distinguish data!
#' @param runid 
#' @param dataname aggregated demand and supply (from aggregated CSV)
#' @param title title for plot
#' @param visualise if \code{FALSE}, only combined and aggregated demand and supply data is returned
#' @return plot, combined supply and demand data
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_demandsupply <- function(simp, simps, dataname = "csv_cell_aggregated", 
		filename = paste("TotalDemandAndSupply_", 
			if(!is.null(simp$sim$rundesc))paste(simp$sim$rundesc, collapse = "-") else simp$sim$id, sep=""),
		title = paste("Demand & Supply (", paste(simp$sim$rundesc, collapse = "/"),")", sep=""),
		visualise = TRUE) {
	aggregated_demand <- data.frame()
	aggregated_supply  <- data.frame()
	for (p in simps) {
		convert_aggregate_demand(p)
		convert_aggregate_supply(p, celldataname = dataname)
		
		input_tools_load(p, "csv_aggregated_demand")
		input_tools_load(p, "csv_aggregated_supply")
		
		shortID <- shbasic::shbasic_substitute(simp, p$sim$shortid)
		
		csv_aggregated_demand$Type <- paste("Demand", shortID, sep="-")
		aggregated_demand <- rbind(aggregated_demand, csv_aggregated_demand)
		
		csv_aggregated_supply$Type <- paste("Supply", shortID, sep="-")
		aggregated_supply <- rbind(aggregated_supply, csv_aggregated_supply)
	}
	
	### Demand & Supply
	datDemand <- data.frame(Tick=aggregated_demand$Tick, Variable=aggregated_demand$variable, 
			Type=aggregated_demand$Type, Value=aggregated_demand$Demand)

	datSupply <- data.frame(Tick=aggregated_supply$Tick, Variable=aggregated_supply$Service, Type=aggregated_supply$Type, 
			Value=aggregated_supply$TotalProduction)
	
	if (simp$fig$averagedemand) {
		datDemand <- aggregate(subset(datDemand, select=c("Value")),
				by = list(Type = datDemand[,"Type"], Tick=datDemand[, "Tick"], Variable = datDemand[,"Variable"]), 
				FUN=sum)
		datDemand <- aggregate(subset(datDemand, select=c("Value")),
				by = list(Tick=datDemand[, "Tick"], Variable = datDemand[,"Variable"]), FUN=mean)
		datDemand$Type <- "Demand (mean)"
	} 
	
	combined <- rbind(datDemand, datSupply)
	combined <- aggregate(subset(combined, select=c("Value")),
			by =list(Tick=combined[, "Tick"], ID=combined[,"Type"], Service = combined[,"Variable"]), FUN=sum)
	combined <- combined[combined$Tick < simp$sim$endtick,]
	
	if(visualise) {
		visualise_lines(simp = simp, data = combined, y_column = "Value", title = title,
				colour_column = "Service",
				linetype_column = "ID",
				filename = filename,
				alpha=0.7)
	}
	
	invisible(combined)
}
#' Supply and Demand gap depending on AFT param
#' 
#' Plots the gap between demand and supply for various runs with
#' agent param as dependent variable.
#'
#' @param simp 
#' @param simps 
#' @param dataname 
#' @param filename 
#' @param title 
#' @param agentparam 
#' @param aft 
#' @param ggplotparams 
#' @param returnplot if true the ggplot object is returned
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_demandsupplygap_agentparams <- function(simp, simps = input_tools_buildsimplist(111:112),
		dataname = "csv_cell_aggregated", 
		filename = paste("SupplyDemandGap_", 
				if(!is.null(simp$sim$rundesc))paste(simp$sim$rundesc, collapse = "-") else simp$sim$id, sep=""),
		title = paste("Demand/Supply Gap", if(!is.null(paste(simp$sim$rundesc))) 
					paste(simp$sim$rundesc, collapse = "/"), sep=" - "),
		agentparam = "givingUpProb", aft = simp$mdata$aftNames[2], ggplotparams = NULL,
		returnplot = FALSE) {
			
	runs <- do.call(c, lapply(simps, function(x) x$sim$shortid))
	
	# get GU Probabilities
	runsdata <- input_csv_param_runs(simp)
	aftParamIds <- runsdata[runsdata$run %in% runs, "aftParamId"]
	aftparamdata <- input_csv_param_agents(simp, aft, filenameprefix = "AftParams_")
	agentparams <- aftparamdata[match(aftParamIds,aftparamdata$aftParamId), agentparam]
	names(agentparams) <- runs
	
	# Get supply demand gap
	supplydemand <- hl_comp_demandsupply(simp, simps, dataname=dataname, visualise = FALSE)
	supplydemand <- supplydemand[supplydemand$Tick == max(supplydemand$Tick),]
	supplydemand$Type <- gsub("[-1-9]", "", supplydemand$ID)
	supplydemand$ID <- gsub("[-A-Za-z]", "", supplydemand$ID)
	
	data <- reshape2::dcast(supplydemand, formula=ID+Service~Type, value.var="Value")
	data$Value <- data$Supply - data$Demand
	data$Tick <- agentparams[data$ID]
	
	# Draw figure
	p1 <- visualise_lines(simp = simp, data = data, y_column = "Value", title = title,
			colour_column = "Service",
			filename = filename,
			alpha=0.7,
			ggplotparams = list(ggplot2::xlab(agentparam), ggplotparams),
			returnplot = returnplot)
	if (returnplot) return(p1)
}
#' Supply and Demand gap depending on AFT param
#' 
#' Plots the gap between demand and supply for various runs with
#' agent param as dependent variable.
#'
#' @param simp 
#' @param simps 
#' @param dataname 
#' @param filename 
#' @param title 
#' @param agentparam 
#' @param aft 
#' @param ggplotparams 
#' @param returnplot if true the ggplot object is returned
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_percentalsupply_perService <- function(simp, simps = input_tools_buildsimplist(111:112),
		dataname = "csv_cell_aggregated", 
		filename = paste("SupplyDemandGap_", 
				if(!is.null(simp$sim$rundesc))paste(simp$sim$rundesc, collapse = "-") else simp$sim$id, sep=""),
		title = paste("Demand/Supply Gap", if(!is.null(paste(simp$sim$rundesc))) 
					paste(simp$sim$rundesc, collapse = "/"), sep=" - "),
		ggplotaddons = NULL,
		service = "Cereal",
		returnplot = FALSE) {
	
	# Get supply demand gap
	supplydemand <- hl_comp_demandsupply(simp, simps, dataname=dataname, visualise = FALSE)
	#supplydemand <- supplydemand[supplydemand$Tick == max(supplydemand$Tick),]
	supplydemand$Type <- gsub("[-0-9]", "", supplydemand$ID)
	supplydemand$ID <- gsub("[-A-Za-z]", "", supplydemand$ID)
	
	data <- reshape2::dcast(supplydemand, formula=ID+Service+Tick~Type, value.var="Value")
	if (!service %in% data$Service) {
		R.oo::throw.default("There is service ", service, "!")
	}
	data <- data[data$Service == service,]
	data$Value <- 100 *data$Supply/data$Demand
	
	# Draw figure
	p1 <- visualise_lines(simp = simp, data = data, y_column = "Value", title = title,
			colour_column = "Service",
			linetype_column = "ID",
			filename = filename,
			alpha=0.7,
			ggplotaddons = ggplotaddons,
			returnplot = returnplot)
	if (returnplot) return(p1)
}