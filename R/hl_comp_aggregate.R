#' Load, aggregate and visualise AFT composition data of several runs to compare
#' 
#' @param simp
#' @param simps list of simp that shall be compared. \code{simp$sim$shortid} is combined with Runid to distinguish data!
#' @param dataname name of aggregated data from cell data
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_cell_aftcomposition <- function(simp, simps, dataname = "csv_cell_aggregated") {
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
	
	visualise_lines(simp, aftData, "Proportion", title = "Total AFT composition",
			colour_column = "AFT",
			colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "ID",
			linetype_legendtitle = simp$sim$rundesclabel,
			linetype_legenditemnames = simp$sim$rundesc,
			filename = paste("TotalAftComposition", 
					shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
			alpha=0.7)
}
#' Compare Yearly aggregated AFT composition
#' 
#' @param simp 
#' @param dataname 
#' @return timelien plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_aggregate_aftcompositions <- function(simp, simps, dataname = "csv_aggregateAFTComposition") {
	dataComp <- data.frame()
	for (p in simps) {
		input_tools_load(p, dataname)
		d <- get(dataname)
		d$Runid <- paste(p$sim$shortid, d$Runid, sep="-")
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
	
	visualise_lines(simp, d, "value", title = "Aft Composition",
			colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "Runid",
			filename = "AftComposition",
			alpha=0.7)
}
#' Read supply and demand and plot for given runid
#' 
#' @param simp 
#' @param runid 
#' @param dataname aggregated demand and supply (from aggregated CSV)
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_comp_demandsupply <- function(simp, simps, dataname = "csv_cell_aggregated") {
	aggregated_demand <- data.frame()
	aggregated_supply  <- data.frame()
	for (p in simps) {
		convert_aggregate_demand(p)
		convert_aggregate_supply(p, celldataname = dataname)
		
		input_tools_load(p, "csv_aggregated_demand")
		input_tools_load(p, "csv_aggregated_supply")
		
		csv_aggregated_demand$Type <- paste("Demand", p$sim$shortid, sep="-")
		aggregated_demand <- rbind(aggregated_demand, csv_aggregated_demand)
		
		csv_aggregated_supply$Type <- paste("Supply", p$sim$shortid, sep="-")
		aggregated_supply <- rbind(aggregated_supply, csv_aggregated_supply)
	}
	
	### Demand & Supply
	datDemand <- data.frame(Tick=aggregated_demand$Tick, Variable=aggregated_demand$variable, 
			Type=aggregated_demand$Type, Value=aggregated_demand$Demand)
	
	datSupply <- data.frame(Tick=aggregated_supply$Tick, Variable=aggregated_supply$Service, Type=aggregated_supply$Type, 
			Value=aggregated_supply$TotalProduction)
	
	combined <- rbind(datDemand, datSupply)
	combined <- aggregate(subset(combined, select=c("Value")),
			by =list(Tick=combined[, "Tick"], ID=combined[,"Type"], Service = combined[,"Variable"]), FUN=sum)
	visualise_lines(simp, combined, "Value", title = paste("Demand & Supply (", paste(simp$sim$rundesc, collapse = "/"),
					")", sep=""),
			colour_column = "Service",
			linetype_column = "ID",
			filename = paste("TotalDemandAndSupply_", paste(simp$sim$rundesc, collapse = "-"), sep=""),
			alpha=0.7)
}