#' Regional AFT composition
#' @param simp 
#' @param dataname 
#' @param facet_ncol 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_regions_aftcomposition <- function(simp, dataname = "csv_cell_aggregated", facet_ncol = 5) {
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
	aftData <- data[, colnames(data) %in% c("Tick", "LandUseIndex", "Runid", "Region", "AFT")]
	aftData$AftNumbers <- aftData$AFT
	aftData <- aggregate(subset(aftData, select=c("AftNumbers")),
			by = list(ID = aftData[,"Runid"],Region = aftData[,"Region"],
					Tick=aftData[, "Tick"],  AFT=aftData[,"LandUseIndex"]),
			FUN=sum)
	aftData$AFT <- as.factor(aftData$AFT)
	aftData$Proportion <- ave(aftData$AftNumbers, aftData$ID, aftData$Region, aftData$Tick, 
			FUN =  function(.x) .x/sum(.x))
	
	visualise_lines(simp, aftData, "Proportion", title = "Regional AFT composition",
			colour_column = "AFT",
			colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "ID",
			linetype_legendtitle = simp$sim$rundesclabel,
			linetype_legenditemnames = simp$sim$rundesc,
			facet_column = "Region",
			facet_ncol = facet_ncol,
			filename = paste("RegionalAftComposition", 
					shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
			alpha=0.7)
}
#' Regional demand and supply figure 
#' @param simp 
#' @param dataname 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_regions_demandandsupply <- function(simp, runid = 0, dataname = "csv_cell_aggregated",
		facet_ncol = 5) {
	
	convert_aggregate_demand(simp)
	convert_aggregate_supply(simp, dataname = dataname)
	
	input_tools_load(simp, "csv_aggregated_demand")
	input_tools_load(simp, "csv_aggregated_supply")
	
	datDemand <- data.frame(Tick=csv_aggregated_demand$Tick, Variable=csv_aggregated_demand$variable, 
			Type="Demand", Value=csv_aggregated_demand$Demand, Region = csv_aggregated_demand$Region)

	datSupply <- csv_aggregated_supply[csv_aggregated_supply$ID == as.numeric(runid),]
	datSupply <- data.frame(Tick=datSupply$Tick, Variable=datSupply$Service, Type="Supply", 
			Value=datSupply$TotalProduction, Region = datSupply$Region)
	
	combined <- rbind(datDemand, datSupply)
	
	combined <- aggregate(subset(combined, select=c("Value")),
			by =list(Tick=combined[, "Tick"], ID=combined[,"Type"], 
					Region = combined[,"Region"], Service = combined[,"Variable"]), FUN=sum)
	visualise_lines(simp, combined, "Value", title = paste("Regional Demand & Supply", 
					simp$sim$rundesc[as.character(runid)]),
			colour_column = "Service",
			facet_column = "Region",
			facet_ncol = facet_ncol,
			linetype_column = "ID",
			filename = paste("RegionalDemandAndSupply-",simp$sim$rundesc[as.character(runid)], sep=""),
			alpha=0.7)
}