#' Load, aggregate and visualise AFT composition data.
#' @param simp 
#' @param dataname 
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftcomposition <- function(simp, dataname = "csv_cell_aggregated") {
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
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
#' Load from csv data, aggregate, and plot AFT competitiveness
#' @param simp 
#' @param dataname 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_competitiveness <- function(simp, dataname = "csv_cell_aggregated") {
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
	aftData <- data[, colnames(data) %in% c("Tick", "Competitiveness", "Runid", "Region", "LandUseIndex")]
	aftData <- aggregate(subset(aftData, select=c("Competitiveness")),
			by = list(ID = aftData[,"Runid"], Tick=aftData[, "Tick"], AFT=aftData[,"LandUseIndex"]),
			FUN=mean)
	aftData$AFT <- as.factor(aftData$AFT)
	
	# Add 0 for missing entries:
	# http://stackoverflow.com/questions/25054174/data-standardization-for-all-group-data-frame-in-r
		## does not work
	#reshape2::melt(reshape2::dcast(aftData, Tick~AFT, value.var="Proportion",fill=0), id.var="Date")
		
	visualise_lines(simp, aftData, "Competitiveness", title = "AFT Competitiveness (invalid mean!)",
			colour_column = "AFT",
			colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = "ID",
			linetype_legendtitle = simp$sim$rundesclabel,
			linetype_legenditemnames = simp$sim$rundesc,
			filename = paste("TotalCompetitivenessInvalid", 
					shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
			alpha=0.7)
}
#' Read supply and demand and plot for given runid.
#' @param simp 
#' @param runid 
#' @param dataname 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_demandsupply <- function(simp, runid=0, dataname = "csv_cell_aggregated") {
	convert_aggregate_demand(simp)
	convert_aggregate_supply(simp, celldataname = dataname)
	
	input_tools_load(simp, "csv_aggregated_demand")
	input_tools_load(simp, "csv_aggregated_supply")
	
	### Demand & Supply
	datDemand <- data.frame(Tick=csv_aggregated_demand$Tick, Variable=csv_aggregated_demand$variable, 
			Type="Demand", Value=csv_aggregated_demand$Demand)
	datSupply <- csv_aggregated_supply[csv_aggregated_supply$ID == as.numeric(runid),]
	datSupply <- data.frame(Tick=datSupply$Tick, Variable=datSupply$Service, Type="Supply", 
			Value=datSupply$TotalProduction)
	combined <- rbind(datDemand, datSupply)
	combined <- aggregate(subset(combined, select=c("Value")),
			by =list(Tick=combined[, "Tick"], ID=combined[,"Type"], Service = combined[,"Variable"]), FUN=sum)
	visualise_lines(simp, combined, "Value", title = paste("Demand & Supply", simp$sim$rundesc[as.character(runid)]),
			colour_column = "Service",
			linetype_column = "ID",
			filename = paste("TotalDemandAndSupply_",simp$sim$rundesc[as.character(runid)], sep=""),
			alpha=0.7)
}
#' Transition plot of AFT take overs due to giving in
#' @param simp 
#' @param runid 
#' @param dataname 
#' @param starttick 
#' @param tickinterval 
#' @param endtick 
#' @param datanametakeovers 
#' @return transition plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_takeovers <- function(simp, runid = simp$sim$runids[1], dataname = "csv_cell_aggregated",
		starttick = 2010, tickinterval=5, endtick = 2040,
		datanametakeovers = "csv_aggregateTakeOver") {
	input_tools_load(simp, dataname)
	dataAgg <- get(dataname)
	
	startPopulation <- data.frame(names(simp$mdata$aftNames), 0)
	names(startPopulation) <- c("Agent", "AFT")
	sp <- aggregate(subset(dataAgg, select=c("AFT"),
					subset = dataAgg$Tick==starttick && dataAgg$Runid == runid), by = list(
					Agent = dataAgg[dataAgg$Tick == starttick && dataAgg$Runid == runid,"LandUseIndex"]), FUN=sum)
	startPopulation[startPopulation$Agent %in% sp$Agent,"AFT"] <- sp$AFT 
	
	startPopulation$Agent <- simp$mdata$aftNames[as.character(startPopulation$Agent)]
	
	input_tools_load(simp, datanametakeovers)
	dataTakeOvers <- get(datanametakeovers)
	simp$mdata$aftNames <- simp$mdata$aftNames[-1]
	dat <- aggregate(subset(dataTakeOvers, select=simp$mdata$aftNames), by = list(
					Tick=dataTakeOvers[, "Tick"],
					Runid=dataTakeOvers[, "Runid"],
					AFT=dataTakeOvers[,"AFT"]),
			FUN=sum)
	
	startPopulation <- startPopulation[match(simp$mdata$aftNames, startPopulation$Agent),]
	colnames(startPopulation)[colnames(startPopulation) == "AFT"] <- "Number"
	
	# TODO cells that go to unmanaged are not considered...
	output_visualise_takeovers(simp,
			data = dat, 
			startpopulation = startPopulation,
			starttick = starttick + 1,
			endtick=endtick,
			tickinterval=tickinterval,
			type_of_arrow = "gradient2sided",
			transitionthreshold = 5)
}
#' Transition plot of AFT take overs (both due to giving in and giving up)
#' @param simp 
#' @param runid 
#' @param dataname 
#' @param starttick 
#' @param tickinterval 
#' @param endtick 
#' @param datanametakeovers 
#' @return transition plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_takeovers_all <- function(simp, runid = simp$sim$runids[1], landusedataname = "csv_LandUseIndex_rbinded",
		starttick = 2010, tickinterval=5, endtick = 2040,
		datanametakeovers = "csv_aggregateTakeOver", dataname = "csv_cell_aggregated") {
	
	dataTakeOversAll <- convert_aggregate_takeovers(simp, landusedataname = landusedataname)
	input_tools_save(simp, "dataTakeOversAll")
	
	hl_takeovers(simp, runid = simp$sim$runids[1], dataname = dataname,
			starttick = starttick, tickinterval=tickinterval, endtick = endtick,
			datanametakeovers = datanametakeovers)
		
}
#' AFT take over fluctations as timeline
#' @param simp 
#' @param runid 
#' @param dataname 
#' @param starttick 
#' @param tickinterval 
#' @param endtick 
#' @param datanametakeovers 
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_afttakeoverfluctuations <- function(simp, dataname = "csv_cell_aggregated",
		starttick = 2010, tickinterval=5, endtick = 2040,
		datanametakeovers = "csv_aggregateTakeOver") {
	input_tools_load(simp, dataname)
	dataAgg <- get(dataname)
	
	input_tools_load(simp, datanametakeovers)
	dataTakeOvers <- get(datanametakeovers)
	simp$mdata$aftNames <- simp$mdata$aftNames[-1]
	dat <- aggregate(subset(dataTakeOvers, select=simp$mdata$aftNames), by = list(
					Tick=dataTakeOvers[, "Tick"],
					Runid=dataTakeOvers[, "Runid"],
					AFT=dataTakeOvers[,"AFT"]),
			FUN=sum)
	
	output_visualise_aftFluctuations(simp,
			data = dat,
			starttick = starttick + 1,
			endtick = endtick - 1,
			tickinterval = tickinterval)
}
#' Yearly aggregated AFT composition
#' @param simp 
#' @param dataname 
#' @return timelien plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aggregate_aftcompositions <- function(simp, dataname = "csv_aggregateAFTComposition") {
	input_tools_load(simp, dataname)
	dataComp <- get(dataname)
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
#' Yearly aggregated demand and supply
#' @param simp 
#' @param dataname
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aggregate_demandsupply <- function(simp, dataname = "csv_aggregateServiceDemand") {
	input_tools_load(simp, dataname)
	data <- convert_aggregate_meltsupplydemand(simp, get(dataname))
	
	# aggregate regions:
	data <- aggregate(subset(data, select=c("Value")),
			by = list(ID = data[,"Runid"],
					Tick=data[, "Tick"],  Scenario = data[,"Scenario"],
					Service=data[,"Service"], Type=data[,"Type"]),
			FUN=sum)
	
	visualise_lines(simp, data, "Value", title = "Aggregated Service Supply & Demand",
			colour_column = "Service",
			colour_legenditemnames = simp$mdata$conversion$services,
			linetype_column = "Type",
			facet_column = "ID",
			filename = paste("AggregateServiceDemand", 
					shbasic::shbasic_condenseRunids(data.frame(data)[, "ID"]), simp$sim$id, sep="_"),
			alpha=0.7)
}
#' Bar plot of number of takeovers per number of trials. Various AFTs as facets.
#' @param simp 
#' @param dataname 
#' @param region 
#' @param facet_ncol 
#' @return plot 
#' 
#' @author Sascha Holzhauer
#' @export
hl_gistatistics_singleRegion <- function(simp, dataname = "csv_aggregateGiStatistics", 
		regions = simp$sim$regions, facet_ncol = 1) {
	
	input_tools_load(simp, "csv_aggregateGiStatistics")
	
	csv_aggregateGiStatistics <- csv_aggregateGiStatistics[csv_aggregateGiStatistics[,"Region"] %in% regions,]
	
	dat <- aggregate(subset(csv_aggregateGiStatistics, select=simp$mdata$aftNames[-1]), by = list(
					Trials=csv_aggregateGiStatistics[, "Trials"],
					Runid=csv_aggregateGiStatistics[, "Runid"]),
			FUN=sum)
	
	melteddat <- reshape2::melt(dat, variable.name="AFT", id.vars= c("Trials", "Runid"), 
			direction="long", value.name = "Number")
	
	visualise_bars(simp, data = melteddat, y_column = "Number", title = "Giving In Statistics",
			facet_column = "AFT", facet_ncol = facet_ncol, fill_column = "AFT",
			alpha=1.0, x_column = "Trials", ggplotaddons = ggplot2::theme(legend.position="none"))
}
#' Bar plot of number of takeovers per number of trials. Various regions as facets.
#' @param simp 
#' @param dataname 
#' @param region 
#' @param facet_ncol 
#' @return plot 
#' 
#' @author Sascha Holzhauer
#' @export
hl_gistatistics_singleAFT <- function(simp, dataname = "csv_aggregateGiStatistics",
		regions = simp$sim$regions, facet_ncol = 1) {

	input_tools_load(simp, "csv_aggregateGiStatistics")
	
	csv_aggregateGiStatistics <- csv_aggregateGiStatistics[csv_aggregateGiStatistics[,"Region"] %in% regions,]

	melteddat <- reshape2::melt(dat, variable.name="AFT", id.vars= c("Region", "Trials", "Runid"), 
			direction="long")
	
	dat <- aggregate(subset(csv_aggregateGiStatistics, select=simp$mdata$aftNames[-1]), by = list(
					Trials=csv_aggregateGiStatistics[, "Trials"],
					Region=csv_aggregateGiStatistics[, "Region"],
					Runid=csv_aggregateGiStatistics[, "Runid"]),
			FUN=sum)
		
	visualise_bars(simp, data = melteddat, y_column = "Number", title = "Giving In Statistics",
			facet_column = "Region", facet_ncol = facet_ncol, fill_column = "Region",
			alpha=1.0, x_column = "Trials", ggplotaddons = ggplot2::theme(legend.position="none"))
}