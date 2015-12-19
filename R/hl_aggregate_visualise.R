#' Load, aggregate and visualise AFT composition data
#' 
#' Usually, does not include the number of unmanaged cells.
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
#' Load, aggregate and visualise production per service and AFT
#' 
#' @param simp 
#' @param dataname
#' @param facet_ncol
#' @param normaliseByAftNumber
#' @return timeline plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_serviceproduction <- function(simp, dataname = "csv_cell_aggregated", facet_ncol=2, 
		normaliseByAftNumber = TRUE) {
	
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
	serviceCols <- names(simp$mdata$conversion$services)[grep("Service",
					names(simp$mdata$conversion$services))]
	# select cols and skip unmanaged:
	prodData <- data[data$LandUseIndex >=0, colnames(data) %in% 
					c("Tick", "LandUseIndex", "AFT", "Runid", "Region", serviceCols)]
	prodData$AftNumbers <- prodData$AFT
	
	prodData <- aggregate(subset(prodData, select=c(serviceCols,"AftNumbers")),
			by = list(ID = prodData[,"Runid"],
					Tick=prodData[, "Tick"],  AFT=prodData[,"LandUseIndex"]),
			FUN=sum)
	
	if (normaliseByAftNumber) {
		prodData[,serviceCols] <- prodData[,serviceCols]/prodData$AftNumbers
	}
	prodData$AftNumbers = NULL
	
	prodData$AFT <- as.factor(prodData$AFT)
	prodData <- reshape2::melt(prodData, c("Tick", "AFT", "ID"), value.name="Production", variable.name="Service")
	
	prodData$Service <- simp$mdata$conversion$services[prodData$Service]
	prodData$AFT <- simp$mdata$aftNames[match(prodData$AFT, names(simp$mdata$aftNames))]
	
	visualise_lines(simp, prodData, "Production", title = "Service Production",
			colour_column = "Service",
			#colour_legenditemnames = simp$mdata$services,
			linetype_column = "ID",
			linetype_legendtitle = simp$sim$rundesclabel,
			linetype_legenditemnames = simp$sim$rundesc,
			facet_colum = "AFT",
			facet_ncol = facet_ncol,
			filename = paste("ServiceProduction", 
					shbasic::shbasic_condenseRunids(data.frame(prodData)[, "ID"]), sep="_"),
			alpha=simp$fig$alpha)
}
#' Load from csv data, aggregate, and plot AFT competitiveness
#' 
#' @param simp 
#' @param dataname 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_competitiveness <- function(simp, dataname = "csv_cell_aggregated") {
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
	aftData <- data[data$LandUseIndex != as.numeric(names(simp$mdata$aftNames)[simp$mdata$aftNames=="Unmanaged"]), 
			colnames(data) %in% c("Tick", "Competitiveness", "Runid", "Region", "LandUseIndex")]
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
#' Load from csv data, aggregate, and plot AFT competitiveness per region
#' 
#' If the number of regions is above \code{simp$fig$maxnumtypes} facets will be applied to region variable
#' @param simp 
#' @param dataname 
#' @return ggplot2 plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_competitivenessPerRegion <- function(simp, dataname = "csv_cell_aggregated", facet_ncol = 4) {
	input_tools_load(simp, dataname)
	data <- get(dataname)
	
	aftData <- data[data$LandUseIndex != as.numeric(names(simp$mdata$aftNames)[simp$mdata$aftNames=="Unmanaged"]), 
			colnames(data) %in% c("Tick", "Competitiveness", "Runid", "Region", "LandUseIndex")]
	aftData <- aggregate(subset(aftData, select=c("Competitiveness")),
			by = list(ID = aftData[,"Runid"], Tick=aftData[, "Tick"], Region=aftData[, "Region"], 
					AFT=aftData[,"LandUseIndex"]),
			FUN=mean)
	aftData$AFT <- as.factor(aftData$AFT)
	
	# Add 0 for missing entries:
	# http://stackoverflow.com/questions/25054174/data-standardization-for-all-group-data-frame-in-r
	## does not work
	#reshape2::melt(reshape2::dcast(aftData, Tick~AFT, value.var="Proportion",fill=0), id.var="Date")
	
	if (length(simp$sim$regions) <= simp$fig$maxnumtypes) {
		linetype_column = "Region"
		linetype_legendtitle = simp$sim$rundesclabel
		linetype_legenditemnames = simp$sim$rundesc
		facet_column = NULL
	} else {
		linetype_column = NULL
		linetype_legendtitle = NULL
		linetype_legenditemnames = NULL
		facet_column = "Region"
	}
	visualise_lines(simp, aftData, "Competitiveness", title = "AFT Competitiveness",
			colour_column = "AFT",
			colour_legenditemnames = simp$mdata$aftNames,
			linetype_column = linetype_column,
			linetype_legendtitle = linetype_legendtitle,
			linetype_legenditemnames = linetype_legenditemnames,
			facet_column = facet_column,
			facet_ncol = facet_ncol,
			filename = paste("TotalCompetitivenessPerRegion", 
					shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
			alpha=0.7)
}
#' Read supply and demand and plot for given runid
#' 
#' @param simp 
#' @param runid 
#' @param dataname 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_demandsupply <- function(simp, runid = simp$sim$id, dataname = "csv_cell_aggregated") {
	convert_aggregate_demand(simp)
	convert_aggregate_supply(simp, celldataname = dataname)
	
	input_tools_load(simp, "csv_aggregated_demand")
	input_tools_load(simp, "csv_aggregated_supply")
	
	### Demand & Supply
	datDemand <- data.frame(Tick=csv_aggregated_demand$Tick, Variable=csv_aggregated_demand$variable, 
			Type="Demand", Value=csv_aggregated_demand$Demand)
	datSupply <- csv_aggregated_supply[csv_aggregated_supply$ID == runid,]
	
	if (sum(csv_aggregated_supply$ID == runid) == 0) {
		futile.logger::flog.info("No ID %s in supply data!",
				str(runid),
				name = "craftyr.hl.aggregate.visualise")
	}
	
	datSupply <- data.frame(Tick=datSupply$Tick, Variable=datSupply$Service, Type="Supply", 
			Value=datSupply$TotalProduction)
	combined <- rbind(datDemand, datSupply)
	combined <- aggregate(subset(combined, select=c("Value")),
			by =list(Tick=combined[, "Tick"], ID=combined[,"Type"], Service = combined[,"Variable"]), FUN=sum)
	visualise_lines(simp, combined, "Value", title = paste("Demand & Supply", simp$sim$rundesc[runid]),
			colour_column = "Service",
			linetype_column = "ID",
			filename = paste("TotalDemandAndSupply_",simp$sim$rundesc[runid], sep=""),
			alpha=0.7)
}
#' Read data and visualise datacolumns as lines
#' 
#' Reads data from CSV file if not existing as rData object and visualises datacolums.
#' @param simp 
#' @param dataname name of rData object
#' @param datatype datatype of CSV data (only relevant if rData not yet existing)
#' @param datacolumns columns to plot
#' @param linetypecol
#' @param colourcol
#' @param titleprefix
#' @param filenameprefix 
#' @param percent data.frame with frist column containing the grouping (and named with the grouping variable) 
#' 	the 2nd column values shall be considered as 100% for
#' @return plot (and rData) 
#' 
#' @author Sascha Holzhauer
#' @export
hl_lines_from_csv <- function(simp, dataname, datatype, datacolumns = NULL, linetypecol = "ID",
		colourcol = "Type", titleprefix = NULL, filenameprefix = NULL, percent = NULL) {
	
	if (!input_tools_checkexists(simp, dataname)) {
		
		assign(dataname, input_csv_data(simp, dataname = NULL, 
				datatype = datatype,
				pertick = FALSE, bindrows = TRUE))
		
		input_tools_save(simp, dataname)
	}
	
	input_tools_load(simp, dataname)
	data <- get(dataname)

	data <- reshape2::melt(data, variable.name="Type", id.vars= c("Region", "Tick", "Runid", "Scenario"), 
			direction="long", value.name = "Value")
	if(!is.null(datacolumns)) data <- data[data$Type %in% datacolumns,]
	
	if(!is.null(percent)) {
		data <- plyr::ddply(data, names(percent)[1], function(cd, percent) {
					cd$Value <- cd$Value * 100 / percent[percent[,1] == as.character(unique(cd[,names(percent)[1]])), 2]
					cd
				}, percent = percent)
	}
	
	
	visualise_lines(simp, data, "Value", title = paste(titleprefix, simp$sim$rundesc[simp$sim$runid]),
			colour_column = colourcol,
			linetype_column = linetypecol,
			filename = paste(filenameprefix, "_", simp$sim$rundesc[simp$sim$runid], sep=""))
}
#' Read cell volatility data and visualise CellVolatility and NumVolatileCells as lines
#' 
#' @param simp 
#' @param dataname 
#' @param datatype 
#' @param datacolumns 
#' @return plot (and rData)
#' 
#' @author Sascha Holzhauer
#' @export
hl_volatility <- function(simp, dataname = "csv_aggregated_cellvolatility", 
		datatype = "AggregateCellVolatility", datacolumns = c("CellVolatility", "NumVolatileCells"),
		percent = NULL) {
	
	hl_lines_from_csv(simp, dataname = dataname, 
			datatype = datatype, datacolumns = datacolumns, linetypecol = "Region",
			colourcol = "Type", titleprefix = "Cell Volatility", filenameprefix = "CellVolatility", 
			percent = percent)
}
#' Transition plot of AFT take overs due to giving in
#' 
#' @param simp 
#' @param runid 
#' @param dataname 
#' @param starttick 
#' @param tickinterval 
#' @param endtick 
#' @param datanametakeovers
#' @param transitionthreshold
#' @param aftnames AFT names (defaults to simp$mdata$aftNames without the first entry which is usually
#' 				   'Unmanaged' and not considered in stored take over data)
#' @return transition plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_takeovers <- function(simp, runid = simp$sim$runids[1], dataname = "csv_cell_aggregated",
		starttick = simp$sim$starttick, tickinterval=5, endtick = simp$sim$endtick,
		datanametakeovers = "csv_aggregateTakeOver", transitionthreshold = 5, 
		aftnames = simp$mdata$aftNames[-1]) {
	input_tools_load(simp, dataname)
	dataAgg <- get(dataname)
	
	startPopulation <- data.frame(names(aftnames), 0)
	names(startPopulation) <- c("Agent", "AFT")
	sp <- aggregate(subset(dataAgg, select=c("AFT"),
					subset = dataAgg$Tick==starttick && dataAgg$Runid == runid), by = list(
					Agent = dataAgg[dataAgg$Tick == starttick && dataAgg$Runid == runid,"LandUseIndex"]), FUN=sum)
	
	startPopulation[startPopulation$Agent %in% sp$Agent,"AFT"] <- sp$AFT[sp$Agent %in% startPopulation$Agent]
	
	startPopulation$Agent <- aftnames[as.character(startPopulation$Agent)]
	
	input_tools_load(simp, datanametakeovers)
	dataTakeOvers <- get(datanametakeovers)
	
	if(any(aftnames %in% names(dataTakeOvers))) {
		dat <- aggregate(subset(dataTakeOvers, select=aftnames), by = list(
						Tick=dataTakeOvers[, "Tick"],
						Runid=dataTakeOvers[, "Runid"],
						AFT=dataTakeOvers[,"AFT"]),
				FUN=sum)
		
		startPopulation <- startPopulation[match(aftnames, startPopulation$Agent),]
		colnames(startPopulation)[colnames(startPopulation) == "AFT"] <- "Number"
		
		# TODO cells that go to unmanaged are not considered...
		output_visualise_takeovers(simp,
				data = dat, 
				startpopulation = startPopulation,
				starttick = starttick,
				endtick=endtick,
				tickinterval=tickinterval,
				type_of_arrow = "gradient2sided",
				transitionthreshold = transitionthreshold,
				aftnames = aftnames)
	} else {
		warning(paste("There is no AFT giving in data in rData with name", datanametakeovers, "for ID", simp$sim$id))
	}
}
#' Transition plot of AFT take overs (both due to giving in and giving up)
#' 
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
		starttick = simp$sim$starttick, tickinterval=10, endtick = simp$sim$endtick,
		datanametakeovers = "csv_aggregateTakeOver", dataname = "csv_cell_aggregated",
		grouping = c("Scenario", "Runid", "Region")) {
	
	if (!shbasic::sh_tools_issaved(simp, datanametakeovers)) {
		assign(datanametakeovers, convert_aggregate_takeovers(simp, landusedataname = landusedataname, 
						grouping = grouping))
		input_tools_save(simp, datanametakeovers)
	}
	
	hl_takeovers(simp, runid = simp$sim$runids[1], dataname = dataname,
			starttick = starttick, tickinterval = tickinterval, endtick = endtick,
			datanametakeovers = datanametakeovers, aftnames = simp$mdata$aftNames)
		
}
#' AFT take over fluctations as timeline
#' 
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
		starttick = simp$sim$starttick, tickinterval=5, endtick = simp$sim$endtick,
		datanametakeovers = "csv_aggregateTakeOver") {
	input_tools_load(simp, dataname)
	dataAgg <- get(dataname)
	
	input_tools_load(simp, datanametakeovers)
	dataTakeOvers <- get(datanametakeovers)
	simp$mdata$aftNames <- simp$mdata$aftNames[-1]
	
	if(any(aftnames %in% names(dataTakeOvers))) {
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
	} else {
		warning(paste("There is no AFT giving in data in rData with name", datanametakeovers, "for ID", simp$sim$id))
	}
}
#' Yearly aggregated AFT composition
#' 
#' @param simp 
#' @param dataname
#' @param includeunmanaged if \code{TRUE} the number of unmanaged cells is plotted, too. It is derived from the
#' 			total number of cells obtained
#' @param aggcelldataname
#' @return timelien plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aggregate_aftcompositions <- function(simp, dataname = "csv_aggregateAFTComposition", 
		includeunmanaged = FALSE, aggcelldataname = "dataAgg") {
	# dataname = "dataAggregateAFTComposition"
	input_tools_load(simp, dataname)
	dataComp <- get(dataname)
	
	# filter rows with "?"s
	dataComp[,grep("AFT.", colnames(dataComp))] <- as.numeric(do.call(cbind, lapply(dataComp[,grep("AFT.", 
											colnames(dataComp))], as.character)))
	dataComp <- dataComp[complete.cases(dataComp),]
	
	colnames(dataComp) <- gsub("AFT.", "", colnames(dataComp))
	
	data <- reshape2::melt(dataComp, variable.name="Agent", id.vars= c("Region", "Tick", "Runid", "Scenario"), 
			direction="long")

	d <- aggregate(subset(data, select=c("value")), by = list(AFT = data$Agent, 
					Tick= data$Tick, Runid=data$Runid, Scenario=data$Scenario), 
			"sum", na.rm = TRUE)
	
	if (includeunmanaged) {
		
		input_tools_load(simp, aggcelldataname)
		aggcelldata <- get(aggcelldataname)
		
		cellnum <- sum(aggcelldata[aggcelldata$Tick == aggcelldata$Tick[1], "AFT"])
		
		d <-  plyr::ddply(d, "Tick", function(df, cellnum) {
					rbind(df, data.frame(AFT = "Unmanaged",
									Tick = unique(df$Tick),
									Runid = unique(df$Runid),
									Scenario = unique(df$Scenario),
									value = cellnum - sum(df$value)))
				}, cellnum = cellnum)
		
	}
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
#' 
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
			# TODO use simp$mdata$services or document setting NULL
			colour_legenditemnames = simp$mdata$conversion$services,
			linetype_column = "Type",
			facet_column = "ID",
			filename = paste("AggregateServiceDemand", 
					shbasic::shbasic_condenseRunids(data.frame(data)[, "ID"]), simp$sim$id, sep="_"),
			alpha=0.7)
}
#' Bar plot of number of takeovers per number of trials
#' 
#' Various AFTs as facets.
#' @param simp 
#' @param dataname 
#' @param region 
#' @param facet_ncol
#' @param numboxes number of bins
#' @param numberrange vector of two: considered range of numbers
#' @param trialrange vector of two: considered range of trials
#' @return plot 
#' 
#' @author Sascha Holzhauer
#' @export
hl_gistatistics_singleRegion <- function(simp, dataname = "csv_aggregateGiStatistics", 
		regions = simp$sim$regions, facet_ncol = 1, numboxes = NULL, numberrange = NULL, trialrange = NULL) {
	
	input_tools_load(simp, "csv_aggregateGiStatistics")
	
	csv_aggregateGiStatistics <- csv_aggregateGiStatistics[csv_aggregateGiStatistics[,"Region"] %in% regions,]
	
	if (length(csv_aggregateGiStatistics) == 0) {
		futile.logger::flog.info("No data in csv_aggregateGiStatistics for regions %s! Probably 
				there are no giving ins.",
					paste(regions, collapse="/"),
					name = "craftyr.hl.aggregate.visualise.gistatistics")
		message("No Giving In statistic available.")
	} else {
	
		if (is.null(numboxes)) {
			numboxes <- length(csv_aggregateGiStatistics[, "Trials"])
		}
		
		if (!is.null(trialrange)) {
			csv_aggregateGiStatistics <- csv_aggregateGiStatistics[csv_aggregateGiStatistics$Trials >= trialrange[1] &
							csv_aggregateGiStatistics$Trials <= trialrange[2],]	
		}
		
		## <---- determine bins:
		datbin <- aggregate(subset(csv_aggregateGiStatistics, select=simp$mdata$aftNames[-1]), by = list(
						Trials=csv_aggregateGiStatistics[, "Trials"],
						Runid=csv_aggregateGiStatistics[, "Runid"]),
				FUN=sum)
		melteddatbin <- reshape2::melt(datbin, variable.name="AFT", id.vars= c("Trials", "Runid"), 
				direction="long", value.name = "Number")		
		
		melteddatbin <- melteddatbin[melteddatbin$Number >= numberrange[1] &
						melteddatbin$Number <= numberrange[2],]
		
		# determine breaks to have <numboxes> quantile groups (not considering number per group)
		breaks <- Hmisc::cut2(unique(melteddatbin$Trials), g=numboxes, onlycuts=TRUE)
		#### ---->
		
		dat <- aggregate(subset(csv_aggregateGiStatistics, select=simp$mdata$aftNames[-1]), by = list(
						Trials=cut(csv_aggregateGiStatistics[, "Trials"], breaks = breaks),
						Runid=csv_aggregateGiStatistics[, "Runid"]),
				FUN=sum)
		
		melteddat <- reshape2::melt(dat, variable.name="AFT", id.vars= c("Trials", "Runid"), 
				direction="long", value.name = "Number")
		
		visualise_bars(simp, data = melteddat, y_column = "Number", title = "Giving In Statistics",
				facet_column = "AFT", facet_ncol = facet_ncol, fill_column = "AFT",
				alpha=1.0, x_column = "Trials", ggplotaddons = ggplot2::theme(legend.position="none",
						axis.text.x = element_text(angle = 90, hjust = 1)))
	}
}
#' Bar plot of number of takeovers per number of trials
#' 
#' Various regions as facets.
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
#' Visualise pre-allocation competitiveness per AFT
#' 
#' @param simp 
#' @param dataname 
#' @param maxcompetitiveness either an absoulte value or a percentage given as string (e.g. "90%"). The latter
#' 			selects the lowest X percent.
#' @param afts AFTs to display (as vector of names)
#' @return facet histogram plot 
#' 
#' @inheritParams visualise_competition_prealloc
#' 
#' @author Sascha Holzhauer
#' @export
hl_competitiveness_prealloc <- function(simp, dataname = "csv_preAlloc_rbinded", 
		maxcompetitiveness = "100%", 
		# passed to visualise_competition_prealloc
		numbins = 20,
		facet_ncol = NULL,
		filename = paste("PreAllocationCompetition", simp$sim$id, sep="_"),
		title = NULL,
		ggplotaddons = NULL,
		setfigdims = TRUE,
		afts = simp$mdata$aftNames,
		checkexists = FALSE,
		skipemptybins = TRUE) {
	
	storename <- paste("PreAllocationCompetitionPlot", paste(afts, collapse="-"), numbins, sep="_")
	if (checkexists && input_tools_checkexists(simp, storename)) {
		local({
			input_tools_load(simp, storename)
			
			numTicks <- simp$sim$endtick - simp$sim$starttick + 1
			if(setfigdims) {
				simp$fig$height			<- simp$fig$height * if(is.null(facet_ncol)) numTicks else numTicks/facet_ncol
				simp$fig$width			<- simp$fig$width * if(is.null(facet_ncol)) length(simp$mdata$aftNames) - 1 else facet_ncol 
			}
			simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "bars", sep="/"), filename = filename)
			
			print(get(storename))
			
			simp$fig$close()
		})
		gc()
	} else {
		
		input_tools_load(simp, dataname)
		data <- get(dataname)
		
		# check for table/data.frame
		if("Above" %in% colnames(data) && "Below" %in% colnames(data)) {
			
			suppressWarnings(data$Comp <- as.numeric(levels(data$Comp)[data$Comp]))
			aftIDs = as.numeric(names(simp$mdata$aftNames)[match(afts, simp$mdata$aftNames)])
			data <- data[complete.cases(data) & data$PreAllocLandUseIndex %in% aftIDs,]
			
			if(skipemptybins) {
				data <- data[!(data$Above == 0 & data$Below == 0),]
			}
			
			if (grepl("%", maxcompetitiveness)) {
				maxcompetitiveness <- quantile(data$Comp, 
						as.numeric(gsub("%", "",maxcompetitiveness))/100)
			}
			
			data <- data[data$Comp <= maxcompetitiveness,]
			
			
			visualise_competition_preallocTable(simp, data, facet_ncol = facet_ncol, filename = filename,
					numbins = numbins, title = title, ggplotaddons = ggplotaddons, setfigdims = setfigdims, 
					storename = if(checkexists) storename else NULL)
		} else {
		
			suppressWarnings(data$PreAllocCompetitiveness <-  
							as.numeric(levels(data$PreAllocCompetitiveness)[data$PreAllocCompetitiveness]))
			suppressWarnings(data$PreAllocGivingUpThreshold <-  
							as.numeric(levels(data$PreAllocGivingUpThreshold)[data$PreAllocGivingUpThreshold]))
			
			data <- data[complete.cases(data),]
			
			if (grepl("%", maxcompetitiveness)) {
				maxcompetitiveness <- quantile(data$PreAllocCompetitiveness, 
						as.numeric(gsub("%", "",maxcompetitiveness))/100)
			}
			
			data <- data[data$PreAllocCompetitiveness <= maxcompetitiveness,]
			# data <- data[1:1000,]
			
			data$GU <- 0
			data$GU[data$PreAllocCompetitiveness < data$PreAllocGivingUpThreshold] <- 1
			data$GU <- as.factor(data$GU)
			data$AFT <- simp$mdata$aftNames[match(data$PreAllocLandUseIndex, as.numeric(names(simp$mdata$aftNames)))]
			
			data$PreAllocLandUseIndex <- NULL
			data$PreAllocGivingUpThreshold <- NULL
			
			data <- data[data$AFT %in% afts,]
			
			visualise_competition_prealloc(simp, data, facet_ncol = facet_ncol, filename = filename,
					numbins = numbins, title = title, ggplotaddons = ggplotaddons, setfigdims = setfigdims, 
					storename = if(checkexists) storename else NULL)
		}
	}
}
#' Visualise pre-allocation competitiveness per AFT
#' 
#' Write one file per AFT.
#' @return facet histogram plot in file 
#' 
#' @inheritParams hl_competitiveness_prealloc
#' 
#' @author Sascha Holzhauer
#' @export
hl_competitiveness_preallocPerAft <- function(simp, dataname = "csv_preAlloc_rbinded",
		maxcompetitiveness = "90%", 
		# passed to visualise_competition_prealloc
		numbins = 20,
		facet_ncol = 5,
		filename = paste("PreAllocationCompetition", simp$sim$id, sep="_"),
		title = NULL, ggplotaddons = NULL,
		setfigdims = TRUE,
		afts = simp$mdata$aftNames[-1],
		checkexists = FALSE,
		skipemptybins = TRUE) {
	
	lapply(afts, function(aft) {
				hl_competitiveness_prealloc(
						simp,
						dataname = dataname,
						maxcompetitiveness = maxcompetitiveness,
						numbins = numbins,
						facet_ncol = facet_ncol,
						filename = paste(filename, aft, sep="_"),
						title =  if (!is.null(title)) paste(title, aft) else NULL,
						ggplotaddons = ggplotaddons,
						setfigdims = setfigdims,
						afts = aft,
						checkexists = checkexists,
						skipemptybins = skipemptybins)})	
}
#' Read stored landuse data, calculate and visualise spatial autocorrelation score
#' 
#' NOTE: Needs the cell data to be stored as rData (see \link{input_csv_data}).
#' @param simp 
#' @param celldataname 
#' @param starttick 
#' @param tickinterval 
#' @param endtick 
#' @param linetypecol 
#' @param type 
#' @param regions 
#' @param colourcol 
#' @param titleprefix 
#' @param filenameprefix 
#' @return lines plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aggregate_sa <- function(simp, celldataname = "csv_LandUseIndex_rbinded", 
		starttick = simp$sim$starttick, tickinterval=10, endtick = simp$sim$endtick,
		linetypecol = "Region", type = "Moran",
		regions = simp$sim$regions,
		colourcol = NULL, titleprefix = paste("SpatialAutocorrelation (", type, ")", sep=""), 
			filenameprefix = paste("SpatialAutocorrelation", type, sep="_")) {

	input_tools_load(simp, objectName = celldataname)
	data <- get(celldataname)
	
	scoresdata <- do.call(rbind, lapply(seq(from=starttick, to=endtick, by=tickinterval), 
					function(tick, data, type ,regions) {
						do.call(rbind,lapply(regions, function(tick, region, data, type) {
				tickdata <- data[data$Tick == tick & data$Region %in% region, 
						c(simp$csv$cname_x, simp$csv$cname_y, "LandUseIndex")]
				raster <- craftyr::convert_2raster(simp, tickdata, targetCRS = "+init=EPSG:32632", layers=c(1))
				data.frame(Value = analyse_statistics_sa_local(raster[[1]], type = type),
						Tick = tick,
						Region = region)}, tick = tick, data = data, type = type))
			}, data = data, type = type, regions = regions))

	visualise_lines(simp, scoresdata, "Value", title = paste(titleprefix, simp$sim$rundesc[simp$sim$runid]),
			colour_column = colourcol,
			linetype_column = linetypecol,
			filename = paste(filenameprefix, "_", simp$sim$rundesc[simp$sim$runid], sep=""))
}
#' Visualise stored metric LandUseConnectivity as timelines
#' 
#' @param simp 
#' @param dataname 
#' @param datatype 
#' @param datacolumns 
#' @return plot (and rData)
#' 
#' @author Sascha Holzhauer
#' @export
hl_connectedness <- function(simp, dataname = "csv_aggregated_connectivity", 
		datatype = "LandUseConnectivity", aftcolumns = simp$mdata$aftNames[-1],
		percent = NULL) {
	# dataname = "dataAggregateConnectivity"
	craftyr::hl_lines_from_csv(simp, dataname = dataname, 
			datatype = datatype, datacolumns = aftcolumns, linetypecol = "Region",
			colourcol = "Type", titleprefix = "Connectedness", filenameprefix = "Connectedness288", 
			percent = percent)
}
#' Read and plot normalised per-cell residuals
#' 
#' @param simp 
#' @param filenamemarginalutils 
#' @param filenamePlotPercellDemand 
#' @param filenameNormalisedResiduals 
#' @param capitalfilepartorder 
#' @return plot(s)
#' 
#' @author Sascha HolzhauerVar1
#' @export
hl_normalisedutilities <- function(simp,
		filenamemarginalutils = paste(simp$dirs$output$rdata, "MarginalUtilitiesPerCell.csv", sep = "/"),
		filenamePlotPercellDemand = NULL,
		filenameNormalisedResiduals = paste("NormalisedResiduals", simp$sim$runid, sep="_"),
		capitalfilepartorder = c("regionalisation", "U", "regions", "U", "datatype")) {
	
	marginalUtils <- shbasic::sh_tools_loadorsave(SIP = simp, 
			OBJECTNAME = "csv_MarginalUtilitites_melt",
			PRODUCTIONFUN = input_marginalutilities,
			simp = simp, filename = filenamemarginalutils)

	marginalUtils$Scenario	<- simp$sim$scenario
	names(marginalUtils)[names(marginalUtils)=="Runid"] <- "ID"
	# to distinguish from demand:
	names(marginalUtils)[names(marginalUtils)=="Value"] <- "V"

	input_tools_load(simp, "dataAggregateSupplyDemand")
	data <- convert_aggregate_meltsupplydemand(simp, dataAggregateSupplyDemand)
	data <- aggregate(subset(data, select=c("Value")),
			by = list(ID = data[,"Runid"],
					Tick=data[, "Tick"],  Scenario = data[,"Scenario"],
					Service=data[,"Service"], Type=data[,"Type"]),
			FUN=sum)
	
	simp$sim$filepartorder	<- capitalfilepartorder
	num <- input_csv_param_capitals_cellnumbers(simp, regionpartfromend = 2, regionpartdevider = "_")
	data$Value <- data$Value / sum(num$Cells)
	
	if (!is.null(filenamePlotPercellDemand)) {
		visualise_lines(simp, data, "Value", title = "Per-cell Demand & Supply",
				colour_column = "Service",
				linetype_column = "Type",
				filename = filenamePlotPercellDemand,
				alpha=simp$fig$alpha)
	}
	
	# merge data
	data <- data[data$Type == "Demand",]
	normalised <- merge(marginalUtils, data)
	normalised$Value <-  normalised$V / normalised$Value
	visualise_lines(simp, normalised, "Value", title = "Demand-normalised per-cell Utilities",
			colour_column = "Service",
			linetype_column = "Type",
			filename = filenameNormalisedResiduals,
			alpha=simp$fig$alpha)
}
#' Reads single CSV file with AFT composition and produces timeline figure
#' 
#' @param simp 
#' @param csvfilename 
#' @param title 
#' @param figurefilename 
#' @return figure
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftcomposisition_file <- function(simp, csvfilename, title = "AftComposition", 
		figurefilename = "AftComposition") {
	dataComp <- utils::read.csv(csvfilename, na.strings = simp$csv$nastrings)

	dataComp[,grep("AFT.", colnames(dataComp))] <- as.numeric(do.call(cbind, 
					lapply(dataComp[,grep("AFT.", colnames(dataComp))], as.character)))
	dataComp <- dataComp[complete.cases(dataComp),]
	colnames(dataComp) <- gsub("AFT.", "", colnames(dataComp))
	
	data <-reshape2::melt(dataComp, variable.name="Agent", id.vars= c("Region", "Tick"), direction="long")
	d <- aggregate(subset(data, select=c("value")), by = list(AFT = data$Agent, Tick= data$Tick), "mean", na.rm = TRUE)
	
	############### substitute AFT names by AFT ID
	aftNumbers <- names(simp$mdata$aftNames)
	names(aftNumbers) <- simp$mdata$aftNames
	d$AFT <- aftNumbers[as.character(d$AFT)]
	
	visualise_lines(simp, d, "value", title = title,
			colour_column = "AFT", colour_legenditemnames = simp$mdata$aftNames,
			filename = figurefilename,
			alpha=0.7)
}