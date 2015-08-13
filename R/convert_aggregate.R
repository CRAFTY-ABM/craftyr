#' Melts demand and supply data and splits service columns into type and service.
#' @param simp 
#' @param data 
#' @return melted data.frame
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_meltsupplydemand <- function(simp, data) {
	data <-reshape2::melt(data, variable.name="Service", 
			id.vars= c("Region", "Tick", "Runid", "Scenario"), 
			direction="long")
	names(data)[names(data)=="value"] <- "Value"
	data$Service 	<- levels(data$Service)[data$Service]
	splitted 		<- read.table(text = data$Service, sep = ".", colClasses = "character")
	data$Type 		<- splitted[,1]
	data$Service 	<- splitted[,2]
	data
}
#' Aggregate and store demand
#' 
#' Tries to load demand from stored rData (usually from separate supply and demand CSV outputs).
#' If rData not present at \code{sourcedataname} it uses param data.
#' 
#' @param simp 
#' @param checkexists 
#' @param demanddataname 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_demand <- function(simp, checkexists = TRUE,
		demanddataname = "csv_aggregated_demand", sourcedataname = "dataAggregateSupplyDemand") {
	if (!checkexists | !input_tools_checkexists(simp, demanddataname)) {
		# check for supply data as rData
		if (input_tools_checkexists(simp, sourcedataname)) {
			futile.logger::flog.info("Using rData (%s)",
					sourcedataname,
					name = "craftyr.convert.aggregate.demand")
			
			input_tools_load(simp, sourcedataname)
			demand <- get(sourcedataname)
			# exclude supply data:
			demand <- demand[, grepl("Demand", colnames(demand)) | colnames(demand) %in% c("Region", "Tick")]
			# adjust demand colnames:
			colnames(demand) <- gsub("Demand.", "", colnames(demand))
		} else {
			futile.logger::flog.info("Using param data",
					name = "craftyr.convert.aggregate.demand")
			
			simp$sim$filepartorder_demands <- c("regionalisation", "U", "scenario", "U", "datatype", "U", "regions")
			demand <- input_csv_param_demand(simp)
			demand <- lapply(demand, function(x) {
						x$Region <- substr(x$filename[1],
								nchar(x$filename[1])-5,nchar(x$filename[1])-4)
						x$filename <- NULL
						x
					})
			demand <- do.call(rbind, demand)
			colnames(demand)[which(colnames(demand)=="Year")] <- "Tick"
		}
		
		demand <- reshape2::melt(demand, id.vars=c("Region", "Tick"))
		colnames(demand)[which(colnames(demand)=="value")] <- "Demand"
		demand$variable <- as.factor(simp$mdata$conversion$services[as.character(demand$variable)])
		assign(demanddataname,demand)
		input_tools_save(simp, demanddataname)
	}
}
#' Aggregate and store supply data
#' 
#' Tries to load supply from stored rData (usually from separate supply and demand CSV outputs).
#' If rData not present at \code{sourcedataname} it uses cell data.
#' 
#' @param simp 
#' @param celldataname aggregated cell CSV data
#' @param checkexists 
#' @param supplydataname 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_supply <- function(simp, celldataname = "csv_cell_aggregated", checkexists = TRUE,
		supplydataname = "csv_aggregated_supply", sourcedataname = "dataAggregateSupplyDemand") {
	if (!input_tools_checkexists(simp, supplydataname)) {
		# check for supply data as rData
		if (input_tools_checkexists(simp, sourcedataname)) {
			futile.logger::flog.info("Using rData (%s)",
					sourcedataname,
					name = "craftyr.convert.aggregate.supply")
			
			input_tools_load(simp, sourcedataname)
			data <- get(sourcedataname)
			
			# exclude supply data:
			supply <- data[, grepl("ServiceSupply", colnames(data)) | colnames(data) %in% c("Region", "Tick", "Runid")]
			# adjust demand colnames:
			colnames(supply) <- gsub("Supply", "", colnames(supply))
			
		} else {
			futile.logger::flog.info("Using cellData (%s)",
					celldataname,
					name = "craftyr.convert.aggregate.supply")
			
			input_tools_load(simp, dataname)
			data <- get(dataname)
			supply <- data[, colnames(data) %in% c("Tick", "Service.Meat", "Service.Cereal", "Service.Recreation",
							"Service.Timber", "Runid", "Region")]
		}
		
		supply <- reshape2::melt(supply, id.vars=c("Runid", "Tick", "Region"))
		supply$variable <- simp$mdata$conversion$services[supply$variable]
		colnames(supply)[which(colnames(supply)=="value")] <- "TotalProduction"
		
		supplyAgg <- aggregate(subset(supply, select=c("TotalProduction")), by = 
						list(Tick=supply[, "Tick"], Region = supply[,"Region"], ID=supply[,"Runid"], 
								Service = supply[,"variable"]), FUN=sum)
		assign(supplydataname,supplyAgg)
		input_tools_save(simp, supplydataname)
	}
}
convert_aggregate_takeovers <- function(simp, landusedataname = "csv_LandUseIndex_rbinded", regions = simp$sim$regions) {
	# <---- test data
	simp <- param_getExamplesSimp()
	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
			attachfileinfo = TRUE, bindrows = TRUE)
	rownames(cdata) <- NULL
	
	###
	
	simp <- param_getExamplesSimp()
	csv_aggregateTakeOvers <- input_csv_data(simp, dataname = NULL, datatype = "TakeOvers", pertick = FALSE,
			bindrows = TRUE,
			skipXY = TRUE)
	# test data ---->
	
	input_tools_load(simp, dataname)
	cdata <- get(dataname)
	
	# TODO aggregate/count changes in land use
	
	td <- matrix(c(1,2,1,2, 1,2,2,2, 4,2,2,4, 4,1,5,1), ncol= 4, byrow = TRUE)
	rle(td[1,])
	rle(td[4,])
	
	afts <- names(simp$mdata$aftNames)
	
	transitions <- aft
	
	for (tick in 2:length(unique(cdata$Tick))) {
		ticks = unique(cdata$Tick)[tick:(tick + 1)]
		sapply(afts, function(x, ticks) sapply(afts, function(x, y, ticks) {
					cat(x, y, "\n")
					sum(cdata[cdata$Tick == ticks[1], "LandUseIndex"] == x & 
									cdata[cdata$Tick == ticks[2], "LandUseIndex"] == y)
				}, x = x, ticks = ticks), ticks = ticks)
	}
	
}