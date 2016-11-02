#' Melts demand and supply data and splits service columns into type and service
#' 
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
#' @param simp considered elements: \itemize{
#' 			\item{\code{simp$mdata$conversion$services}}
#' 			\item{\code{\link{input_tools_load}}}
#' 			\item{\code{\link{input_csv_param_demand}}}}
#' @param checkexists
#' @param forceparam don't attempt to use stored rData.
#' @param demanddataname 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_demand <- function(simp, checkexists = TRUE, forceparam = FALSE,
		demanddataname = "csv_aggregateServiceDemand", sourcedataname = "dataAggregateSupplyDemand") {
	if (!checkexists | !input_tools_checkexists(simp, demanddataname)) {
		# check for supply data as rData
		if (!forceparam & input_tools_checkexists(simp, sourcedataname)) {
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
		
		demand <- reshape2::melt(demand, id.vars=c("Region", "Tick"), variable.name = "Service",
				value.name = "Demand")
		
		demand$Service <- as.character(demand$Service)
		demand$Service[as.character(demand$Service) %in% names(simp$mdata$conversion$services)] <- 
				simp$mdata$conversion$services[as.character(demand$Service[
						as.character(demand$Service) %in% names(simp$mdata$conversion$services)])]
		demand$Service <- as.factor(demand$Service)
		assign(demanddataname,demand)
		input_tools_save(simp, demanddataname)
	}
}
#' Aggregate and store supply data
#' 
#' Tries to load supply from stored rData (usually from separate supply and demand CSV outputs).
#' If rData not present at \code{sourcedataname} it uses cell data.
#' 
#' @param simp considered elements: \itemize{
#' 			\item{\code{simp$mdata$conversion$services}}
#' 			\item{\code{\link{input_tools_load}}}}
#' @param celldataname aggregated cell CSV data
#' @param checkexists 
#' @param supplydataname name for storing supply data
#' @param sourcedataname name for retrieving data (rData)
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
			
			input_tools_load(simp, celldataname)
			data <- get(celldataname)
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
#' Calculates the percental demand supply gap for all ticks
#' 
#' @param simp 
#' @param dataname 
#' @param includesum if \code{TRUE} computes the sum across all services
#' @return named (ticks) vector of percental demand supply gap
#' 
#' @author Sascha Holzhauer
#' @export
convert_supplydemand_percentage <- function(simp, datanamedemand = "csv_aggregated_demand",
		datanamesupply = "csv_aggregated_supply", includesum = TRUE, asvector = FALSE) {	
	input_tools_load(simp, objectName=datanamedemand)
	input_tools_load(simp, objectName=datanamesupply)
	
	colnames(csv_aggregated_demand)[colnames(csv_aggregated_demand) == "variable"] <- "Service"
	data <- merge(csv_aggregated_supply, csv_aggregated_demand)
	
	if (length(unique(data$Scenario)) > 1) {
		R.oo::throw.default("Data contains more than one scenario (", unique(data$Scenario), ")")
	}
	if (length(unique(data$Runid)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(data$Runid), ")")
	}
	
	if(includesum) {
		sum <- aggregate(data[, c("Demand", "TotalProduction")], list(
						Tick	= data$Tick,
						Region	= data$Region,
						ID		= data$ID
				),
				FUN=sum)
		sum$Service <- "Total"
		data <- rbind(data, sum)
	}
	
	data$Percentage <-  100 * data$TotalProduction / data$Demand
	return(if(asvector) setNames(data$Percentage, data$Tick) else data)
	
}
#' Extracts numbers of take overs for every pair of AFT for every tick from stored cell csv data
#' Requires land use indices stored in an object whose name is given by \code{landusedataname}.
#' 
#' Scenario, Runid and Region are preserved.
#' @param simp 
#' \itemize{
#'  \item \code{\link{input_tools_load}}
#'  \item \code{simp$mdata$aftNames}
#' }
#' @param landusedataname Name of stored land use indices object
#' @param grouping adjusts which columns are considered for aggregatation
#' @return data.frame with previous AFT as row and resulting AFT as column
#' 
#' @family takeovers
#' @author Sascha Holzhauer
#' @export
convert_aggregate_takeovers <- function(simp, landusedataname = "csv_LandUseIndex_rbinded",
		grouping = c("Scenario", "Runid", "Region")) {
	# <---- test data
#	simp <- param_getExamplesSimp()
#	cdata <- input_csv_data(simp, dataname = NULL, datatype = "Cell", columns = "LandUseIndex",
#			pertick = TRUE, starttick = 2000, endtick = 2020, tickinterval = 10,
#			attachfileinfo = TRUE, bindrows = TRUE)
#	rownames(cdata) <- NULL
	# test data ---->
	
	input_tools_load(simp, landusedataname)
	cdata <- get(landusedataname)
	afts <- as.numeric(names(simp$mdata$aftNames))
	
	cdata <- cdata[,c("LandUseIndex", grouping, "Tick")]
	rownames(cdata) <- NULL
	
	transitions <- plyr::ddply(cdata, grouping, function(cd) {
		# cd <- cdata[cdata$Region == "region",] 
		results <- list()
		for (tick in 1:(length(unique(cd$Tick)) - 1)) {
			# tick = 1
			ticks = sort(unique(cd$Tick))[tick:(tick + 1)]
			result <- sapply(afts, function(fromAFT, ticks) {
							# fromAFT = afts[3]
							data <- sapply(afts, 
								function(fromAFT, toAFT, ticks) {
									# toAFT = afts[3]
									data.frame(number = if(fromAFT == toAFT) 0 else 
														sum(cd[cd$Tick == ticks[1], "LandUseIndex"] == fromAFT & 
													cd[cd$Tick == ticks[2], "LandUseIndex"] == toAFT),
												AFT = setNames(simp$mdata$aftNames[as.character(fromAFT)], NULL),
												toAFT = setNames(simp$mdata$aftNames[as.character(toAFT)], NULL),
												Tick = ticks[2])
								}, fromAFT = fromAFT, ticks = ticks, simplify = FALSE)
							do.call(rbind, data)
						}, ticks = ticks, simplify = FALSE)
			results <- do.call(rbind, append(list(results),result))
		}
		results
	})
	
	transitions <- reshape2::dcast(transitions, as.formula(paste("Tick + ", paste(grouping, collapse=" + "),
							" + AFT ~ toAFT", sep="")), 
					fun.aggregate = sum, margins = NULL,
			subset = NULL, fill = NULL, drop = TRUE, value.var = "number")
}