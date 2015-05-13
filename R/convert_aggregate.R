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
#' @param simp 
#' @param checkexists 
#' @param demanddataname 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_demand <- function(simp, checkexists = TRUE,
		demanddataname = "csv_aggregated_demand") {
	if (!input_tools_checkexists(simp, demanddataname)) {
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
		demand <- reshape2::melt(demand, id.vars=c("Region", "Tick"))
		colnames(demand)[which(colnames(demand)=="value")] <- "Demand"
		demand$variable <- as.factor(simp$mdata$conversion$services[as.character(demand$variable)])
		assign(demanddataname,demand)
		input_tools_save(simp, demanddataname)
	}
}
#' Aggregate and store supply
#' @param simp 
#' @param dataname 
#' @param checkexists 
#' @param demanddataname 
#' @return -
#' 
#' @author Sascha Holzhauer
#' @export
convert_aggregate_supply <- function(simp, dataname = "csv_cell_aggregated", checkexists = TRUE,
		supplydataname = "csv_aggregated_supply") {
	if (!input_tools_checkexists(simp, supplydataname)) {
		input_tools_load(simp, dataname)
		data <- get(dataname)
		
		supply <- data[, colnames(data) %in% c("Tick", "Service.Meat", "Service.Cereal", "Service.Recreation",
						"Service.Timber", "Runid", "Region")]
		
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