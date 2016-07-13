#' Calculates the Shannon Diversity Index for AFT composition.
#' 
#' TODO Consider \code{simp$sim$regions}
#' 
#' @param simp Expects the defintion of one run
#' @param dataname  
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (percental demand supply gap) when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_aggaft_diversity_shannon <- function(simp, dataname = "csv_aggregateAFTComposition", asvector = FALSE) {
	# dataname = "dataAggregateAFTComposition"
	d <- input_processAftComposition(simp, dataname = dataname)
	
	if (length(unique(d$Scenario)) > 1) {
		R.oo::throw.default("Data contains more than one scenario (", unique(d$Scenario), ")")
	}
	if (length(unique(d$Runid)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(d$Runid), ")")
	}
	
	metric <- plyr::ddply(d, "Tick", function(df) {
				# df <- d[d$Tick == 2010,]
				df$value <- df$value/sum(df$value)
				sum = -sum(plyr::ddply(df, "AFT", function(data) {
							data$value * log(data$value)
						})$V1)
			})
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(Metric = "DivLuShannon",
								Tick =  metric$Tick, Value = metric$V1))
}
#' Return proportion of given set of AFTs of all agents (usually not unmanaged cells)
#' 
#' @param simp 
#' @param afts set (vector of chracters) of AFTs whose proportion is to be determined. 
#' @param dataname
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric when \code{asvector == TRUE} 
#' 
#' @author Sascha Holzhauer
#' @export
metric_aggaft_proportions <- function(simp, afts, aftsname = NULL, 
		dataname = "csv_aggregateAFTComposition", asvector = FALSE) {
	# dataname = "dataAggregateAFTComposition"
	d <- input_processAftComposition(simp, dataname = dataname)
	
	if (length(unique(d$Scenario)) > 1) {
		R.oo::throw.default("Data contains more than one scenario (", unique(d$Scenario), ")")
	}
	if (length(unique(d$Runid)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(d$Runid), ")")
	}
	
	metric <- plyr::ddply(d, "Tick", function(df) {
				# df <- d[d$Tick == 2010,]
				value <- sum(df[df$AFT %in% afts,]$value)/sum(df$value)
			})
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
								Metric = paste("ConsProp", if(!is.null(aftsname)) aftsname else 
													paste(afts, collapse="-")),
								Tick =  metric$Tick, Value = metric$V1))
}
#' Calculates the percental demand supply gap for all ticks
#' 
#' @param simp 
#' @param service character, the service the gap is returned for. "Sum" for sum.
#' @param datanamedemand
#' @param datanamesupply
#' @param includesum if \code{TRUE} computes the sum across all services
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (percental demand supply gap) when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_agg_supplydemand_percentage <- function(simp, service = "Sum", datanamedemand = "csv_aggregated_demand",
		datanamesupply = "csv_aggregated_supply", includesum = TRUE, asvector = FALSE) {	
	data <- convert_supplydemand_percentage(simp = simp, datanamedemand = datanamedemand,
			datanamesupply = datanamesupply, includesum = includesum)
	data <- data[data$Service == service,]
	metric <- 100 * data$TotalProduction / data$Demand
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
								Metric = paste("SupplyPercent", service, sep="_"),
								Tick =  metric$Tick, Value = metric$V1))
}
#' Calculates the maximum percental demand supply gap for across services for all ticks
#' 
#' @param simp 
#' @param services vector of character. Limits set of considered services if not \code{NULL}.
#' @param dataname 
#' @param includesum if \code{TRUE} computes the sum across all services
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric (percental demand supply gap) when \code{asvector == TRUE} 
#' 
#' @author Sascha Holzhauer
#' @export
metric_agg_supplydemand_maximum <- function(simp, services = NULL, datanamedemand = "csv_aggregated_demand",
		datanamesupply = "csv_aggregated_supply", includesum = TRUE, asvector = FALSE) {

	data <- convert_supplydemand_percentage(simp = simp, datanamedemand = datanamedemand,
			datanamesupply = datanamesupply, includesum = includesum)
	
	if (!is.null(services)) {
		data <- data[data$Service %in% services,]
	}
	metric <- aggregate(data[, c("Demand", "TotalProduction")], list(
					Tick	= data$Tick,
					Region	= data$Region,
					ID		= data$ID
			),
			FUN=max)
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
								Metric = paste("SupplyMaxGap", if (!is.null(services)) "_", services, sep=""),
								Tick =  metric$Tick, Value = metric$V1))
}
#' TODO
#' 
#' @param simp
#' @param region the region to calculate the metric for. If \code{NULL} the mean across regions is returned.
#' @param datanamesupply 
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_agg_supplyperreg_simpson <- function(simp, region = NULL, datanamesupply = "csv_aggregated_supply",
		asvector = FALSE) {
	
	input_tools_load(simp, objectName=datanamesupply)
	supply <- get(datanamesupply)
	
	if (length(unique(supply$ID)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(supply$ID), ")")
	}
	
	metric <- plyr::ddply(supply, c("Region", "Tick"), function(df) {
				# df <- demand[demand$Tick == 2010 & demand$Region == "DE13",]
				sum <- 1 - sum((df$TotalProduction/sum(df$TotalProduction))^2)
			})
	
	if (is.null(region)) {
		metric <- aggregate(metric$V1, by = list(Tick = metric$Tick), FUN=mean)
		names(metric)[names(metric)=="x"] <- "V1"
	} else {
		metric <- metric[metric$Region == region,]
	}
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
			Metric = paste("DivSupplyPerRegSimpson",  if (!is.null(region)) "_", region, sep=""),
			Tick =  metric$Tick, Value = metric$V1))
}
#' TODO
#' 
#' @param simp
#' @param service the service to calculate the metric for. If \code{NULL} the mean across services is returned.
#' @param datanamesupply 
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_agg_supplyaccrossreg_simpson <- function(simp, service = NULL, datanamesupply = "csv_aggregated_supply",
		asvector = FALSE) {
	# get regional service supply
	input_tools_load(simp, objectName=datanamesupply)
	supply <- get(datanamesupply)
	
	if (length(unique(supply$ID)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(supply$ID), ")")
	}
	
	# get number of cells per region
	simp$sim$filepartorder 		<- c("regions", "U", "datatype")
	cellnumbers <- input_csv_param_capitals_cellnumbers(simp, regionpartfromend = 2, regionpartdevider = "_")
	
	# normalise regional service supply by number of cells within each region
	cellnumbernormalisation <- setNames(cellnumbers$Cells / max(cellnumbers$Cells), cellnumbers$Region)	
	supply$TotalProduction <- supply$TotalProduction/cellnumbernormalisation[
			match(supply$Region,names(cellnumbernormalisation))]
	
	# calcualate simpson's metric
	metric <- plyr::ddply(supply, c("Service", "Tick"), function(df) {
				# df <- demand[demand$Tick == 2010 & demand$Service == "Meat",]
				sum <- (1 / sum((df$TotalProduction/sum(df$TotalProduction))^2)) / 
						length(unique(df$Region))
			})
	
	if (is.null(service)) {
		metric <- aggregate(metric$V1, by = list(Tick = metric$Tick), FUN=mean)
		names(metric)[names(metric)=="x"] <- "V1"
	} else {
		metric <- metric[metric$Service == service,]
	}
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
		Metric = paste("DivSupplyAcrossRegSimpson", if (!is.null(service)) "_", service, sep=""), 
		Tick =  metric$Tick, Value = metric$V1))
}

#' Regional land use diversity
#' 
#' @param simp 
#' @param region the region to calculate the metric for. If \code{NULL} the mean across regions is returned.
#' @param dataname  
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_aggaft_diversity_simpson <- function(simp, region = NULL, dataname = "csv_aggregateAFTComposition",
		asvector = FALSE) {
	# dataname = "dataAggregateAFTComposition"
	d <- input_processAftComposition(simp, dataname = dataname, aggregateRegions=FALSE)
	
	if (length(unique(d$Scenario)) > 1) {
		R.oo::throw.default("Data contains more than one scenario (", unique(d$Scenario), ")")
	}
	if (length(unique(d$Runid)) > 1) {
		R.oo::throw.default("Data contains more than one run ID (", unique(d$Runid), ")")
	}
	
	metric <- plyr::ddply(d, c("Region", "Tick"), function(df) {
				# df <- demand[demand$Tick == 2010 & demand$Region == "DE13",]
				sum <- 1 - sum((df$value/sum(df$value))^2)
			})
	
	if (is.null(region)) {
		metric <- aggregate(metric$V1, by = list(Tick = metric$Tick), FUN=mean)
		names(metric)[names(metric)=="x"] <- "V1"
	} else {
		metric <- metric[metric$Region == region,]
	}
	
	return(if(asvector) setNames(metric$V1, metric$Tick) else data.frame(
								Metric = paste("DivLuPerRegSimpson", if (!is.null(region)) "_", region, sep=""),
								Tick =  metric$Tick, Value = metric$V1))
}
#' Service delivery efficiency
#' 
#' @param simp
#' @param service the service to calculate the metric for. If \code{NULL} the mean across services is returned.
#' @param datanamesupply 
#' @param datanameaft
#' @param asvector if \code{TRUE} metric is returned as named vector
#' @return data.frame with cols Metric, Ticks and Value or named vector (ticks) of metric when \code{asvector == TRUE}
#' 
#' @author Sascha Holzhauer
#' @export
metric_agg_regionalsupply_efficiency <- function(simp, service = NULL, 
		datanamesupply = "csv_aggregated_supply",
		datanameaft = "csv_aggregateAFTComposition",
		asvector = FALSE) {
	
	# get regional service supply
	input_tools_load(simp, objectName=datanamesupply)
	supply <- get(datanamesupply)
	supply <- aggregate(supply$TotalProduction, by = list(Service=supply$Service, Tick=supply$Tick), FUN=sum)
	names(supply)[names(supply) == "x"] <- "Supply"
	
	d <- input_processAftComposition(simp, dataname = datanameaft, aggregateRegions=FALSE)
	d <- aggregate(d$value, by = list(AFT=d$AFT, Tick=d$Tick), FUN=sum)
	
	# assign producing AFTs to services
	producingAFTs <- sapply(simp$mdata$services, function(x) c())
	for (aft in simp$mdata$aftNames[-1]) {
		# aft = afts[3]
		productivities <- input_csv_param_productivities(simp, aft = aft, 
				filenameprefix = "AftProduction_",
				filenamepostfix = "_multi_medium")
		for (service_ in productivities[productivities$Production > 0, "X"]) {
			producingAFTs[[service_]][length(producingAFTs[[service_]]) + 1] <- aft
		}
	}
	
	# determine number of supplying cells per service (using productivities)
	servicecells <- plyr::ddply(d, c("Tick"), function(df) {
		# df <- demand[demand$Tick == 2010 & demand$Region == "DE13",]
		sapply(simp$mdata$services, function(service)
			sum(df[df$AFT %in% producingAFTs[[service]], "x"]))
	})
	
	servicecells <- melt(servicecells, id.vars="Tick", variable.name ="Service",
		measure.vars = simp$mdata$services, value.name = "Servicecells")

	data <- merge(supply, servicecells)

	# calculate initial mean supply per supplying cell (all regions)
	data$PerCellSupply <- data$Supply / data$Servicecells
	initialMean <- data[data$Tick == simp$sim$starttick, c("Service", "PerCellSupply")]
	data$Metric <- data$PerCellSupply / initialMean[match( data$Service, initialMean$Service), "PerCellSupply"]
	
	if (is.null(service)) {
		data <- aggregate(data$Metric, by = list(Tick = data$Tick), FUN=mean)
		names(data)[names(data)=="x"] <- "Metric"
	} else {
		data <- data[data$Service == service,]
	}
	
	return(if(asvector) setNames(data$MEtric, data$Tick) else data.frame(
								Metric = paste("EffSupply", if (!is.null(service)) "_", service, sep=""),
								Tick =  data$Tick, Value = data$Metric))
}
	
