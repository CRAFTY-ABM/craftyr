#' Output aggregated supply as table with services as columns
#' 
#' @param simp 
#' @param dataname 
#' @param latex 
#' @return data.frame or printed LaTeX table
#' 
#' @author Sascha Holzhauer
#' @export
hl_table_supply <- function(simp, dataname = "csv_aggregateServiceDemand", latex = TRUE) {
	# dataname = "dataAggregateSupplyDemand"
	input_tools_load(simp, dataname)
	data <- get(dataname)
	d <- data[, grep("ServiceSupply",colnames(data), fixed=T)]
	d <- aggregate(d, by = list(Tick = data$Tick), FUN= "sum")
	
	if (latex) {
		table <- xtable::xtable(d,
				label= "data.supply", 
				caption= "Service supply"
		)
		
		print(table, sanitize.colnames.function = identity,
				sanitize.rownames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	} else {
		return(d)
	}
}
#' Output aggregated demand as table with services as columns
#' 
#' @param simp 
#' @param dataname 
#' @param latex 
#' @return data.frame or printed LaTeX table
#' 
#' @author Sascha Holzhauer
#' @export
hl_table_demand <- function(simp, dataname = "csv_aggregateServiceDemand", latex = TRUE) {
	# dataname = "dataAggregateSupplyDemand"
	input_tools_load(simp, dataname)
	data <- get(dataname)
	d <- data[, grep("Demand",colnames(data), fixed=T)]
	d <- aggregate(d, by = list(Tick = data$Tick), FUN= "sum")
	
	if (latex) {
		table <- xtable::xtable(d,
				label= "data.demand", 
				caption= "Service demand"
		)
		
		print(table, sanitize.colnames.function = identity,
				sanitize.rownames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	} else {
		return(d)
	}
}