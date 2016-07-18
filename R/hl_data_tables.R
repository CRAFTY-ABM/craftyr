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
#' @param demanddataname passed to \link{convert_aggregate_demand}
#' @param dataname passed to \link{convert_aggregate_demand} as \code{sourcedataname}
#' @param latex if \code{FALSE} return the data.frame of demands instead of LaTeX table.
#' @return data.frame or printed LaTeX table
#' 
#' @author Sascha Holzhauer
#' @export
hl_table_demand <- function(simp, dataname = "dataAggregateSupplyDemand", 
		demanddataname = "csv_aggregateServiceDemand", latex = TRUE, forceparam = FALSE, regionset = NULL) {
	# dataname = "dataAggregateSupplyDemand"
	convert_aggregate_demand(simp, demanddataname = demanddataname, sourcedataname = dataname,
			forceparam = forceparam)
	input_tools_load(simp, demanddataname)
	data <- get(demanddataname)
	colnames(data)[colnames(data)=="variable"] <- "Service"
	d <- reshape2::dcast(data, formula=Tick+Region~Service, value.var="Demand")
	
	if (is.null(regionset)) {
		d <- aggregate(d[,!colnames(d) %in% c("Region", "Tick")], by = list(Tick = d$Tick), FUN= "sum")
	} else {
		d <- d[d$Region %in% regionset,]
	}
	
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