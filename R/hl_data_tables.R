#' Output aggregated supply as table with services as columns
#' 
#' @param simp 
#' @param dataname name for retrieving supply data (RData)
#' @param celldataname name for retrieving supply data (celldata)
#' @param sourcedataname name for storing supply data
#' @param latex 
#' @return data.frame or printed LaTeX table
#' 
#' @author Sascha Holzhauer
#' @export
hl_table_supply <- function(simp, 
		celldataname = "csv_cell_aggregated",
		sourcedataname = "dataAggregateSupplyDemand",
		dataname = "csv_aggregated_supply",
		latex = TRUE) {
	# dataname = "dataAggregateSupplyDemand"
	
	convert_aggregate_supply(simp, celldataname = celldataname, checkexists = TRUE,
			supplydataname = dataname, sourcedataname = sourcedataname)
	
	input_tools_load(simp, dataname)
	data <- get(dataname)
	d <- data[, grep("ServiceSupply",colnames(data), fixed=T)]
	d <- aggregate(d, by = list(Tick = data$Tick), FUN= "sum")
	
	colnames(d) <- gsub("ServiceSupply.", "", colnames(d))
	
	if (latex) {
		table <- xtable::xtable(d,
				label= "data.supply", 
				caption= "Service supply"
		)
		
		print(table, sanitize.colnames.function = NULL,
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
		
		print(table, sanitize.colnames.function = NULL,
				sanitize.rownames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	} else {
		return(d)
	}
}
#' Output aggregated number of AFTs as table with AFTs as columns
#' 
#' @param simp 
#' @param dataname name for retrieving aggregated AFT data (RData)
#' @param aggcelldataname name for retrieving aggregated cell data (celldata) to calculate unmanagend
#' @param includeunmanaged included numbers for unmanaged cells if TRUE
#' @param#' aftasnumber If TRUE show AFT as numbers instead of names
#' @param afts list of AFTs to limit set of shown AFTs
#' @param latex output as latex table string
#' @return data.frame or printed LaTeX table
#' 
#' @author Sascha Holzhauer
#' @export
hl_table_afts <- function(simp, dataname = "csv_aggregateAFTComposition", aftasnumber = TRUE,
		includeunmanaged = FALSE, aggcelldataname = "dataAgg", afts = NULL, latex = TRUE) {
	
	d <- input_processAftComposition(simp, dataname = dataname)
	operator = if (any(d$value > 1.0)) "sum" else "mean"
	
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
	# afts <- c("COF_Cereal", "NCOF_Livestock")
	if (!is.null(afts)) {
		d <- d[d$AFT %in% afts,]
	}
	
	# substitute AFT names by AFT ID
	aftNumbers <- names(simp$mdata$aftNames)
	names(aftNumbers) <- simp$mdata$aftNames
	
	if (aftasnumber) {
		d$AFT <- aftNumbers[as.character(d$AFT)]
	}
	
	d <- reshape2::dcast(d, Tick~AFT, mean,
			value.var = "value")
	
	
	if (latex) {
		table <- xtable::xtable(d,
				label= "data.afts", 
				caption= "Numbers of allocated AFTs"
		)
		
		print(table, sanitize.colnames.function = NULL,
				sanitize.rownames.function = identity,
				include.rownames = FALSE,
				table.placement = "H")
	} else {
		return(d)
	}
}