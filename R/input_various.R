#' Reads marginal utilties and prepares the data for plotting. Stores data and returns.
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
input_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, "MarginalUtilities.csv", sep="/")) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape2::melt(utilities, variable.name="Service", 
			id.vars= c("Year"), 
			direction="long",
			value.name = "Value")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$ID <- simp$sim$runid
	csv_MarginalUtilitites_melt
}

#' Proceses/Aggregates stored, aggregated AFT composistion data. Checks whether data contains absolute numbers or
#' proportions.
#' 
#' @param simp 
#' @param dataname  
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
input_processAftComposition <- function(simp, dataname = "csv_aggregateAFTComposition", aggregateRegions=TRUE) {
	
	input_tools_load(simp, dataname)
	dataComp <- get(dataname)
	
	# filter rows with "?"s
	dataComp[,grep("AFT.", colnames(dataComp))] <- as.numeric(do.call(cbind, lapply(dataComp[,grep("AFT.", 
											colnames(dataComp))], as.character)))
	dataComp <- dataComp[complete.cases(dataComp),]
	
	if (nrow(dataComp) == 0) {
		R.oo::throw.default("No row left in %s after removing incomplete rows!", dataname, "input.aft.composition")
	}
	
	colnames(dataComp) <- gsub("AFT.", "", colnames(dataComp))
	
	data <- reshape2::melt(dataComp, variable.name="Agent", id.vars= c("Region", "Tick", "Runid", "Scenario"), 
			direction="long")
	
	operator = if (any(data$value > 1.0)) "sum" else "mean"
	bylist <- list(AFT = data$Agent, 
			Tick= data$Tick, Runid=data$Runid, Scenario=data$Scenario)
	if (!aggregateRegions) {
		bylist <- append(bylist, list(Region=data$Region))
	}
	d <- aggregate(subset(data, select=c("value")), by = bylist, 
			operator, na.rm = TRUE)
	return(d)
}