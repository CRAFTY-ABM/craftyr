#' Aggregate demand and supply for all regions per tick and store as CSV file
#' 
#' @param simp 
#' @param dataname
#' @return csv file
#' 
#' @author Sascha Holzhauer
#' @export
hl_aggregate_demandsupply_csv <- function(simp, dataname = "dataAggregateSupplyDemand") {
	input_tools_load(simp, dataname)
	data <- get(dataname)

	data <- aggregate(data[,1:(length(simp$mdata$services)*2)], 
			by = list("Tick" = data$Tick), FUN = sum)

	filename <- sprintf("%s/%s_DemandSupply.%s",
		simp$dirs$output$data,
		output_tools_getDefaultFilename(simp),
		"csv")

	shbasic::sh.ensurePath(filename, stripFilename = TRUE)
	utils::write.csv(data, file = filename, row.names = FALSE)
}