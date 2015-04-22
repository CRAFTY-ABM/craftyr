#' Reads marginal utilties and prepares the data for plotting. Stores data and returns.
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
input_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, "MarginalUtilities.csv", sep="/")) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape::melt(utilities, variable_name="Service", 
			id.vars= c("Year"), 
			direction="long")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$Runid <- 0
	csv_MarginalUtilitites_melt
}