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