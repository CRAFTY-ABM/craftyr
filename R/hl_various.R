#' Reads marginal utilties and prepares the data for plotting. Stores data and returns.
#' @param simp 
#' @param filename 
#' @return data.frame
#' 
#' @author Sascha Holzhauer
#' @export
hl_marginalutilities <- function(simp, filename = paste(simp$dirs$output$rdata, "MarginalUtilities.csv", sep="/"),
		storerdata = TRUE) {
	utilities = read.csv(filename, colClasses = "numeric")
	csv_MarginalUtilitites_melt = reshape::melt(utilities, variable_name="Service", 
			id.vars= c("Year"), 
			direction="long")
	colnames(csv_MarginalUtilitites_melt)[colnames(csv_MarginalUtilitites_melt) == "Year"] <- "Tick"
	csv_MarginalUtilitites_melt$Runid <- 0
	
	if (storerdata) {
		input_tools_save(simp, "csv_MarginalUtilitites_melt")
	}
	
	visualise_lines(simp, csv_MarginalUtilitites_melt, "value", title = "Marginal Utilities",
			colour_column = "Service",
			filename = paste("MarginalUtilities", sep=""),
			alpha=0.7)
}