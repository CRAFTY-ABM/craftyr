#' Create maps of initial capital levels
#' @param simp 
#' @param capitals 
#' @param filenameorder 
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_param_capital_map <- function(simp, capitals = simp$mdata$capitals, 
		filenameorder = c("regionalisation", "U", "regions", "U", "datatype")) {
	simp$sim$filepartorder <- filenameorder
	
	capitalData <- input_csv_param_capitals(simp, capitals)
	melted <- reshape2::melt(capitalData, id.vars = c(simp$csv$cname_x, simp$csv$cname_y),
			variable.name = "Capital", value.name = "Value")
	visualise_cells_printPlots(simp, melted, idcolumn = "Capital", valuecolumn = "Value",
			title = "Capitals", ncol = 2, 
			coloursetname=simp$colours$Capital)
}