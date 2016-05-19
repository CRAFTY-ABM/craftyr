#' Create maps of initial capital levels
#' @param simp 
#' @param capitals 
#' @param filenameorder 
#' @param returnplot if true the ggplot object is returned
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_param_capital_map <- function(simp, capitals = simp$mdata$capitals, 
		filenameorder = c("regionalisation", "U", "regions", "U", "datatype"),
		returnplot = FALSE) {
	
	simp$sim$filepartorder <- filenameorder
	
	capitalData <- input_csv_param_capitals(simp, capitals)
	melted <- reshape2::melt(capitalData, id.vars = c(simp$csv$cname_x, simp$csv$cname_y),
			variable.name = "Capital", value.name = "Value")
	p1 <- visualise_cells_printPlots(simp, melted, idcolumn = "Capital", valuecolumn = "Value",
			title = "Capitals", ncol = 2, 
			coloursetname=simp$colours$Capital, returnplot = returnplot)
	if (returnplot) return(p1)
}