#' AFT map for 2010 and 2040 with differences
#' @param simp 
#' @param dataname 
#' @return map plot
#' 
#' @author Sascha Holzhauer
#' @export
hl_aftmap <- function(simp, dataname = "csv_LandUseIndex_rbinded") {
	
	input_tools_load(simp, dataname)
	cdata <- get(dataname)
	
	cdata$Region <- NULL
	cdata$Scenario <- NULL
	
	cdata <- split(cdata, list(cdata$Tick,cdata$Runid))
	
	cdata <- lapply(cdata, function(x) {x$Runid <- NULL; x})
	cdata <- lapply(cdata, function(x) {x$Tick <- NULL; x})
	
	diffcells <<- cdata$"2010.3"
	diffcells$LandUseIndex <<- NA
	
	cdata$"2010.3"$ID <- as.factor("Hetero_2010")
	cdata$"2040.3"$ID <- as.factor("Hetero_2040")
	
	diffhetero <- diffcells
	diffhetero$LandUseIndex[cdata$"2040.3"$LandUseIndex - cdata$"2010.3"$LandUseIndex != 0] <- 3
	diffhetero$ID <- as.factor("Diff_Hetero_2040-2010")
	toplot_hetero <- list(cdata$"2010.3", cdata$"2040.3", diffhetero)
	
	rm(diffhetero)
	rm(cdata)
	rm(diffcells)
	
	visualise_cells_printPlots(simp, toplot_hetero, idcolumn = "ID",
			title = "EU-Hetero", legendtitle = "AFTs",
			factorial= TRUE, omitaxisticks = TRUE, ncol = 1,
			legenditemnames = simp$mdata$aftNames, coloursetname="AFT")
}