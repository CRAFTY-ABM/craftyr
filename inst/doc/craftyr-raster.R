## ----, eval=TRUE, results="hide"-----------------------------------------
library(craftyr)
simp <- param_getExamplesSimp()
raster_landUseIndex <- input_raster_output(simp,
		datatype = "Agent", 
		dataname = "SerialID",
		starttick = 2010)
names(raster_landUseIndex) <- simp$sim$rundesc
input_tools_save(simp, "raster_landUseIndex")

## ----, eval=TRUE, results="hide"-----------------------------------------
input_tools_load(simp, "raster_landUseIndex")

## ----, eval=TRUE, fig.width=7--------------------------------------------
landUseIndexData <- convert_raster_getAftNumbers(raster_landUseIndex)
landUseIndexData$AFT <- factor(landUseIndexData$AFT)
visualise_lines(simp, landUseIndexData, "Value", title = "Total AFT composition",
		colour_column = "AFT",
		colour_legenditemnames = simp$mdata$aftNames,
		linetype_column = "Runid",
		linetype_legendtitle = simp$sim$rundesclabel,
		linetype_legenditemnames = simp$sim$rundesc,
		filename = paste("TotalAftComposition", 
				shbasic::shbasic_condenseRunids(data.frame(aftData)[, "ID"]), sep="_"),
		alpha=0.7)

