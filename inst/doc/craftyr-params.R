## ---- eval=TRUE----------------------------------------------------------
#source("../demo/simp-machine.R")
## read capital data from CSV into raster
#data <- input_csv_param_capitals(simp)
#rasters <- convert_2raster(simp, data, layers = simp$mdata$capitals)
#names(rasters) <- "RegionA"
#visualise_raster_printRawPlots(simp, rasters, legendtitle = "Capitals Example", 
#		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, id="None")

## ---- eval=FALSE---------------------------------------------------------
#  simp$sim$filepartorder <- c("regionalisation", "U", "regions", "U", "datatype")
#  capitaldata <- input_csv_param_capitals(simp)
#  capitaldata <- reshape2::melt(capitaldata, id.vars = c(simp$csv$cname_x, simp$csv$cname_y),
#  	variable.name = "Capital", value.name = "Value")
#  visualise_cells_printPlots(simp, capitaldata, idcolumn = "Capital", valuecolumn = "Value",
#  	title = "Capitals", ncol = 2,
#  	coloursetname=simp$colours$Capital)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_param_capital_map(simp, capitals = simp$mdata$capitals,
#  	filenameorder= c("regionalisation", "U", "regions", "U", "datatype"))

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  functions <- input_xml_param_competition(simp, filename =  "Competition_linear")
#  visualise_competition_funcs(simp, functions)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_printCompetitionFunctions(simp, filename = "Competition_linear")

