## ---- eval=FALSE---------------------------------------------------------
#  source("../demo/simp-machine.R")
#  # read capital data from CSV into raster
#  data <- input_csv_param_capitals(simp)
#  rasters <- convert_2raster(simp, data, layers = simp$mdata$capitals)
#  names(rasters) <- "RegionA"
#  visualise_raster_printRawPlots(simp, rasters, legendtitle = "Capitals Example",
#  		factorial= FALSE, omitaxisticks = FALSE, ncol = 1, id="None")

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
#  simp <- param_getExamplesSimp()
#  cellnumbers <- input_csv_param_capitals_cellnumbers(simp, regionpartfromend = 2, regionpartdevider = "_")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  functions <- input_xml_param_competition(simp, filename =  "Competition_linear")
#  visualise_competition_funcs(simp, functions)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_printCompetitionFunctions(simp, filename = "Competition_linear")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_compileruninfos(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_compilerunparams(simp)

## ---- eval=FALSE, results="hide"-----------------------------------------
#  library(craftyr)
#  simp <- param_getExamplesSimp()
#  data <- input_csv_param_productivities(simp, aft = "C_Cereal", filenameprefix = "AftProduction_",
#  				filenamepostfix = "_multi_medium")
#  		
#  table <- xtable::xtable(data,
#  		label= paste("param.productivities.", aft, sep=""),
#  		caption= paste("Productivities and Capital Sensitivities for ",
#  		tikzDevice::sanitizeTexString(aft), sep=""))
#  
#  print(table, sanitize.colnames.function = identity,
#  		sanitize.rownames.function = identity,
#  		include.rownames = FALSE,
#  		table.placement = "h!")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_printAgentProductionParameters(simp, filenamepostfix = "_mono_medium")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  source("./demo/example_table_singleagentparameters.R")

## ---- eval=FALSE, results="hide"-----------------------------------------
#  hl_printAgentParameters(simp, aftParamId = aftParamId)

