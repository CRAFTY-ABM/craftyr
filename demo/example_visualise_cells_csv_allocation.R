#######################################################################
# crafty demo for csv data visualisation of AFT allocation
# Last update: 	27.10.2014
# Author: 		Sascha Holzhauer
# TODO adapt from rawPlot
#######################################################################
source(simp$simpDefinition)   # read simp
data <- input_csv_param_capitals(simp)
raster <- convert_2raster(simp, data)
visualise_raster_printRawPlots(simp, raster, legendtitle = "Capitals Example", 
		factorial= FALSE, omitaxisticks = FALSE, ncol = 1)