#######################################################################
# crafty demo for raster visualisation
# Last update: 	23.06.2014
# Author: 		Sascha Holzhauer
#######################################################################
source("demo/example_config.R")   # read simp
data <- input_csv_param_capitals()
raster <- convert_2raster(simp, data)
visualisation_raster_printRawPlots(raster, legendTitle = "Capitals Example", 
		factorial= FALSE, omitAxisTicks = FALSE, ncol = 1, id="None")
