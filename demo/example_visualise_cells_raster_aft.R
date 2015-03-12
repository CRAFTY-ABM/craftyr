#######################################################################
# crafty demo for raster visualisation (capital input data)
# Last update: 	23.06.2014
# Author: 		Sascha Holzhauer
# TODO adapt from rawPlot
#######################################################################
source(simp$simpDefinition)   # read simp
data <- input_csv_param_capitals()
raster <- convert_2raster(simp, data)
visualisation_raster_printRawPlots(raster, legendTitle = "Capitals Example", 
		factorial= FALSE, omitAxisTicks = FALSE, ncol = 1, id="None")
