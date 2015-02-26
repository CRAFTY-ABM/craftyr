#######################################################################
# crafty demo for raster visualisation (capital input data)
# Last update: 	20.02.2015
# Author: 		Sascha Holzhauer
#######################################################################
source(simp$simpDefinition)   # read simp
data <- input_csv_param_capitals()
visualisation_cells_printPlots(data, legendTitle = "Capitals Example", 
		factorial= FALSE, omitAxisTicks = FALSE, ncol = 1, id="None")
