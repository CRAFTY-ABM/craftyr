#######################################################################
# crafty demo for raster visualisation (capital input data)
# Last update: 	20.02.2015
# Author: 		Sascha Holzhauer
#######################################################################
simp <- param_getExamplesSimp()
data <- input_csv_data()
visualisation_cells_printPlots(data, 
		factorial= TRUE, ncol = 1, id="None")
