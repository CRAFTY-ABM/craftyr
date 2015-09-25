#' Postprocessing for CRAFTY simulations
#'
#' \tabular{ll}{
#' Package \tab		crafty \cr
#' Model:  \tab		<Basic> \cr
#' Licence:\tab		GPL v3 \cr
#' Be aware of:\tab	requires libraries ggplot2, RColorBrewer, R.oo \cr
#' Version: \tab	0.1.0 \cr
#' Date	   	\tab 	2014-06-27 \cr
#' Changes	\tab	refactoring, comment \cr
#' }
#' 
#' @description The package offers methods to aggregate, process, and output data from CRAFTY simulations.
#' @name  	crafty
#' @aliases crafty
#' @docType package
#' @title 	Processing CRAFTY model output
#' @author 	Sascha Holzhauer, CESR/UoE \email{Sascha.Holzhauer@@ed.ac.uk},
#' 			Calum Brown, UoE, \email{Calum.Brown@@ed.ac.uk}
#' @references TODO
#' @keywords crafty post-processing ABM land use change LUCC
#' 	example demo/example_visualise_raster_capitals.R
#' 
NULL
.onAttach <- function(...) {
	packages = installed.packages()
	packageStartupMessage(paste("Welcome to craftyr ", 
					packages[packages[,"Package"] == "craftyr", "Version"], "!", sep=""))
}