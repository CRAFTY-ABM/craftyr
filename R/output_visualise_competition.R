#' Plot competition functions for services
#' @param simp 
#' @param functions 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_funcs <- function(simp, functions) {
	
	futile.logger::flog.debug("Print competition functions for services...",
			name="crafty.visualise.competition")
	
	simp$fig$numcols <- 1
	simp$fig$numfigs <- 1
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "param", sep="/"), 
			filename = "competitionFunctions")
	
	stat_functions <- unlist(mapply(function(fun, name) {
						eval(substitute(
										expr = {
											ggplot2::stat_function(fun = fun, aes(colour = service))
										}, env = list(service=name)))
					}, functions, names(functions)))
	
	f <- ggplot(data.frame(x = c(-10, 10)), aes(x))
	f <- f + stat_functions +
			scale_colour_manual("Services", values = simp$colours$Service)
	print(f)
	simp$fig$close()
}