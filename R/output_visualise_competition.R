#' Plot competition functions for services
#' @param simp 
#' @param functions 
#' @param xrange Vector of two. The x range to plot 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_funcs <- function(simp, functions, xrange = c(-3,3)) {
	
	futile.logger::flog.debug("Print competition functions for services...",
			name="craftyr.visualise.competition")
	
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
	
	f <- ggplot(data.frame(x = xrange), aes(x))
	f <- f + stat_functions +
			scale_colour_manual("Services", values = simp$colours$Service)
	print(f)
	simp$fig$close()
}