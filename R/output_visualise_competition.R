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
#' Histogram of competitiveness per AFT: stacked: below/above GU threshold
#' 
#' @param simp 
#' @param data
#' @param facet_ncol 
#' @param filename 
#' @param numbins 
#' @param title 
#' @param ggplotaddons 
#' @param setfigdims if \code{TRUE} \code{simp$fig$height} and \code{simp$fig$width} are set appropriately
#' @return facet histogram plot 
#' 
#' @author Sascha Holzhauer
#' @export
visualise_competition_prealloc <- function(simp, data, facet_ncol = length(simp$mdata$aftNames) - 1,
		filename = paste("PreAllocationCompetition", simp$sim$id, sep="_"),
		numbins = 20, binwidth = max(data$PreAllocCompetitiveness)/numbins, title = NULL, ggplotaddons = NULL, setfigdims = TRUE) {
	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	
	if(setfigdims) {
		simp$fig$height			<- 200 * length(unique(data$Tick))
		simp$fig$width			<- 400 * facet_ncol
	}
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	scaleFillElem <- ggplot2::scale_fill_manual(name="GivingUp", 
			values = c("1"="red", "0" = "green"),
			labels = c("1"="Giving up", "0"="Persisting"))
	
	facetElem = ggplot2::facet_wrap(as.formula(paste("Tick ~", "AFT")), ncol = facet_ncol, scales="free_y")
	
	p1 <- ggplot2::ggplot(data, aes(x=PreAllocCompetitiveness, fill=GU)) + 
			geom_bar(binwidth = binwidth) + 
			facetElem  +
			scaleFillElem +
			{if (!is.null(title)) ggplot2::labs(title = title) else NULL} +
			ggplotaddons
	print(p1)
	simp$fig$close()
}