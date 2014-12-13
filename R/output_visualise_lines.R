#' Prints a lines of data as ggplot2 with potentially different colour, linetype,
#' and potentially as facet plot.
#' Uses ticks as X variable if present, runid otherwise.
#' 
#' @param simp SIMulation Properties
#' @param data data.frame
#' @param y_column
#' @param title figure title
#' @param colour_column column used to define colours
#' @param colour_legendtitle title for colour legend
#' @param colour_legenditemnames vector of names for colour legend items
#' @param linetype_column column used to define linetypes
#' @param linetype_legendtitle title for linetype legend
#' @param linetype_legenditemnames vector of names for linetype legend items
#' @param facet_column column used to define facets
#' @param facet_ncol number of columns of facet wrap
#' @param filename without extension
#' @param alpha
#' @param ggplotparams vector of ggplot objects to add
#' @return ggplot2 line visualisation
#' @example demo/example_visualise_raster_plots.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_lines <- function(simp, data, y_column, title = NULL,
		colour_column = NULL, colour_legendtitle = colour_column, colour_legenditemnames = NULL,
		linetype_column = NULL, linetype_legendtitle = linetype_column, linetype_legenditemnames = NULL,
		facet_column = NULL, facet_ncol = 2, filename = paste(title, shbasic::shbasic_condenseRunids(data[, "Runid"]), sep="_"),
		alpha=0.7, ggplotparams = NULL) {

	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "lines", sep="/"), filename = filename)
	
	scaleColourElem <- NULL
	if (!is.null(colour_column)) {
		scaleColourElem <- ggplot2::scale_colour_manual(name=colour_legendtitle, 
				values = if (!is.null(simp$colours[[colour_column]])) simp$colours[[colour_column]] else 
							settings_colours_getColors(number = length(unique(data[, colour_column]))),
				 labels = if(!is.null(colour_legenditemnames)) colour_legenditemnames else ggplot2::waiver())
	}
	
	scaleLinetypeElem <- NULL
	if (!is.null(linetype_column)) {
		scaleLinetypeElem <- ggplot2::scale_linetype_manual(name=linetype_legendtitle, 
				values = if (!is.null(simp$linetypes[linetype_column])) simp$linetypes[[linetype_column]] else 
							seq(from=1, by=1, to=unique(length(data[,linetype_column]))),
				labels = if(!is.null(linetype_legenditemnames)) linetype_legenditemnames else ggplot2::waiver())
	}
	
	facetElem = NULL
	if (!is.null(facet_column)) {
		facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol)
	}
	
	if ("Tick" %in% names(data)) {
		x_column = "Tick"
	} else {
		x_column = "Runid"
	}	

	p1 <- ggplot2::ggplot() +
			ggplot2::geom_line(data = data , alpha=alpha, mapping=ggplot2::aes_string(x = x_column, y = y_column,
							colour = colour_column, linetype = linetype_column)) +
			facetElem  +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=8)) +
		 	scaleColourElem +
			scaleLinetypeElem + 
			if (title != "") ggplot2::labs(title = title) else NULL
	print(p1)
	dev.off()
}