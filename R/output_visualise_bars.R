#' Prints a bar of data as ggplot2 with potentially different colour, linetype,
#' and potentially as facet plot.
#' 
#' @param simp SIMulation Properties
#' @param data data.frame or list that is rbinded to a data.frame
#' @param y_column
#' @param title figure title
#' @param fill_column column used to define colours
#' @param fill_legendtitle title for colour legend
#' @param fill_legenditemnames vector of names for colour legend items
#' @param facet_column column used to define facets
#' @param facet_ncol number of columns of facet wrap
#' @param filename without extension
#' @param alpha
#' @param ggplotaddons vector of ggplot objects to add
#' @param x_column
#' @param position passed to geom_bar
#' @return ggplot2 line visualisation
#' @example demo/example_visualise_lines_csv_allocation.R
#'
#' @author Sascha Holzhauer
#' @export
visualise_bars <- function(simp, data, y_column, title = NULL,
		fill_column = NULL, fill_legendtitle = fill_column, fill_legenditemnames = NULL,
		facet_column = NULL, facet_ncol = 4, filename = paste(title, shbasic::shbasic_condenseRunids(data[, "Runid"]), sep="_"),
		alpha=1.0, ggplotaddons = NULL, x_column = "ID", position = "dodge") {

	if (!is.data.frame(data)) {
		data <- do.call(rbind, data)
	}
	if (!is.null(facet_column)) {
		data[,facet_column] <- as.factor(data[,facet_column])
	}

	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "bars", sep="/"), filename = filename)
	
	scaleFillElem <- NULL
	if (!is.null(fill_column)) {
		scaleFillElem <- ggplot2::scale_fill_manual(name=fill_legendtitle, 
				values = if (!is.null(simp$fills[[fill_column]])) simp$fills[[fill_column]] else 
							settings_colours_getColors(number = length(unique(data[, fill_column]))),
				 labels = if(!is.null(fill_legenditemnames)) fill_legenditemnames else ggplot2::waiver())
	}
	
	facetElem = NULL
	if (!is.null(facet_column)) {
		facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol)
	}
	
	p1 <- ggplot2::ggplot() +
			ggplot2::geom_bar(data = data , alpha=alpha, mapping=ggplot2::aes_string(x = x_column, y = y_column,
							fill = fill_column), stat="identity", position = position) +
			facetElem  +
		 	scaleFillElem +
			{if (title != "") ggplot2::labs(title = title) else NULL} +
			(if (x_column == fill_column) ggplot2::scale_x_discrete(breaks=NULL) else NULL) +
			ggplotaddons
	print(p1)
	simp$fig$close()
}