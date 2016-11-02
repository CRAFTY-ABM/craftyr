
#' Get colour palette of requested size.
#' 
#' Applied if colours are not defined in \code{simp$fills} or \code{simp$colours} 
#' @param number 
#' @param dim  
#' @return colour palette
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_getColors <- function(number, dim=1, set="Set1") {
	colors = rev(RColorBrewer::brewer.pal(number, set))
}
#' Return requested colour palette
#' @param SIMP
#' @param number number of required colours
#' @param set one of c("Topo","RedGreen","GreenRed") or one for brewer.pal
#' @return colour palette
#' 
#' @deprecated
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_getColorSet <- function(simp, number=9, set="Set1") {
	if (set %in% names(simp$colours)) {
		colors = simp$colours[[set]]
	} else if (set=="AFT") {
		colors = settings_colours_getAftColours()
	} else if (set=="Mixed100") {
		colors = settings_colours_getAftColours() # TODO
	} else if (set=="Topo") {
		colors = fBasics::topoPalette(number)
	} else if (set=="RedGreen") {
		colors = grDevices::rainbow8(number, s = 1, v = 1, start = 1, end = 2/6, alpha = 1)
	} else if (set=="GreenRed") {
		colors = rev(grDevices::rainbow(number, s = 1, v = 1, start = 1, end = 2/6, alpha = 1))
	} else  {
		if (number < 9 & number > 3) {
			colors = rev(RColorBrewer::brewer.pal(number, "Set1"))
		} else {
			colors = topo.colors(number)
		}
	} 
	colors
}
#' Show colour palette
#' @param bg 
#' @param cex 
#' @param srt 
#' @return colour palettes plot
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_showColours<- function(bg = "gray", cex = 0.75, srt = 30) {
	m <- ceiling(sqrt(n <- length(cl <- colors())))
	length(cl) <- m*m; cm <- matrix(cl, m)
	op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
	plot(1:m,1:m, type="n", axes=FALSE)
	text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
}