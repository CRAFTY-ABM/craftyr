#' Return AFT colours
#' @return coolour pallete
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_getAftColours <- function() {
	aftColours <- c("0" = "orange1",
					"1" = "lightgoldenrod",
					"2" = "indianred4",
					"3" = "indianred1",
					"4" = "green4",
					"5" = "royalblue2",
					"6" = "darkviolet")
}
#' Get colour palette of requested dimension
#' @param number 
#' @param dim  
#' @return colour palette
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_getColors <- function(number, dim=1) {
	if (dim == 1) {
		colors = rev(RColorBrewer::brewer.pal(number, "Set1"))
	}
	if (dim == 2) {
		colors = rev(RColorBrewer::brewer.pal(number, "Accent"))
	}
	colors
}
#' Return requested colour palette
#' @param number number of required colours
#' @param set one of c("Topo","RedGreen","GreenRed") or one for brewer.pal
#' @return colour palette
#' 
#' @author Sascha Holzhauer
#' @export
settings_colours_getColorSet <- function(number, set="Set1") {
	if (set=="AFT") {
		colors = settings_colours_getAftColours()
	} else if (set=="Topo") {
		colors = fBasics::topoPalette(number)
	} else if (set=="RedGreen") {
		colors = grDevices::rainbow(number, s = 1, v = 1, start = 1, end = 2/6, alpha = 1)
	} else if (set=="GreenRed") {
		colors = rev(grDevices::rainbow(number, s = 1, v = 1, start = 1, end = 2/6, alpha = 1))
	}
	if (is.null(set)) {
		colors = rev(RColorBrewer::brewer.pal(number, "Set1"))
	}
	else {
		colors = rev(RColorBrewer::brewer.pal(number, set))
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