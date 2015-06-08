#' Init figure output
#' @param simp SIMulation Properties
#' @param outdir (default: simp$dirs$output$figures) 
#' @param filename 
#' @param ensurePath switch
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
output_visualise_initFigure <- function(simp, outdir = simp$dirs$output$figures, filename, ensurePath = TRUE) {
	
	if (is.null(filename)) {
		R.oo::throw.default("Filename may not be NULL!")
	}
	if (is.null(simp$fig$resfactor)) {
		R.oo::throw.default("Set simp$fig$resFactor!")
	}
	if (is.null(simp$fig$width)) {
		R.oo::throw.default("Set simp$fig$width!")
	}
	if (is.null(simp$fig$height)) {
		R.oo::throw.default("Set simp$fig$height!")
	}
	if (is.null(simp$fig$numfigs) || length(simp$fig$numfigs) == 0) {
		R.oo::throw.default("Set simp$fig$numFigs (correctly)!")
	}
	
	if (is.null(simp$fig$outputformat)) {
		outputformat <- "png"
	} else {
		outputformat <- simp$fig$outputformat
	}
	
	futile.logger::flog.info("Output figure file: %s/%s.%s", 
			outdir,
			filename,
			outputformat,
			name = "craftyr.visualise.init")
	
	if (ensurePath) {
		shbasic::sh.ensurePath(outdir)
	}
	
	if (!is.null(simp$fig$splitfigs) && simp$fig$splitfigs > 0) {
		numFigs = 1
	} else {
		numFigs = simp$fig$numfigs
	}
	
	if(!is.null(simp$fig$numlines)) {
		numLines = simp$fig$numlines
	} else {
		if (!is.null(simp$fig$numcols)) {
			numLines = ceiling(numFigs / simp$fig$numcols)
		} else {
			numLines = numFigs
		}
	}
	
	if(!is.null(simp$fig$numcols)) {
		numCols = simp$fig$numcols
	} else {
		numCols = 1
	}
	
	futile.logger::flog.debug("Height: %f (num lines: %.1f)\nWidth:%f (number of column: %.1f)",
			simp$fig$height * simp$fig$resfactor * numLines,
			numLines,
			simp$fig$width * simp$fig$resfactor * numCols,
			numCols,
			name = "craftyr.visualise.init")

	if (outputformat == "png") {
		grDevices::png(file=paste(outdir, '/', filename,".png",sep=""),
				height = simp$fig$height * simp$fig$resfactor * numLines,
				width = simp$fig$width * simp$fig$resfactor * numCols,
				res=150 * simp$fig$resfactor,
				pointsize=12)
		
	} else if (outputformat == "jpeg") {
		grDevices::jpeg(file = paste(outdir, '/', filename, ".jpeg", sep=""),
			height = simp$fig$height * simp$fig$resfactor * numLines,
			width = simp$fig$width * simp$fig$resfactor * numCols,
			quality = 90,
			res = 150 * simp$fig$resfactor)
	}
}