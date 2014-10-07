#' Init figure output
#' @param simp
#' @param outdir
#' @param filename 
#' @param ensurePath 
#' @return plot
#' 
#' @author Sascha Holzhauer
#' @export
output_visualise_initFigure <- function(simp, outdir, filename, ensurePath = TRUE) {

	if (is.null(filename)) {
		throw("Filename may not be NULL!")
	}
	if (is.null(simp$fig$resfactor)) {
		throw("Set simp$fig$resFactor!")
	}
	if (is.null(simp$fig$width)) {
		throw("Set simp$fig$width!")
	}
	if (is.null(simp$fig$height)) {
		throw("Set simp$fig$height!")
	}
	if (is.null(simp$fig$numfigs) || length(simp$fig$numfigs) == 0) {
		throw("Set simp$fig$numFigs (correctly)!")
	}
	
	if (is.null(simp$fig$outputformat)) {
		outputformat <- "png"
	} else {
		outputformat <- simp$fig$outputformat
	}
	
	print(paste("Output figure file: ", outdir, '/', filename , ".", outputformat, sep=""))
	
	if (ensurePath) {
		sh.ensurePath(outdir)
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
			numLines = numFigs / simp$fig$numcols
		} else {
			numLines = numFigs
		}
	}
	
	if(!is.null(simp$fig$numCols)) {
		numCols = simp$fig$numCols
	} else {
		numCols = 1
	}
	
		
	if (outputformat == "png") {
		png(file=paste(outdir, '/', filename,".png",sep=""),
				height = simp$fig$height * simp$fig$resfactor * numLines,
				width = simp$fig$width * simp$fig$resfactor * numCols,
				res=150 * simp$fig$resfactor,
				pointsize=12)
		
	} else if (outputformat == "jpeg") {
		jpeg(file = paste(outdir, '/', filename, ".jpeg", sep=""),
			height = simp$fig$height * simp$fig$resfactor * numLines,
			width = simp$fig$width * simp$fig$resfactor * numCols,
			quality = 90,
			res = 150 * simp$fig$resfactor)
	}
}