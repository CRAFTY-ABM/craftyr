#' Create a default output filename of the pattern
#' <world>_<scenario>_<condensed runids>_<region>_<postifx>. Does not include extension!
#' @param simp 
#' @param postfix 
#' @return String of default output filename
#' 
#' @author Sascha Holzhauer
#' @export
output_tools_getDefaultFilename <- function(simp, postfix = "") {
	paste(simp$sim$world, "_",
		  simp$sim$scenario, "_",
		  shbasic::shbasic_condenseRunids(simp$sim$runids), 
		  if (length(simp$sim$regions) > 7) "" else
					  paste("_", paste(simp$sim$regions, collapse = "-"), sep=""),
		  if (postfix != "") "_",
		  gsub(" ", "_", postfix), sep="")
}
#' Format vector of characters into latex list
#' 
#' @param simp 
#' @param x 
#' @param out.format 
#' @param environment 
#' @param marker 
#' @return latex formated list
#' 
#' @author Sascha Holzhauer
#' @export
output_printList <- function(simp, textvector, out.format = knitr::opts_knit$get("out.format"),
		environment = "itemize",
		marker = NULL) {
	if (out.format == "markdown") {
		if (!missing(environment) || !missing(marker)) {
			warning("Ignoring arguments that are not supported for markdown output.")
		}
		out <- sprintf("\n\n%s\n \n", paste("*", textvector, collapse = "\n"))
	} else {
		if (out.format == "latex") {
			itemCommand <- if (missing(marker)) {
						"\\item"
					} else {
						sprintf("\\item[%s]", marker)
					}
			listEnv <- c(
					sprintf("\\begin{%s}\n", environment),
					sprintf("\n\\end{%s}\n", environment))
			out <- paste(itemCommand, textvector, collapse = "\n")
			out <- sprintf("%s%s%s", listEnv[1], out, listEnv[2])
		} else {
			stop("Output format not supported.")
		}
	}
	return(knitr::asis_output(out))
}