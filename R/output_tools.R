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
		  shbasic::shbasic_condenseRunids(simp$sim$runids), "_",
		  simp$sim$regions, 
		  if (postfix != "") "-",
		  postfix, sep="")
}