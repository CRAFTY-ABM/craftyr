#' Add colour attribute to nodes in graphml file
#' 
#' @param simp 
#' @param graphfilename 
#' @param frattributename 
#' @param colourattributename 
#' @return new graph file
#' 
#' @author Sascha Holzhauer
#' @export
addColourAttributeFr <- function(simp, graphfilename, frattributename="FR", colourattributename="ncolor") {
	# read graphml file
	g <- igraph::read.graph(file(description=graphfilename), format="graphml")
	
	fr = igraph::get.vertex.attribute(g, frattributename)
	g <- igraph::set.vertex.attribute(g, colourattributename, value = gplots::col2hex(simp$colours$AFT[match(as.numeric(fr), names(simp$colours$AFT))]))
	
	outfilename = paste(dirname(graphfilename), "/Color_", basename(graphfilename), sep="")
	igraph::write.graph(g, file=outfilename, format="graphml")
}