#' Reads output data for the specified datatype amd dataname from CSV data for potentially multiple runs, 
#' regions, and multiple ticks.
#' @param simp SIMulation Properties
#' @param datatype  (e.g. "Capital")
#' @param dataname (e.g. "Cap1")
#' @param colums vector of colum names. If given restricted returned colums to the given headers (plus X and Y coordinates)
#' @param pertick if TRUE the filename will be complemented by all available ticks 
#' @param attachfileinfo
#' @return list of data.frames containing requested data  
#' 
#' @author Sascha Holzhauer
#' @export
input_csv_data <- function(simp, datatype = NULL, dataname = "Cell", colums = NULL, pertick = FALSE, attachfileinfo = TRUE) {	
	fileinfos = input_tools_getModelOutputFilenames(simp, datatype = datatype, dataname = dataname, pertick = pertick)
	data <- lapply(fileinfos$filename, utils::read.csv)
	if (!is.null(colums)) {
		data <- lapply(data, function(x) x[, c(simp$csv$cname_x, simp$csv$cname_y, colums)])
	}
	if (attachfileinfo) {
		data  <- mapply(function(d, info) cbind(d, info[,-which(names(info)  %in% c("filename", names(d)))], row.names=NULL),
			data,
			split(fileinfos, rownames(fileinfos)), SIMPLIFY = FALSE)
	}
	do.call(rbind.data.frame, data) 
}
