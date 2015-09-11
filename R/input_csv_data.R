#' Reads output data from CSV files
#' Reads output data for the specified datatype and dataname from CSV data for potentially multiple runs, 
#' regions, and multiple ticks.
#' @param simp SIMulation Properties
#' @param datatype  datatype (e.g. "Capital")
#' @param dataname dataname (e.g. "Cap1")
#' @param columns Vector of colum names. If given restricts returned colums to the given headers 
#' 			(plus X and Y coordinates, and - if (attachfileinfo == TRUE) - Tick, RunId, Scenario, and Region)
#' @param pertick If TRUE the filename will be complemented by all available ticks 
#' @param extension file extension of those files to read
#' @param starttick first tick to consider 
#' @param endtick  last tick to consider
#' @param tickinterval Note: tickinterval is based on starttick which defaults to 0!
#' @param attachfileinfo If TRUE, further information about the file is attached to the returned list
#' @param splitfileinfo If TRUE, use split upon fileinfo data (not tested)
#' @param bindrows, If TRUE, rbind all data into one data.frame
#' @param aggregationFunction function applied to aggregate data
#' @param do not return X and Y columns
#' @return List (unless bindrows == TRUE) of data.frames (one list item per data folder) containing requested data  
#' 
#' @author Sascha Holzhauer debug
#' @export
input_csv_data <- function(simp, datatype = NULL, dataname = "Cell", columns = NULL, pertick = FALSE, 
		extension = "csv",
		starttick = if(!is.null(simp$sim$starttick)) simp$sim$starttick else simp$tech$mintick,
		endtick = if(!is.null(simp$sim$endtick)) simp$sim$endtick else simp$tech$maxtick, 
		tickinterval = simp$csv$tickinterval_agg, 
		attachfileinfo = TRUE,
		splitfileinfo = FALSE,
		bindrows = FALSE,
		aggregationFunction = NULL,
		skipXY = FALSE) {	
	fileinfos = input_tools_getModelOutputFilenames(simp, datatype = datatype, dataname = dataname, extension = extension, pertick = pertick,
			starttick = starttick, endtick = endtick, tickinterval = tickinterval)
	
	futile.logger::flog.debug("File infos: \n%s", 
			paste(do.call(rbind, fileinfos)$Filename, collapse="\n"),
			name="craftyr.input.csv")
	
	data <- lapply(fileinfos, function(item) {
				result <- plyr::ddply(item, "Filename", function(df) {
							return <- tryCatch({
								data <- utils::read.csv(df[,"Filename"])
								if (length(data[,1]) == 0) {
									warnings("CSV file ", df[,"Filename"] , " does not contain any rows!")
									return(NULL)
								} else {
									if (attachfileinfo) {
										infos <- df[,-which(colnames(df)  %in% c("Filename", names(data)))]
										data <- cbind(data, infos, row.names=NULL)
									}
									if (skipXY) {
										data[,simp$csv$cname_x] <- NULL
										data[,simp$csv$cname_y] <- NULL
									}
									if (!is.null(aggregationFunction)) {
										data <- aggregationFunction(simp, data)
									}
								}
								invisible(data)
							}, error = function(err) {
								warning(err)
								return(NULL)
							})
							if(length(return) == 0)  R.oo::throw.default("Something's wrong (returned data frame empty)...")
							invisible(return)
				})
				# don't know why filename is within ddply return...
				negindices <- which(colnames(result)  %in% c("Filename"))
				result <- if(length(negindices)>0) result[, -negindices] else result
	})
	
	if (!is.null(columns)) {
		# assumes that fileinfos structure is the same for all list elements!
		data <- lapply(data, function(x){
						tryCatch({
								x[, c(if(!skipXY) c(simp$csv$cname_x, simp$csv$cname_y), columns, 
									if (attachfileinfo) 
										colnames(fileinfos[[1]])[colnames(fileinfos[[1]]) %in% colnames(x)])]
							
						}, error = function(e) {
								futile.logger::flog.error("Undefined columns requested from CSV data: %s (%s)", 
									paste(unique(c(if(!skipXY) c(simp$csv$cname_x, simp$csv$cname_y),
										columns, if (attachfileinfo) 
										colnames(fileinfos[[1]])[colnames(fileinfos[[1]]) %in% colnames(x)])), 
										collapse="|"),
									e,
									name="craftyr.input.csv")
						})})
	}
			
	if (attachfileinfo & splitfileinfo) data <- split(data, data[,names(fileinfos)])
	
	if (bindrows) 
		result <- do.call(rbind.data.frame, data)
	else
		result <- data
	invisible(result)
}
