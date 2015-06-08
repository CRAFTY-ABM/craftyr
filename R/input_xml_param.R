#' Reads competition functions for each service
#' @param simp SIMulation Properties
#' @param aft
#' @return data.frame containing productivities  
#' 
#' @author Sascha Holzhauer
#' @export
input_xml_param_competition <- function(simp, filename = "Competition_linear") {
	filepath <- paste(simp$dirs$param$getparamdir(simp, datatype="competition"), 
			"/", filename, ".xml", sep="")
	
	xmlParsed <- xmlParse(file=filepath)
	xml_data <- xmlToList(xmlParsed)
	
	xml_data$.attrs <- NULL
	xml_data <- sapply(xml_data, unlist,  recursive = F, simplify=F)
	
	functions <- sapply(xml_data, get_function)
	names(functions) <- do.call(rbind, xml_data)[,".attrs.service"]
	return(functions)
}

get_function <- function(data) {
	if (data["curve.class"] %in% "com.moseph.modelutils.curve.LinearFunction") {
		return(function(x) {x*as.numeric(data["curve.b"]) + as.numeric(data["curve.a"])})
	}
}