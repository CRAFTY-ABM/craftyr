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
	
	xmlParsed <- XML::xmlParse(file=filepath)
	xml_data  <- XML::xmlToList(xmlParsed)
	
	xml_data$.attrs <- NULL
	xml_data <- sapply(xml_data, unlist,  recursive = F, simplify=F)
	
	functions <- sapply(xml_data, get_function)
	names(functions) <- do.call(rbind, xml_data)[,".attrs.service"]
	return(functions)
}
get_function <- function(data) {
	if (data["curve.class"] %in% "com.moseph.modelutils.curve.LinearFunction") {
		return(function(x) {x*as.numeric(data["curve.b"]) + as.numeric(data["curve.a"])})
	} else if (data["curve.class"] %in% "com.moseph.modelutils.curve.ExponentialFunction") {
		return(function(x) {as.numeric(data["curve.A"]) + 
							(as.numeric(data["curve.B"])*exp(as.numeric(data["curve.C"])*x))})
	} else if (data["curve.class"] %in% "com.moseph.modelutils.curve.SigmoidFunction") {
		# read params from run.csv by runid
		
		runid = as.numeric(if(grepl('-', simp$sim$runids[1])) strsplit(simp$sim$runids[1], '-')[[1]][1] else {
							simp$sim$runids[1]})
		
		rundata <- read.csv(file=paste(simp$dirs$data, simp$sim$version, "Runs.csv", sep="/"))
		rundata <- rundata[rundata$run == runid,]
		
		# defaults need to comply with those in com.moseph.modelutils.curve.SigmoidFunction
		a = get_xmlfunction_parameter(data, rundata, "A", 1.0)
		h = get_xmlfunction_parameter(data, rundata, "H", 1.0)
		p = get_xmlfunction_parameter(data, rundata, "P", 3.0)
		c = get_xmlfunction_parameter(data, rundata, "C", 0.0)
		d = get_xmlfunction_parameter(data, rundata, "D", 0.0)
		norm = if (get_xmlfunction_parameter(data, rundata, "normalise", "true") == "false") FALSE else TRUE

		return(test = function(x, A=a, H=h, P=p, C=c, D=d, normalise = norm) {
					fun = A*(x-C)^P/(H^P + abs((x-C)^P)) + D
					if (normalise) {
						fun = sapply(fun, function(f, D) {
									if ((f) < D) {
										(D+(f-D) * (1.0+D))
									} else  {
										(D+(f-D)*(1.0-D))
									}}, D=D)
					}
					fun
				})


	} else {
		futile.logger::flog.warn("Curve in %s not supported!",
					filename,
					name = "craftyr.input.xml.param.competition")
	}
}

#' Extact parameter from curce XML file
#' 
#' Considers given default value in case the parameter is not defined in the XML file.
#' @param data 
#' @param rundata 
#' @param parameter 
#' @param default 
#' @return parameter value
#' 
#' @author Sascha Holzhauer
#' @export
get_xmlfunction_parameter <- function(data, rundata, parameter, default) {
	 value = default;
	 if(!is.na(data[paste("curve.", parameter, sep="")])) {
	 	if (grepl("@", data[paste("curve.", parameter, sep="")])) {
			value =  as.numeric(rundata[, paste("Comp_", data[".attrs.service"], "_", parameter, sep="")])
		} else {
			value = as.numeric(data[paste("curve.", parameter, sep="")])
		}
	}
	return(value)
 }