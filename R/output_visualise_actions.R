#' Visualise the performance and effect of Actions
#' 
#' @param simp 
#' @param dataname 
#' @param monitordataname 
#' @param title 
#' @param filename 
#' @param y_column 
#' @param colour_column 
#' @param shape_column 
#' @param fill_column 
#' @param facet_column 
#' @param facet_ncol 
#' @param alpha_column 
#' @param size_column 
#' @param returnplot if true the ggplot object is returned
#' @return figure
#' 
#' @author Sascha Holzhauer
#' @export
visualise_actions <- function(simp, 
		dataname = "dataActions", 
		monitordataname = "dataAggregateConnectivity",
		monitorvars = simp$mdata$aftNames[-1],
		monitorColours = NULL,
		title = "Institutional Action",
		filename = NULL,
		x_column = "Tick",
		y_column = "Score",
		colour_column = "FR",
		shape_column = "Agent",
		fill_column = "Action",
		facet_column = "Region", 
		facet_column_levels = NULL,
		facet_ncol = 1,
		alpha_column = "Selected",
		alpha = 0.5,
		size_column = "RestrictionsNumber",
		measure_name = "Connectedness",
		actionColours = NULL,
		returnplot = FALSE) {
	
#	dataname = "dataActions" 
#	monitordataname = "csv_aggregated_supply"
#	monitorvars = simp$mdata$services
#	monitorColours = simp$colours$Service
#	title = "Institutional Action"
#	filename = NULL
#	x_column = "Tick"
#	y_column = "Score"
#	colour_column = "Service"
#	shape_column = "Agent"
#	fill_column = "Action"
#	facet_column = "Region" 
#	facet_ncol = 1
#	alpha_column = "Selected"
#	size_column = NULL
#	measure_name = "TotalProduction"
	
	if (is.null(monitorColours)) {
		monitorColours <- simp$colours$AFT[names(simp$colours$AFT) %in% names(simp$mdata$aftNames)]
		monitorColours <- setNames(monitorColours,
				simp$mdata$aftNames[match(names(monitorColours) ,as.numeric(names(monitorColours)))])
	}
	
	input_tools_load(simp, dataname)
	actionData <- get(dataname)
	
	if(length(actionData) == 0) {
		R.oo::throw.default("Action data is empty!")
	}
	
	input_tools_load(simp, monitordataname)
	monitorData <- get(monitordataname)

	
	# visualise per region (panel) / per institution (line type?): score over time
	# colour: action
	# alpha: reduced for unselected
	
	# simp$fig$init <- function(simp, outdir, filename) {}
	# simp$fig$close<- function() {} 
	titlepart <- if(!is.null(title)) title else "InstitutionalAction"
	if(is.null(filename)) filename = paste(gsub(" ", "-", titlepart), shbasic::shbasic_condenseRunids(actionData[,"Runid"]), sep="_")
	
	actionData <- actionData[, c(x_column, shape_column, fill_column, y_column, alpha_column, facet_column, size_column)]
	actionData$Selected[actionData$Selected == 0] <- 0.7
	actionData$Selected <- as.factor(actionData$Selected)
	
	if (!measure_name %in% colnames(monitorData)) {
		monitorData <- reshape2::melt(data = monitorData, id.vars = c(facet_column, x_column), 
				measure.vars = monitorvars,
				variable.name = colour_column, value.name = measure_name)
	}
	monitorData$Region <- factor(paste(monitorData$Region, measure_name))
	
	data <- merge(actionData, monitorData, all = TRUE)
	
	
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "lines", sep="/"), filename = filename)
	
	# Actions:
	# order actions and synchonise actionColours with it as 'override.aes' in guide_fill does not care:
	data[,fill_column] <- factor(data[,fill_column], levels(data[,fill_column])[order(levels(data[,fill_column]))])

	if (is.null(actionColours)) {
		actionColours <- settings_colours_getColors(number = max(3,length(unique(actionData$Action))))
	} else {
		actionColours <- actionColours[order(names(actionColours))]
	}
	
	if(length(actionColours) != length(levels(data$Action[!is.na(data$Action)]))) {
		R.oo::throw.default(sprintf("Number of colours in actionColours (%d) does not match required number (%d)! Defined actions: %s",
						length(actionColours),
						length(levels(data$Action[!is.na(data$Action)])),
						paste(levels(data$Action[!is.na(data$Action)]), collapse="/")))
	}
	actionColours <- setNames(actionColours, levels(data$Action[!is.na(data$Action)]))
	actionColours <- actionColours[1:length(levels(data$Action[!is.na(data$Action)]))]
	
	scaleFillElem <- ggplot2::scale_fill_manual(name= fill_column, 
				values = c(actionColours))
	
	# Monitored data:
	scaleColourElem <- ggplot2::scale_colour_manual(name=colour_column, 
			values = c(monitorColours),
			labels =  ggplot2::waiver())
		
	
	facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol, scales="free_y")
	
	# order facets:
	if(is.null(facet_column_levels)) {
		data[,facet_column] <-  factor(data[,facet_column], levels(data[,facet_column])[order(levels(data[,facet_column]))])
	} else {
		data[,facet_column] <-  factor(data[,facet_column], facet_column_levels)
	}
	
	if (!is.null(size_column)) {
		data[,size_column]	<- as.numeric(levels(data[,size_column])[data[,size_column]])
		data[is.na(data[,size_column]),size_column]	<- mean(data[,size_column], na.rm = TRUE) 
	}
	
	p1 <- ggplot2::ggplot() +
			ggplot2::geom_point(data = data, mapping=ggplot2::aes_string(alpha=alpha_column, x = x_column, y = y_column,
							fill = fill_column, shape = shape_column, size=size_column)) +
			facetElem  +
			
			ggplot2::scale_shape_manual(values=c(21:25)) +
			scaleFillElem +
			guides(fill=ggplot2::guide_legend(override.aes=list(colour=actionColours))) +
			ggplot2::scale_alpha_discrete(range=c(alpha, 1), labels=c("Not performed", "Performed")) +
			
			ggplot2::geom_line(data = data, mapping=ggplot2::aes_string(x = x_column, y = measure_name, 
							colour = colour_column)) +
			scaleColourElem +
			
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=8)) +
			(if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL) 
	print(p1)
	simp$fig$close()
	if (returnplot) return(p1)
}