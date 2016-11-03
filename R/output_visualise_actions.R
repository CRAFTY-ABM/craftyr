#' Visualise the performance and effect of Actions
#' 
#' @param simp 
#' @param actiondata  data.frame with columns \code{Action}, \code{Score}, \code{Selected}, \code{facet_column} and \code{x_column}.
#' 					If a character string, the data will be loaded.
#' @param onlyselected If \code{TRUE} only selected actions will be shown.
#' @param monitordata (melted) data.frame with columns \code{facet_column} and \code{x_column}.
#' 					If a character string, the data will be loaded. NOTE: the value for \code{facet_column} should be distinct from
#' 					the one in \code{actiondata} to show monitored data in a separate facet.
#' @param title figure title
#' @param filename	filename for figure file
#' @param x_column	X variable
#' @param y_column_action Y variable for action facets
#' @param y_column_measure Y variable for measure facets
#' @param colour_column Colour variable
#' @param shape_column Shape variable
#' @param fill_column d Fill variable for action facets
#' @param alpha_column Alpha variable
#' @param alpha lower alpha value (upper is 1)
#' @param size_column Size variable
#' @param lineseparator_column_actions used to draw separate line besides 'Agent'. A character string of comma-separated variables
#' 						is possible (feeds into \code{interaction)}.
#' @param linetype_column_measure Variable for linetype of measure facets
#' @param facet_column Facet Variable
#' @param facet_ncol number of facet columns
#' @param facet_column_levels enables the user to specify the order of facets - vector of character strings.
#' @param agentfillcolour named vector with keys for actiondata values in column \code{fill_column}
#' @param monitorcolours named vector with keys for actiondata values in column \code{colour_column}
#' @param actionlinecolour Colour for lines connecting action symbols (character)
#' @param actionshapenumbers numbers of shapes for agents (action symbols)
#' @param returnplot if true the ggplot object is returned
#' @param ggplotaddons pass additional ggplot elements, e.g. \code{ggplot2::facet_grid(as.formula("Region ~ Runid"), scales="free_y")}.
#' 					Can be a single element or a list of elements.
#' @return figure
#' 
#' @author Sascha Holzhauer
#' @export
visualise_actions <- function(simp, 
		actiondata 			= "dataActions",
		onlyselected 		= FALSE,
		monitordata 		= NULL,
		title 				= NULL,
		filename 			= NULL,
		x_column 			= "Tick",
		y_column_action 	= "Score",
		y_column_measure	= "Value",
		colour_column 		= "Service",
		shape_column 		= "Agent",
		fill_column 		= "Action",
		alpha_column 		= "Selected",
		alpha 				= 0.5,
		size_column 		= NULL,
		lineseparator_column_actions	= NULL,
		linetype_column_measures 		= NULL,
		linetype_column_actions = NULL,
		facet_column 		= "Region", 
		facet_ncol = 1,
		facet_column_levels = NULL,
		actionfillcolours 	= NULL,
		monitorcolours		= NULL,
		actionlinecolour 	= "darkgrey",
		actionshapenumbers	= c(21:25),
		returnplot = FALSE,
		ggplotaddons = NULL) {
	
#	dataname = "dataActions" 
#	monitordataname = "csv_aggregated_supply"
#	monitorvars = simp$mdata$services
#	monitorColours = simp$colours$Service
#	title = "Institutional Action"
#	filename = NULL
#	x_column = "Tick"
#	y_column_action = "Score"
#	colour_column = "Service"
#	shape_column = "Agent"
#	fill_column = "Action"
#	facet_column = "Region" 
#	facet_ncol = 1
#	alpha_column = "Selected"
#	size_column = NULL
#	measure_name = "TotalProduction"
	
	
#	# comapre settings:
#	dataname = "dataActions"
#	onlyselected = TRUE
#	monitordataname = "csv_aggregated_supply"
#	monitorvars = simp$mdata$services
#	monitorColours = simp$colours$Service
#	title = "Institutional Action"
#	filename = NULL
#	x_column = "Tick"
#	y_column_action = "Score"
#	colour_column = "Service"
#	shape_column = "Agent"
#	fill_column = "Action"
#	facet_column = "Region" 
#	facet_ncol = 1
#	facet_column_levels = NULL
#	alpha_column = "Selected"
#	alpha = 0.7
#	size_column = NULL
#	linetype_column = "ID"
#	lineseparator_column_actions = "Runid"
#	measure_name = "TotalProduction"
#	actionfillcolours = NULL
#	returnplot = FALSE
	
	if (is.null(monitorcolours)) {
		monitorcolours <- simp$colours$AFT[names(simp$colours$AFT) %in% names(simp$mdata$aftNames)]
		monitorcolours <- setNames(monitorcolours,
				simp$mdata$aftNames[match(names(monitorcolours) ,as.numeric(names(monitorcolours)))])
	}
	
	if (!is.null(actiondata) && is.character(actiondata)) {
		input_tools_load(simp, actiondata)
		actiondata <- get(actiondata)
	}
	
	if(length(actiondata) == 0) {
		R.oo::throw.default("Action data is empty!")
	}
	
	if (!is.null(monitordata) && is.character(monitordata)) {
		input_tools_load(simp, monitordata)
		monitordata <- get(monitordata)
	}
	
	# visualise per region (panel) / per institution (line type?): score over time
	# colour: action
	# alpha: reduced for unselected
	
	# simp$fig$init <- function(simp, outdir, filename) {}
	# simp$fig$close<- function() {} 
	titlepart <- if(!is.null(title)) title else "InstitutionalAction"
	if(is.null(filename)) filename = paste(gsub(" ", "-", titlepart), 
				shbasic::shbasic_condenseRunids(actiondata[,"Runid"]), sep="_")
	
	if (onlyselected) {
		actiondata <- actiondata[actiondata$Selected == 1,]
	}
	
	
	data <- merge(actiondata, monitordata, all = TRUE)
	
	if (!is.null(alpha_column)) {
		data[,alpha_column] <- factor(data[,alpha_column])
	}
	
	simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "lines", sep="/"), filename = filename)
	
	# Actions:
	# order actions and synchonise actionfillcolours with it as 'override.aes' in guide_fill does not care:
	data[,fill_column] <- factor(data[,fill_column], levels(data[,fill_column])[order(levels(data[,fill_column]))])
	data$Action <- factor(data$Action)
	# handle colours:
	if (is.null(actionfillcolours)) {
		actionfillcolours <- settings_colours_getColors(simp, number = max(3,length(unique(actiondata$Action))))
	} else {
		actionfillcolours <- actionfillcolours[order(names(actionfillcolours))]
		if(any(!levels(data$Action[!is.na(data$Action)]) %in% names(actionfillcolours))) {
			R.oo::throw.default(sprintf("actionfillcolours (%s) does not contain all actions in data (%s)!",
							paste(names(actionfillcolours), collapse="/"),
							paste(levels(data$Action[!is.na(data$Action)]), collapse="/")))
		}
		actionfillcolours <- actionfillcolours[names(actionfillcolours) %in% levels(data$Action[!is.na(data$Action)])]
	}
	
	
	actionfillcolours <- setNames(actionfillcolours, levels(data$Action[!is.na(data$Action)]))
	actionfillcolours <- actionfillcolours[1:length(levels(data$Action[!is.na(data$Action)]))]
	scaleFillElem <- ggplot2::scale_fill_manual(name= fill_column, 
				values = c(actionfillcolours))
		
	# Monitored data:
	scaleColourElem <- ggplot2::scale_colour_manual(name=colour_column, 
			values = c(monitorcolours),
			labels =  ggplot2::waiver())
		
	
	# facets / order facets:
	facetElem <- ggplot2::facet_wrap(as.formula(paste("~", facet_column)), ncol = facet_ncol, scales="free_y")
	if(is.null(facet_column_levels)) {
		data[,facet_column] <-  factor(data[,facet_column], levels(data[,facet_column])[order(levels(data[,facet_column]))])
	} else {
		data[,facet_column] <-  factor(data[,facet_column], facet_column_levels)
	}
	
	# handle action symbol size:
	if (!is.null(size_column)) {
		data[,size_column]	<- as.numeric(levels(data[,size_column])[data[,size_column]])
		data[is.na(data[,size_column]),size_column]	<- mean(data[,size_column], na.rm = TRUE) 
	}
	
	scaleAlphaElem <- if (!onlyselected) {
		ggplot2::scale_alpha_discrete(range=c(alpha, 1), labels=c("Not performed", "Performed"))
	} else {
		ggplot2::scale_alpha_discrete(range=c(1, 1), guide=FALSE)
	}
	
	lineElemActions <- ggplot2::geom_line(data = data[!is.na(data$Selected) & data$Selected == 1,], 
									mapping=ggplot2::aes_string(x = x_column, y = y_column_action, 
						 group = if (is.null(lineseparator_column_actions)) "Agent" else 
									 paste("interaction(Agent, ", lineseparator_column_actions, ")", sep=""),
						 linetype = linetype_column_actions
					), color = actionlinecolour)
	
	if (!is.null(linetype_column_measures)) {
		lineElemMeasures <- ggplot2::geom_line(data = data, mapping=ggplot2::aes_string(x = x_column, y = y_column_measure,
						linetype = linetype_column_measures, 
						colour = colour_column)
					)
	} else {
		lineElemMeasures <- ggplot2::geom_line(data = data, mapping=ggplot2::aes_string(x = x_column, y = y_column_measure, 
						colour = colour_column))
	}
	
	p1 <- ggplot2::ggplot() +
			lineElemActions +			
			ggplot2::geom_point(data = data, mapping=ggplot2::aes_string(alpha=alpha_column, x = x_column, y = y_column_action,
				fill = fill_column, shape = shape_column, size=size_column)) +
			lineElemMeasures +
			
			facetElem  +
			ggplot2::scale_shape_manual(values=actionshapenumbers) +
			scaleFillElem +
			
			ggplot2::guides(fill=ggplot2::guide_legend(override.aes=list(colour=actionfillcolours))) +
			ggplot2::guides(linetype=ggplot2::guide_legend(title=linetype_column_measures)) +
			scaleAlphaElem +
			scaleColourElem +
			ggplot2::scale_x_continuous(breaks= scales::pretty_breaks()) +
			ggplot2::theme(strip.text.x = ggplot2::element_text(size=8)) +
			(if (!is.null(title) && title != "") ggplot2::labs(title = title) else NULL) +
			ggplotaddons
	print(p1)
	simp$fig$close()
	if (returnplot) return(p1)
}


#ggplot(mpg, aes(displ, hwy)) + 
#		geom_point(aes(colour = class)) + 
#		geom_point(aes(colour = trans)) 