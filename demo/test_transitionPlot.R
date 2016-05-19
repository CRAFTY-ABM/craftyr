# Test for transistion plot with Land Use data
# 
# Author: Sascha Holzhauer
###############################################################################


library(Gmisc)
library(magrittr)

transitions <- structure(list("2020" = structure(c(0, 6657, 899, 9212, 175,  
								4159, 349, 0, 0, 0, 0, 0, 0, 0, 5037, 2238, 0, 2214, 64, 1098,  
								91, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1988, 870, 120,  
								1261, 38, 0, 53, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list( 
								c("Unmanaged", "C_Cereal", "NC_Cereal", "C_Livestock", "NC_Livestock",  
										"Forester", "VLI_Livestock"), c("Unmanaged", "C_Cereal",  
										"NC_Cereal", "C_Livestock", "NC_Livestock", "Forester", "VLI_Livestock" 
								))), "2030" = structure(c(0, 57, 396, 58, 0, 4006, 0, 0,  
								0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 78, 0, 0, 0, 0, 0, 0, 0, 0,  
								0, 0, 0, 0, 0, 0, 0, 4015, 10, 123, 11, 0, 0, 0, 0, 0, 0, 0,  
								0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(c("Unmanaged", "C_Cereal",  
										"NC_Cereal", "C_Livestock", "NC_Livestock", "Forester", "VLI_Livestock" 
								), c("Unmanaged", "C_Cereal", "NC_Cereal", "C_Livestock", "NC_Livestock",  
										"Forester", "VLI_Livestock"))), "2040" = structure(c(0, 17, 117,  
								0, 0, 4123, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  
								0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4384, 0, 19, 0, 0, 0, 0,  
								0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(c("Unmanaged",  
										"C_Cereal", "NC_Cereal", "C_Livestock", "NC_Livestock", "Forester",  
										"VLI_Livestock"), c("Unmanaged", "C_Cereal", "NC_Cereal", "C_Livestock",  
										"NC_Livestock", "Forester", "VLI_Livestock")))), .Names = c("2020",  
				"2030", "2040"))

ticks <- c(2010, 2020, 2030, 2040)

aftcolours <- structure(c("black", "orange1", "lightgoldenrod", "indianred4",  "indianred1", "green4", 
				"royalblue2"), .Names = c("-1", "0",  "1", "2", "3", "4", "5"))

population <- structure(list(population = c(0.595591322603219, 0.0594209237228831,  0.169099895031491, 
						0.0583100069979006, 0.00352956613016095, 0.0973276766969909,  0.0167206088173548), 
				pop2 = c(0.658686144156753, 0.0167118614415675,  0.211625262421274, 0.00282102869139258, 
						0.00231805458362491,  0.0932732680195941, 0.0145643806857943), 
				pop2 = c(0.660881735479356,  0.0164188243526942, 0.209696466060182, 0.00251924422673198, 
						0.00231805458362491,  0.0936012946116165, 0.0145643806857943), 
				pop2 = c(0.660326277116865,  0.0163444716585024, 0.209101644506648, 0.00251924422673198, 
						0.00231805458362491,  0.0948259272218334, 0.0145643806857943)), 
		.Names = c("population",  "pop2", "pop2", "pop2"), 
		row.names = c("Unmanaged", "C_Cereal",  "NC_Cereal", "C_Livestock", "NC_Livestock", "Forester", 
				"VLI_Livestock" ), class = "data.frame")



###
options(error=recover)
transitionsPlot <- transitions[[1]] %>%
		getRefClass("Transition")$new(label=as.character(c(ticks[1], ticks[2])))


transitionsPlot$title 			<- "AFT transitionsPlot"
transitionsPlot$box_label_pos 	<- "bottom"
transitionsPlot$arrow_type 		<- "gradient2sided"
transitionsPlot$max_lwd 		<- grid::unit(.05, "npc")
transitionsPlot$box_width 		<- 1/(length(transitions)*4)
transitionsPlot$box_label_cex	<- 1.2
transitionsPlot$box_cex			<- 1.3
transitionsPlot$title_cex		<- 1.4
transitionsPlot$fill_clr		<- as.matrix(data.frame(aftcolours, aftcolours))
#transitionsPlot$lwd_prop_type	<- "box"


#for (col in 3:length(transitionsPlot)) {
#		table(data$Charnley_class_1yr, data$Charnley_class_2yr, data$Sex) %>%
#				transitionsPlot$addtransitionsPlot(label="2 years after")
#	}

transitionsPlot$render()
