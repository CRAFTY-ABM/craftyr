################################################################
# SIMulation Properties:
################################################################
simp <- list()
simp$sim <- list()
simp$sim$worldname 				<- "ToyWorldA"
simp$sim$version				<- "V001"
simp$sim$allocversion			<- "V001AllocGen"
simp$sim$scenario				<-
simp$sim$reginalisation			<- "1"
simp$sim$regions				<- NULL
simp$sim$runid					<- 0
simp$sim$runids					<- NULL
		
### Directories ################################################################
simp$dirs <- list()
simp$dirs$project			<- "C:/Data/LURG/workspace/CRAFTY_ConsVis-ToyWorld/"
simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
simp$dirs$outputdir			<- "C:/Data/LURG/Projects/Volante/Output/"

simp$dirs$output <- list()
simp$dirs$output$data		<- paste(simp$dirs$outputdir, "Data/", sep="")
simp$dirs$output$rdata		<- paste(simp$dirs$outputdir, "RData/", sep="") 
simp$dirs$output$raster		<- paste(simp$dirs$outputdir, "Raster/", sep="") 
simp$dirs$output$figures	<- paste(simp$dirs$outputdir, "Figures/", sep="")
simp$dirs$output$reports	<- paste(simp$dirs$outputdir, "Reports/", sep="")


### CSV Column Names ###########################################################
simp$csv <- list()
simp$csv$cname_region 		<- "Region"
simp$csv$cname_tick 		<- "Tick"
simp$csv$cname_aft 			<- "Agent"
simp$csv$cname_x			<- "Y"
simp$csv$cname_y			<- "X"

### Model Data ################################################################

simp$mdata <- list()
simp$mdata$capitals 		<- c("Cprod", "Fprod", "Infra", "Grass", "Nat", "Econ")
simp$mdata$aftNames			<- c("0" = 'C_Cereal', "1" = 'NC_Cereal', "2" = 'C_Livestock', "3" = 'NC_Livestock',
		"4" = 'Forester', "5" = 'Conservationist', "6" = 'BiofuelFarmer')

### Figure Settings ###########################################################
simp$fig <- list()
simp$fig$resfactor		<- 3
simp$fig$outputformat 	<- "png" #"jpeg"
simp$fig$init			<- function() 
simp$fig$numfigs		<- 1
simp$fig$numcols		<- 1
simp$fig$height			<- 700
simp$fig$width			<- 1000
simp$fig$splitfigs		<- FALSE

library(craftyr)