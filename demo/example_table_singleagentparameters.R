library(craftyr)
simp <- param_getExamplesSimp()
aft	<- simp$mdata$aftNames[-1]

# get productivity table 
data <- input_csv_param_agents(simp, aft, filenameprefix  = "AftParams_", filenamepostfix = "")
names(data) <- gsub("Distribution", "Dist", names(data), fixed = TRUE)
agentparams <- cbind(AFT = aft, data[data["aftParamId"] == aftParamId, 
						-c(1,length(data[1,])-2,length(data[1,])-1,length(data[1,]))])

# adapt table
colnames(agentparams) <- gsub("givingUp", "GU", colnames(agentparams), fixed = TRUE)
colnames(agentparams) <- gsub("givingIn", "GI", colnames(agentparams), fixed = TRUE)

# print table
table <- xtable::xtable(agentparams,
		label= "param.agents",
		caption= "Agent Parameters"
)
print(table, sanitize.colnames.function = identity,
		sanitize.rownames.function = identity,
		include.rownames = FALSE,
		table.placement = "h!")