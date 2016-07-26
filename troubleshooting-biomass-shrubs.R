##misc code; shrub biomass estimation

#figuring out wtf i'm doing

# look at the data
###need to open channel first

measure <- sqlQuery(channel, paste("select * from MeasurePlants"))
a <- as.data.frame(unique(measure$Species))

# the one shrub we don't have an equation for
chrvis <- measure[measure$Species =="CHRVIS",]
