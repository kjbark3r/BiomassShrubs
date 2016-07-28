##misc code; shrub biomass estimation

#figuring out wtf i'm doing

# look at the data
###need to open channel first

measure <- sqlQuery(channel, paste("select * from MeasurePlants"))
a <- as.data.frame(unique(measure$Species))

# the one shrub we don't have an equation for
chrvis <- measure[measure$Species =="CHRVIS",]

##############################
# summarise to average across columns

#make sure you can make summarise work
shrub <- summarise(shrub, test = sum("Basal1"))
#nope
shrub <- summarise(shrub, test = sum(Basal1))
#oh duh

#now average
test <- rowMeans(shrub[,c("Basal1", "Basal2")], na.rm=TRUE)
#yas
test <- rowMeans(shrub[,c("Basal1":"Basal5")], na.rm=TRUE)
#newp
test2 <- rowMeans(subset(shrub, select = c(Basal1, Basal3)), na.rm=TRUE)
#yas
test <- rowMeans(subset(shrub, select = c(Basal1, Basal2, Basal3)), na.rm=TRUE)
#oh fuck yas

a  %>% mutate(test = mean(c(Basal1, Basal2), na.rm=TRUE))
#works but runs down column instead of just that row

  mutate(test = rowMeans(c(Basal1, Basal2), na.rm=TRUE))
#doesn't fucking work
  
  shrub$AvgBasal <- rowMeans(subset(shrub, select = c(Basal1, Basal2), na.rm=TRUE))
  #works but NAs mess it up
  #because you put the na.rm in the wrong place...

test <- shrub %>%
  summarise(eh = rowMeans(subset(shrub, select = c(Basal1, Basal2, Basal3)), na.rm=TRUE))


###########
## ESTIMATION FUNCTIONS

# how the hell do you write a function?
eqnL <- function(x, a, b) {
  grams <- a*x + b
  return(grams)
}
x <- plot.shrub[1,3]
a <- plot.shrub[1,4]
b <- plot.shrub[1,5]

eqnL(x, a, b)
#hoooooly shit i think that worked
