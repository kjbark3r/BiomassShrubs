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
L <- function(x, a, b) {
  grams <- a*x + b
  return(grams)
}
x <- plot.shrub[1,3]
a <- plot.shrub[1,4]
b <- plot.shrub[1,5]

a$test <- L(x, a, b)

#hoooooly shit i think that worked
a$testL <- L(x, 6.3794, b)
a$testE <- E(x, 6.3794, b)
a$testP <- P(x, 6.3794, b)
  #yup. awesome.

#now make it a new column in the dataframe
test$g.Leaves <- test$fcn.Leaves(test$Basal, test$a.Leaves, test$b.Leaves)
  #nope, error: attempt to apply non-function
test <- plot.shrub %>%
  mutate(g.Leaves = test$fcn.Leaves(test$Basal, test$a.Leaves, test$b.Leaves))
  #nope, r-splosion. i think it doesn't know that fcn.Leaves is the actual function 
plot.shrub$fcn.Leaves[1]
test <- plot.shrub
test$fcn.Leaves <- as.character(test$fcn.Leaves)
test$g.Leaves <- test$fcn.Leaves(test$Basal, test$a.Leaves, test$b.Leaves)
  #still nope
test$fcn.Leaves[1]
E
"E"

#time to consult the stackoverflow gods

#update an existing column - close to what i need... may have to use this
`updt<-` <- function(x, ..., value) {
  ## x is the object to be manipulated, value the object to be assigned
  x$lbl <- paste0(x$lbl, value)
  x
}

#apply is probably what you need from this one
lump <- function(db, spp.list, new.spp) { #input spp.list as a c('spp.a', 'spp.b', ...), and new.spp must be in quotes (e.g. 'new.spp')
  mini.db <- subset(db, select=spp.list);
  newcol <- as.vector(apply(mini.db, 1, max, na.rm=T));
  newcol[newcol==-Inf] <- NA;
  db[new.spp] <- newcol;
  db <- db[, !names(db) %in% spp.list];
  return(as.data.frame(db));
}
View(test)

#even better, try something like this
#(the 1 means apply by row)
dataFrame$newColumn <- apply(dataFrame, 1, function(x) { . . . } )

test$g.Leaves <- apply(test, 1, test$fcn.Leaves)
  #you want something like this, but it still doesn't know $fcn is the fcn

test$g.Leaves <- apply(test, 1, ifelse(test$fcn.Leaves == "E", E, L))
#Error in rep(yes, length.out = length(ans)) : 
#attempt to replicate an object of type 'closure'

x <- test$Basal; a <- test$a.Leaves; b <- test$b.Leaves
test$g.Leaves <- ifelse(test$fcn.Leaves == "E", E, L)

test$g.Leaves <- apply(test, 1, test$fcn.Leaves(test$Basal, test$a.Leaves, test$b.Leaves))
#still not getting th function thing

test$g.Leaves <- apply(test, 1, ifelse(test$fcn.Leaves == "E", 
                                       E(test$Basal, test$a.Leaves, test$b.Leaves), NA))
#stilllllll not getting th function thing

test$g.Leaves <- apply(test, 1, ifelse(test$fcn.Leaves == "E", 
                                           E(12, 3, 7), NA))
E(12,3,7)
#doesn't think E is a function in the apply() code, but does in above line

test$ugh <- apply(test, 1, E)
#says a is missing but not x. maybe test is x?

test$ugh <- apply(test$Basal, 1, E)
#Error in apply(test$Basal, 1, E) : dim(X) must have a positive length

test$ugh <- apply(test$Basal, 1, E, a = test$a.Leaves, b = test$b.leaves)
#same error as above, of course

test$ugh <- apply(test, 1, E, a = test$a.Leaves, b = test$b.Leaves)
#Error in b * x : non-numeric argument to binary operator

test$ugh <- apply(test, 1, E, x = test$Basal, a = test$a.Leaves, b = test$b.Leaves)
#Error in FUN(newX[, i], ...) : unused argument (newX[, i])

test$fuuuuck <- E(test$Basal, test$a.Leaves, test$b.Leaves)
#hahaha all it took was turrets and hopelessness

test$g.Leaves <- ifelse(test$fcn.Leaves == "E", E(test$Basal, test$a.Leaves, test$b.Leaves), 
                        ifelse(test$fcn.Leaves == "L", L(test$Basal, test$a.Leaves, test$b.Leaves)),
                        P(test$Basal, test$a.Leaves, test$b.Leaves))
#unused argument P() for some reason - but it should be using P - there are no Ls in leaf fcns

test$g.Leaves <- ifelse(test$fcn.Leaves == "E", E(test$Basal, test$a.Leaves, test$b.Leaves), 
                        ifelse(test$fcn.Leaves == "L", L(test$Basal, test$a.Leaves, test$b.Leaves)))
#argument "no" is missing

test$g.Leaves <- ifelse(test$fcn.Leaves == "E", E(test$Basal, test$a.Leaves, test$b.Leaves), 
                        ifelse(test$fcn.Leaves == "L", L(test$Basal, test$a.Leaves, test$b.Leaves)),
                        0)
#but if i add the "no" argument i get an error that it's unused, wtfffffff

test$g.Leaves <- ifelse(test$fcn.Leaves == "E", E(test$Basal, test$a.Leaves, test$b.Leaves), 
                        ifelse(test$fcn.Leaves == "L", L(test$Basal, test$a.Leaves, test$b.Leaves),
                               ifelse(test$fcn.Leaves == "P", P(test$Basal, test$a.Leaves, test$b.Leaves),
                                      NA)))
any(is.na(test$g.Leaves))
#way to go, genius. next time try getting the parens right the first time.




