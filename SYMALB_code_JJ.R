#Model forms include Exponential y = aebx, Power y = axb, Linear y = ax + b, where y is biomass (g) and x is the basal diameter (mm).

setwd("E:/Sapphire Vegetation Database/Shrub Biomass/ShrubData")
data<-read.csv("SYMALBmodel.csv", header=T)
head(data)
attach(data)
dim(data)  #Get n here

#Look at data
par(mfrow=c(2,2))
hist(leafBM)
hist(stemBM) 
hist(base_diam)  

##########################################
#Leaf Biomass
##########################################
#Linear model
plot(base_diam, leafBM)
m1<-lm(leafBM~base_diam)
summary(m1)

#Exponential model
plot(base_diam, log(leafBM))
m2<-lm((log(leafBM))~base_diam)
summary(m2)

#Power model
base_diam2<-(base_diam)^2
plot(base_diam2,leafBM)
m3<-lm(leafBM~base_diam2)
summary(m3)

##########################################
#Stem Biomass
##########################################
#Linear model
plot(base_diam, stemBM)
m1<-lm(stemBM~base_diam)
summary(m1)

#Exponential model
plot(base_diam, log(stemBM))
m2<-lm((log(stemBM))~base_diam)
summary(m2)

#Power model
base_diam2<-(base_diam)^2
plot(base_diam2,stemBM)
m3<-lm(stemBM~base_diam2)
summary(m3)

#Select top model from Adjusted R squared