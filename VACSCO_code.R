#Model forms include Exponential y = aebx, Power y = axb, Linear y = ax + b, where y is biomass (g) and x is the basal diameter (mm).

setwd("C:/Users/CF2752/Documents/Sapphire Elk/VegetationSampling/Biomass/ShrubData")
data<-read.csv("VACSCOmodel.csv", header=T)
head(data)
dim(data)  #Get n here

#Look at data
par(mfrow=c(2,2))
hist(data$leafBM)
hist(data$stemBM) 
hist(data$base_diam)  

##########################################
#Leaf Biomass
##########################################
data2<-data[-c(16), ] 
hist(data2$base_diam)

#Linear model
plot(data2$base_diam, data2$leafBM)
m1<-lm(leafBM~base_diam, data=data2)
summary(m1)

#Exponential model
plot(data2$base_diam, log(data2$leafBM))
m2<-lm((log(leafBM))~base_diam, data=data2)
summary(m2)

#Power model
base_diam2<-data2$base_diam^2
plot(base_diam2,data2$leafBM)
m3<-lm(leafBM~base_diam2, data=data2)
summary(m3)

##########################################
#Stem Biomass
##########################################
#Linear model
plot(data2$base_diam, data2$stemBM)
m1<-lm(stemBM~base_diam, data=data2)
summary(m1)

#Exponential model
plot(data2$base_diam, log(data2$stemBM))
m2<-lm((log(stemBM))~base_diam, data=data2)
summary(m2)

#Power model
plot(base_diam2,data2$stemBM)
m3<-lm(stemBM~base_diam2, data=data2)
summary(m3)