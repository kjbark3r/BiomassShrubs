#Model forms include Exponential y = aebx, Power y = axb, Linear y = ax + b, where y is biomass (g) and x is the basal diameter (mm).

setwd("C:/Users/CF2752/Documents/Sapphire Elk/VegetationSampling/Biomass/ShrubData")
data<-read.csv("VACMEMmodel.csv", header=T)
head(data)
dim(data)  #Get n here
#Look at data
par(mfrow=c(2,2))
hist(leafBM)
hist(stemBM)  #Remove stemBM >3
hist(base_diam)  #Remove base_diam>16

##########################################
#Leaf Biomass
##########################################
data2<-data[-c(34), ] 
hist(data2$base_diam)

#Linear model
plot(data2$base_diam, data2$leafBM)
m1<-lm(leafBM~base_diam, data=data2)
summary(m1)

#Exponential model
plot(data2$base_diam, data2$log(leafBM))
m2<-lm((log(leafBM))~base_diam, data=data2)
summary(m2)

#Power model
base_diam2<-(data2$base_diam)^2
plot(base_diam2,data2$leafBM)
m3<-lm(leafBM~base_diam2, data=data2)
summary(m3)

##########################################
#Stem Biomass
##########################################
data3<-data[-c(15,34), ] 
par(mfrow=c(1,2))
hist(data3$stemBM)
hist(data3$base_diam)

#Linear model
plot(data3$base_diam, data3$stemBM)
m4<-lm(stemBM~base_diam, data=data3)
summary(m4)

#Exponential model
plot(data3$base_diam, log(data3$stemBM))
m5<-lm((log(stemBM))~base_diam, data=data3)
summary(m5)

#Power model
base_diam2<-data3$base_diam^2
plot(base_diam2,data3$stemBM)
m6<-lm(stemBM~base_diam2, data=data3)
summary(m6)