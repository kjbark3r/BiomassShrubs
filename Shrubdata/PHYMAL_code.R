#Model forms include Exponential y = aebx, Power y = axb, Linear y = ax + b, where y is biomass (g) and x is the basal diameter (mm).

setwd("E:/Sapphire Vegetation Database/Shrub Biomass/ShrubData")
data<-read.csv("PHYMALmodel_remove3.csv", header=T)                #PHYMALmodel=full dataset, _remove2=took out Plot 1020 & 1019, _remove3=also took out Plot 825 
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

#Diagnostic Statistics
residuals(m1)
boxplot(residuals(m1), main="Box Plot of Residuals", horizontal=TRUE)    #sum of residuals should be 0, look for symmetrical spread around 0
yhat<-fitted.values(m1)
yhat
#plot residuals against fits, look for values outside of |4|; evenly spaced w/ no pattern; assumption of linearity and constant variance
plot(yhat,residuals(m1), xlab="Fitted", ylab="Residuals", main="Diagnostic Plot Residauls vs. Fits")
abline(h=0)
#Normal probablility plot, look for points to be symmetric and nearly linear
m1.stdres<-rstandard(m1)
qqnorm(m1.stdres, xlab="Normal Hours", ylab="Standardized Residuals", main="Normal Probability Plot for Residuals")
qqline(m1.stdres)

#Standardized deviance residuals
m1.dev<-rstandard(m1)
#Hat values
m1.hat<-hatvalues(m1)
#Standardized Pearson residuals
m1.pear<_residuals(m1, type="Pearson")/sqrt(1-m1.hat)
#Cook's Distance
m1.cook<-cooks.distance(m1)
#delta X squared
m1.detlaX<-m1.pear^2
#delta deviance
m1.deltadev<-m1.dev^2


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

detach(BERREPdata)

#Select top model from Adjusted R squared