##########################################################
#### HERBACEOUS BIOMASS ESTIMATION - NSERP STUDY AREA ####
################## KJB  July 2016  #######################
##########################################################

#### NOTE: MUST USE 32 BIT R TO CONNECT TO ACCESS ####

## WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\BiomassShrubs"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\BiomassShrubs"
	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		  cat("Are you SURE you got that file path right?\n")
		  }
	  }

## PACKAGES

library(RODBC)
library(dplyr)
library(tidyr)
	
#########
## DATA - READ IN AND SET UP

#Connect to Access phenology database (work computer or laptop)
if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/SapphireElkProject_VegetationDatabase.accdb")
  } else {
    if(file.exists(wd_laptop)) {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/SapphireElkProject_VegetationDatabase.accdb")
    } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)


##########
## SHRUBS

# equations to estimate biomass
shrub.eqn <- read.csv("shrub-biomass-equations.csv")
  shrub.eqn$Species <- as.character(shrub.eqn$Species)

# measured shrub data - per quadrat
quadrat.shrub <- sqlQuery(channel, paste("select * from MeasurePlants"))
colnames(quadrat.shrub) <- c("Date", "PlotID", "PlotM", "Species", "Basal1", "Basal2", "Basal3", 
                     "Basal4", "Basal5", "nStems")
quadrat.shrub$Species <- trimws(quadrat.shrub$Species)
quadrat.shrub[, 5:9][quadrat.shrub[, 5:9] == 0] <- NA #make 0s NAs so averaging works
quadrat.shrub <- quadrat.shrub %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".", Date,".",PlotM, sep=""))
quadrat.shrub$AvgBasal <- rowMeans(subset(quadrat.shrub, select = (Basal1:Basal5)), na.rm=TRUE)

shrub <- quadrat.shrub %>%
  group_by(PlotVisit, Species) %>%
  summarise(Basal = mean(AvgBasal)) %>%
  left_join(shrub.eqn, by = "Species") %>%
  ungroup()

# equation functions
L <- function(x, a, b) {
  grams <- a*x + b
  return(grams)
}

E <- function(x, a, b) {
  grams <- a*(exp(1)^(b*x))
  return(grams)
}

P <- function(x, a, b) {
  grams <- a*(x^b)
  return(grams)
}


shrub$g.Leaves <- ifelse(shrub$fcn.Leaves == "E", E(shrub$Basal, shrub$a.Leaves, shrub$b.Leaves), 
                        ifelse(shrub$fcn.Leaves == "L", L(shrub$Basal, shrub$a.Leaves, shrub$b.Leaves),
                               ifelse(shrub$fcn.Leaves == "P", P(shrub$Basal, shrub$a.Leaves, shrub$b.Leaves),
                                      NA)))
shrub$g.Stems <- ifelse(shrub$fcn.Stems == "E", E(shrub$Basal, shrub$a.Stems, shrub$b.Stems), 
                         ifelse(shrub$fcn.Stems == "L", L(shrub$Basal, shrub$a.Stems, shrub$b.Stems),
                                ifelse(shrub$fcn.Stems == "P", P(shrub$Basal, shrub$a.Stems, shrub$b.Stems),
                                       NA)))
shrub <- shrub %>% mutate(ShrubBiomass = g.Leaves+g.Stems)


  


# LIFE FORM 
spp <- sqlQuery(channel, paste("select PlantCode, LifeForm, NameScientific
                                 from NSERP_SP_list"))
spp <- rename(spp, Species = PlantCode)
spp$LifeForm <- trimws(spp$LifeForm)
  
# CLASSIFICATION - plus quadrat id, quadrat-visit ID, plot-visit ID, life form, genus
classn <- sqlQuery(channel, paste("select * from Classification"))
  colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
  classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
classn <- classn %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
	mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  left_join(spp, by = "Species")
classn$Genus <- sapply(strsplit(as.character(classn$NameScientific), " "), "[", 1)
for(i in 1:nrow(classn)) {  
  classn$LifeForm[i] <- ifelse(grepl(' GRASS| JUNCACEAE |CARE ', classn$Species[i]), "graminoid", 
                               ifelse(grepl('UNK ', classn$Species[i]), "forb", next))
}

# HERBACEOUS COVER - creating manually because some recorded numbers are incorrect
cover <- classn %>%
  subset(!PlotM == 10 & !PlotM == 30) %>% #remove non-clipplots
  group_by(QuadratVisit, LifeForm) %>%
  summarise(Wt = sum(Total)) %>%
  spread(LifeForm, Wt) %>%
  rename(ForbCov = forb, GrassCov = graminoid) 
cover$ForbCov[is.na(cover$ForbCov)] <- 0; cover$GrassCov[is.na(cover$GrassCov)] <- 0

# CLIP PLOTS - plus quadrat ID, quadrat-visit ID
clip <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(clip) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
clip <- clip %>%
  mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep=""))

# FORAGE PLANTS
foragespp <- read.csv("foragespecies.csv")

#########
## DATA - MANIPULATIONS/CALCULATIONS

#per quadrat - biomass, all herbaceous (by life form)
quadrat <- clip %>%
  select(QuadratVisit, PlotVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) 
quadrat$ForbWt[is.na(quadrat$ForbWt)] <- 0 #replace NA with 0 
quadrat$GrassWt[is.na(quadrat$GrassWt)] <- 0  
quadrat$AllHerbWt <- quadrat$ForbWt + quadrat$GrassWt

#per quadrat - biomass, all herbaceous (by species)
quadrat.spp <- left_join(cover, classn, by = "QuadratVisit") %>%
  select(-PlotVisit) #avoid duplicated column name after next join
  #rescale species % cover
quadrat.spp$RescaledCover <- ifelse(quadrat.spp$LifeForm == "forb", quadrat.spp$Total/quadrat.spp$ForbCov,
                                      ifelse(quadrat.spp$LifeForm == "graminoid", 
                                             quadrat.spp$Total/quadrat.spp$GrassCov, 
                                             ifelse(NA)))
quadrat.spp <- left_join(quadrat.spp, quadrat, by = "QuadratVisit")
quadrat.spp <- subset(quadrat.spp, select = c(PlotVisit, QuadratVisit, Species, Genus, 
                                              RescaledCover, LifeForm, ForbCov, GrassCov, 
                                              ForbWt, GrassWt, AllHerbWt))
  #estimate species weight based on adjusted %cover
quadrat.spp$ClipGrams <- ifelse(quadrat.spp$LifeForm == "forb", quadrat.spp$RescaledCover*quadrat.spp$ForbWt,
                                ifelse(quadrat.spp$LifeForm == "graminoid", quadrat.spp$RescaledCover*quadrat.spp$GrassWt,
                                       ifelse(NA)))
  #remove quadrats without clip plots
quadrat.spp <- quadrat.spp[!is.na(quadrat.spp$ClipGrams),]

#per plot: herbaceous and forage biomass (forb, grass, both)
herb <- quadrat %>%
  group_by(PlotVisit) %>%
  summarise(ForbBiomass = mean(ForbWt)*1.33333, GrassBiomass = mean(GrassWt)*1.33333, 
            HerbBiomass = mean(AllHerbWt)*1.33333) 
forage <- foragespp %>%
  select(Genus, CumAve) %>%
  transmute(Genus, ForagePlant = ifelse(is.na(CumAve), "No", "Yes")) %>%
  right_join(quadrat.spp, by = "Genus") 
forage$ForagePlant <- ifelse(is.na(forage$ForagePlant), "No", "Yes")
forage$ForageGrams <- ifelse(forage$ForagePlant == "Yes", forage$ClipGrams, 0)
forage <- forage[!duplicated(forage),]
forage <- forage %>%
  group_by(QuadratVisit, LifeForm) %>%
  summarise(ForageG = sum(ForageGrams)) %>%
  spread(LifeForm, ForageG) %>% #0s have lifeform in plot but not clip plot. NAs don't have lifeform
  rename(ForageForbG = forb, ForageGrassG = graminoid) %>%
  mutate(PlotVisit = sub("(.*)[.].*", "\\1", QuadratVisit)) #all chars before last "." of quadvisit
  forage$ForageForbG[is.na(forage$ForageForbG)] <- 0
  forage$ForageGrassG[is.na(forage$ForageGrassG)] <- 0
  #below code is for 2 clip plots  with missing forb species data
  #see missingdata-estimations.R and Nutrition Quicknotes for more info
  forage[forage$QuadratVisit %in% "323.2014-06-30.20","ForageForbGrams"] <- 
    quadrat[quadrat$QuadratVisit %in% "323.2014-06-30.20","ForbWt"]*0.2 #ff323
  forage[forage$QuadratVisit %in% "344.2014-06-16.20","ForageForbGrams"] <-  
    quadrat[quadrat$QuadratVisit %in% "344.2014-06-16.20","ForbWt"]*0.3337 #ff344
forage <- forage %>%
  ungroup() %>%
  group_by(PlotVisit) %>%
  summarise(ForageForbBiomass = mean(ForageForbG)*1.33333, ForageGrassBiomass = mean(ForageGrassG)*1.33333) 
  forage$ForageHerbBiomass <- forage$ForageForbBiomass + forage$ForageGrassBiomass

biomass <- full_join(herb, forage, by = "PlotVisit") %>%
  mutate(PlotID = substr(PlotVisit, 1, 3)) %>%
  mutate(Date = substr(PlotVisit, 5, 14)) %>%
  select(PlotID, Date, PlotVisit, ForbBiomass, GrassBiomass, HerbBiomass,
         ForageForbBiomass, ForageGrassBiomass, ForageHerbBiomass)

write.csv(biomass, file = "biomass-phenology.csv", row.names = FALSE)





