# visualize distribution of mosquito lifespans at each temperature treatment -------------------------
# clear environment
rm(list=ls())

# load libraries
library(plyr)
library(ggplot2)

# read in data
mosqFiles <- list.files("Data/", pattern="*.csv")
survival.df <- lapply(paste0("Data/", mosqFiles), read.csv)
names(survival.df) <- gsub("\\.csv", "", mosqFiles) # remove file extension

# combine list of dataframes into single dataframe
lifespan <- ldply(survival.df, data.frame)

# remove rows where Ezeakacha data Std != 1 (based on rearing and growth experimental temperatures)
lifespan <-lifespan[!(lifespan$.id=="EzeakachaSurvivalData" & lifespan$Std!=1),]

# combine like data
lifespan$Age <- ifelse(is.na(lifespan$Age), lifespan$lifespan, lifespan$Age)
lifespan$Temp <- ifelse(is.na(lifespan$Temp), lifespan$temp, lifespan$Temp)

# create column for mosquito species
lifespan$Species <- ifelse((lifespan$.id=="CaladoSurvivalData"|lifespan$.id=="EzeakachaSurvivalData"), "Aedes albopictus", NA)
lifespan$Species <- ifelse(lifespan$.id=="MiazgowiczData", "Anopheles stephensi", lifespan$Species)
lifespan$Species <- ifelse(lifespan$.id=="ReisenSurvivalData", "Culex tarsalis", lifespan$Species)

# plot data
ggplot(lifespan, aes(x=factor(Temp), y=Age, fill=factor(Species))) + 
  geom_boxplot() + 
  facet_grid(. ~ Species) + 
  xlab("Temperature") +
  ylab("Lifespan") + 
  guides(fill=FALSE) +
  theme_bw() + 
  geom_point(aes(fill = factor(Species)), size = 1, shape = 21, position = position_jitterdodge()) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

# plot MMRR data --------------------------------------------------------------------------------
mmrr <- read.csv("Data/MMRRdata_main.csv", head=T)
mmrr2 <- subset(mmrr, !is.na(DPS1_f))

ggplot(mmrr2, aes(x=factor(species1), y=DPS1_f, fill=factor(genus))) + 
  geom_boxplot() + 
  facet_grid(. ~ genus, scales = "free") + 
  xlab("Species") +
  ylab("Daily survival rate") + 
  guides(fill=FALSE) +
  theme_bw() + 
  geom_point(aes(fill = factor(genus)), size = 1, shape = 21, position = position_jitterdodge()) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

