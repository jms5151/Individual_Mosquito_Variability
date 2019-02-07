# visualize distribution of mosquito lifespans at each temperature treatment -------------------------
# clear environment
rm(list=ls())

# load libraries
library(plyr)
library(ggplot2)

# plot MMRR data - daily mortality -------------------------------------------------------------------
mmrr <- read.csv("Data/mmrr_with_bioclim2_climate.csv", head=T, stringsAsFactors = F)

# round temperature to nearest degree and remove NA valeus 
mmrr$Bioclim_Mean_temp <- round(mmrr$Bioclim_Mean_temp)
mmrr <- subset(mmrr, !is.na(Bioclim_Mean_temp))

# remove surveys where mosquitoes were captures as adults (unknown age)
# 1. field-collected immatures; 2. field-collected adults; 3. Mixed; 4. lab-sourced adults  
mmrr <- subset(mmrr, mos_origin == 1 | mos_origin == 4)

# plot by genus
genera <- c("Aedes", "Anopheles", "Culex")

for (i in 1:length(genera)){
  df <- subset(mmrr, genus == genera[i])
  sppPlot <- ggplot(df, aes(x=factor(Bioclim_Mean_temp), y=DailyMortality, fill=species)) + 
    xlab("Mean temperature") +
    ylab("Daily survival rate") + 
    theme_classic() + 
    geom_point(aes(fill = species), size = 5, shape = 21, position = position_jitterdodge())
  fileName <- paste0("Figures/Daily_mortality_by_temperature_", genera[i], "_lab-sourced.tiff")
  ggsave(sppPlot, file=fileName)
}

# plot lab data - lifespan --------------------------------------------------------------------------
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