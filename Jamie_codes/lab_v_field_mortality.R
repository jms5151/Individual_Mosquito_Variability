# compare lab mortality with field mortality --------------------------------------------------
rm(list=ls()) # clear environment

# load survival functions based on lab data
source("Marta Code/Lifespan_Functions.R")

# load field data
mmrr <- read.csv("Data/mmrr_with_bioclim2_climate.csv", head=T, stringsAsFactors = F)

# subset field data to species with corresponding lab data 
spp <- c("Aedes albopictus", "Aedes aegypti", "Anopheles stephensi", "Culex quinquefasciatus", "Culex tarsalis")
mmrr.spp <- mmrr[mmrr$species %in% spp, ] 

# remove rows with NA values for temperature
mmrr.spp <- subset(mmrr.spp, !is.na(Bioclim_Mean_temp))

# calculate expected daily mortality based on lab fits
mmrr.spp$lab_DailyMortality <- ifelse(mmrr.spp$species == "Aedes albopictus", 1/lf.Aalb(mmrr.spp$Bioclim_Mean_temp), NA)
mmrr.spp$lab_DailyMortality <- ifelse(mmrr.spp$species == "Aedes aegypti", 1/lf.Aaeg(mmrr.spp$Bioclim_Mean_temp), mmrr.spp$lab_DailyMortality)
mmrr.spp$lab_DailyMortality <- ifelse(mmrr.spp$species == "Anopheles stephensi", 1/lf.Anst(mmrr.spp$Bioclim_Mean_temp), mmrr.spp$lab_DailyMortality)
mmrr.spp$lab_DailyMortality <- ifelse(mmrr.spp$species == "Culex quinquefasciatus", 1/lf.Cqui(mmrr.spp$Bioclim_Mean_temp), mmrr.spp$lab_DailyMortality)
mmrr.spp$lab_DailyMortality <- ifelse(mmrr.spp$species == "Culex tarsalis", 1/lf.Ctar(mmrr.spp$Bioclim_Mean_temp), mmrr.spp$lab_DailyMortality)

# load library
library(ggplot2)

# plot colored by temperature 
ggplot(mmrr.spp, aes(x=lab_DailyMortality, y=DailyMortality, fill=Bioclim_Mean_temp)) + 
  xlab("Lab daily mortality rate") +
  ylab("Field daily mortality rate") +
  theme_classic() + 
  geom_point(aes(fill = Bioclim_Mean_temp), size = 5, shape = 21)

# plot colored by species 
ggplot(mmrr.spp, aes(x=lab_DailyMortality, y=DailyMortality, fill=species)) + 
  xlab("Lab daily mortality rate") +
  ylab("Field daily mortality rate") +
  theme_classic() + 
  geom_point(aes(fill = species), size = 5, shape = 21)

# save data
mmrr.spp <- mmrr.spp[,c("genus", "species", "Bioclim_Mean_temp", "DailyMortality", "lab_DailyMortality")]
colnames(mmrr.spp) <- c("Genus", "Species", "Mean_temperature", "Field_daily_mortality_rate", "Lab_daily_mortality_rate")
write.csv(mmrr.spp, "Data/mmrr_v_lab_mortality_by_temp.csv")
