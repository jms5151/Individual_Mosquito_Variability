# clear environment
rm(list=ls())

# load libraries
library(raster)
library(rgdal)

# load data
mmrr <- read.csv("Data/MMRRdata_main.csv", head=T)
spp <- read.csv("Data/vec_species_JMCaldwell.csv", head=T)
colnames(spp)[1] <- "species1"

# combine mmrr data with species names
mmrr2 <- merge(mmrr, spp[,c("genus", "species1", "species")], by=c("genus", "species1"))

# make single column with estimated daily survival by primary or alternative method & remove columns with no dps 
mmrr2$DFS <- ifelse(!is.na(mmrr2$DPS1_f), mmrr2$DPS1_f, mmrr2$DPS2_f)
mmrr2 <- subset(mmrr2, !is.na(DFS))

# calculate daily mortality as one minus daily female survival 
mmrr2$DailyMortality <- 1-mmrr2$DFS

# format dates
mmrr2$date_st <- as.Date(mmrr2$date_st, "%d/%m/%Y %M:%S")
mmrr2$month <- format(mmrr2$date_st, "%m")

# load bioclim 2 data
bioclimfiles <- list.files("Data/bioclim2/")

for (i in 1:length(bioclimfiles)){
  str_name <- paste0("Data/bioclim2/", bioclimfiles[i])
  imported_raster=raster(str_name)
  assign(bioclimfiles[i], imported_raster)
}

# remove data with missing coordinates
mmrr2 <- subset(mmrr2, !is.na(lat) & !is.na(long))

# make coordinates spatial
spdf <- SpatialPointsDataFrame(coords = mmrr2[,c("long", "lat")], data = mmrr2, proj4string = CRS("+proj=longlat +datum=WGS84"))

# extract bioclim data for each mmrr survey by month
rasterFiles <- setNames(lapply(ls(pattern=".tif"), function(x) get(x)), ls(pattern=".tif"))

for (j in 1:length(rasterFiles)){
  p <- spTransform(spdf, crs(rasterFiles[[j]]))       
  newcolname <- substr(bioclimfiles[j], 16, 17)
  mmrr2[,newcolname] <- extract(rasterFiles[[j]], p)
}

# format data
for (k in 1:nrow(mmrr2)){
  colindex <- which(colnames(mmrr2) == mmrr2$month[k])
  mmrr2$Bioclim_Mean_temp[k] <- mmrr2[k,colindex] 
}

# remove extra columns
mmrr3 <- mmrr2[ , -which(names(mmrr2) %in% c("month","01","02","03","04","05","06","07","08","09","10","11","12"))]

# save data
write.csv(mmrr3, "Data/mmrr_with_bioclim2_climate.csv", row.names = F)
