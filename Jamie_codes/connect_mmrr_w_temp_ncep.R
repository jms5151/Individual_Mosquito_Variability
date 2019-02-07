# calculate temperature for mmrr data by lat/lon and time of survey using NCEP temperature data
# clear environment
rm(list=ls())

# load libraries
library(rvest)
library(ncdf4)
library(ggplot2)

# load data
mmrr <- read.csv("Data/MMRRdata_main.csv", head=T, stringsAsFactors = F)

# make single column with estimated daily survival by primary or alternative method 
mmrr$DFS <- ifelse(!is.na(mmrr$DPS1_f), mmrr$DPS1_f, mmrr$DPS2_f)

# format dates
mmrr$date_st <- as.Date(mmrr$date_st, "%d/%m/%Y %M:%S")
mmrr$date_start <- mmrr$date_st - 14
mmrr$date_end <- mmrr$date_st + 14
mmrr$year <- substr(mmrr$date_st, 1, 4)

# subset unique survival data
mmrr2 <- unique(subset(mmrr[,c("MRR_ID", "Country", "site_name", "lat", "long", "date_start", "date_end", "genus", "species1", "species2", "DFS", "year", "mos_origin")], !is.na(DFS)))

# get climate data --------------------------------------
years <- sort(unique(as.numeric(mmrr$year)))
years <- subset(years, years >= 1948) # data is not available before 1948
noaa_esrl_url_base <- "ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface/air.sig995."

for (i in years){
  noaaURL <- paste0(noaa_esrl_url_base, i, ".nc")
  destFilName <- paste0("climate_data/noaa_esrl_", i, ".nc")
  download.file(url=noaaURL, destfile=destFilName, mode="wb") 
}

# concatenate climate data ------------------------------
years <- sort(unique(as.numeric(mmrr2$year)))

# if start date is before new year, adjust start date to January 1
startyear <- substr(mmrr2$date_start, 1, 4)
endyear <- substr(mmrr2$date_end, 1, 4)
indexes <- which(as.numeric(startyear) - as.numeric(endyear) == -1)
mmrr2$date_start[indexes] <- paste0(mmrr2$year[indexes], "-01-01")

# add columns for id and mean temperature
mmrr2$ID <- seq(1,nrow(mmrr2), 1)
mmrr2$NCEP_Mean_temp <- NA

# open nc files and extract climate data for each location and date with mmrr data
for (j in years){
  fileName <- paste0("C:/Users/Jamie/Desktop/climate_data/noaa_esrl_", j, ".nc")
  nc_tmp <- nc_open(fileName)
  nc_temp <- ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1]) # separate temperature data
  missingValues <- ncatt_get(nc_tmp, attributes(nc_tmp$var)$names[1])$missing_value
  nc_lat <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[1]) # separate latitude data
  nc_lon <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[2]) # separate longitude data
  nc_time <- ncvar_get(nc_tmp, attributes(nc_tmp$dim)$names[3]) # separate date data
  nc_time <- as.Date(nc_time/24, origin = "1800-01-01") # format date data
  yearsub <- subset(mmrr2, year==j)
  for (k in 1:nrow(yearsub)){
    first_date <- which(nc_time == yearsub$date_start[k]) 
    last_date <- which(nc_time == yearsub$date_end[k])    
    closestLat <- which.min(abs(nc_lat-mmrr2$lat[k]))
    closestLon <- which.min(abs(nc_lon-mmrr2$long[k]))
    nc_temp2 <- nc_temp[closestLat,closestLon,first_date:last_date] # subset temperature array to date, latitude, and longitude of interest
    nc_temp2[nc_temp2 == missingValues] <- NA
    idIndex <- which(mmrr2$ID == yearsub$ID[k])
    mmrr2$NCEP_Mean_temp[idIndex] <- round(mean(nc_temp2,na.rm=T)−273.15) # Kelvin to celsius
  }
  nc_close(nc_tmp)
}

# save data
write.csv(mmrr2, "Data/mmrr_with_climate.csv", row.names=F)