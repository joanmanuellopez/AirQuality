#### AIR QUALITY IN TORONTO ####

# Data corresponding to three pollutants (NO2, PM2.5 and O3 )
# at one single station (Toronto Downtown) in one year (2012)
# Source of data: http://www.airqualityontario.com/history/index.php

# Pollutant, Nitrogen Dioxide [NO2] -- Unit, parts per billion (ppb)
# Pollutant, Fine Particulate Matter [PM2.5] -- Unit, micrograms per cubic metre (micrograms/m3)
# Pollutant, Ozone [O3] -- Unit, parts per billion (ppb)

# Note: Previous cleaning work has been performed in a text editor before importing the file

library("tidyverse")
library("stringr")

nitrogen_df <- read_csv("cleanNO2_31103_2012.csv")
fineparticulate_df <- read_csv("cleanPM25_31103_2012.csv")
ozone_df <- read_csv("cleanO3_31103_2012.csv")

# These attributes don't add information, however they could be useful
# in case of merging information of other stations or other pollutants
nitrogen_df$Station_ID <- as.factor(nitrogen_df$Station_ID)
nitrogen_df$Pollutant <- as.factor(nitrogen_df$Pollutant)
#nitrogen_df$Date <- as.character(nitrogen_df$Date)
fineparticulate_df$Station_ID <- as.factor(fineparticulate_df$Station_ID)
fineparticulate_df$Pollutant <- as.factor(fineparticulate_df$Pollutant)

ozone_df$Station_ID <- as.factor(ozone_df$Station_ID)
ozone_df$Pollutant <- as.factor(ozone_df$Pollutant)


#### Create a dataframe for the whole year with the three pollutants ####
nitrogen.1y.df <- nitrogen_df

# let's do a gather to get all the hours of the day in a row
nitrogen.1y.df <- nitrogen.1y.df %>% gather(Hour, NO2, H01:H24)

nitrogen.1y.df$Hour <- as.integer(substr(nitrogen.1y.df$Hour,2,3)) - 1
nitrogen.1y.df$Hour <- str_pad(nitrogen.1y.df$Hour,2,"left","0")

nitrogen.1y.df <- cbind(nitrogen.1y.df,paste(nitrogen.1y.df$Date,nitrogen.1y.df$Hour), stringsAsFactors=FALSE)

colnames(nitrogen.1y.df)[6] <- "DateTime"
nitrogen.1y.df <- nitrogen.1y.df[,c(ncol(nitrogen.1y.df), 1:(ncol(nitrogen.1y.df)-1))]

# Store DateTime as Date type and arrange the dataset according to date
nitrogen.1y.df$DateTime <- strptime(nitrogen.1y.df$DateTime, "%Y-%m-%d %H")
nitrogen.1y.df <- nitrogen.1y.df[order(nitrogen.1y.df$DateTime),]

nitrogen.1y.df$Date <- NULL
nitrogen.1y.df$Hour <- NULL

## And repeat for the fine particulates PM2.5!!
finepart.1y.df <- fineparticulate_df

# let's do a gather to get all the hours of the day in a row
finepart.1y.df <- finepart.1y.df %>% gather(Hour,PM25,H01:H24)

finepart.1y.df$Hour <- as.integer(substr(finepart.1y.df$Hour,2,3)) - 1
finepart.1y.df$Hour <- str_pad(finepart.1y.df$Hour,2,"left","0")

finepart.1y.df <- cbind(finepart.1y.df,paste(finepart.1y.df$Date,finepart.1y.df$Hour), stringsAsFactors=FALSE)

colnames(finepart.1y.df)[6] <- "DateTime"
finepart.1y.df <- finepart.1y.df[,c(ncol(finepart.1y.df), 1:(ncol(finepart.1y.df)-1))]

# Store DateTime as Date type and arrange the dataset according to date
finepart.1y.df$DateTime <- strptime(finepart.1y.df$DateTime, "%Y-%m-%d %H")
finepart.1y.df <- finepart.1y.df[order(finepart.1y.df$DateTime),]

finepart.1y.df$Date <- NULL
finepart.1y.df$Hour <- NULL

## And once again for the ozone O3!!
ozone.1y.df <- ozone_df

# let's do a gather to get all the hours of the day in a row
ozone.1y.df <- ozone.1y.df %>% gather(Hour,O3,H01:H24)

ozone.1y.df$Hour <- as.integer(substr(ozone.1y.df$Hour,2,3)) - 1
ozone.1y.df$Hour <- str_pad(ozone.1y.df$Hour,2,"left","0")

ozone.1y.df <- cbind(ozone.1y.df,paste(ozone.1y.df$Date,ozone.1y.df$Hour), stringsAsFactors=FALSE)

colnames(ozone.1y.df)[6] <- "DateTime"
ozone.1y.df <- ozone.1y.df[,c(ncol(ozone.1y.df), 1:(ncol(ozone.1y.df)-1))]

# Store DateTime as Date type and arrange the dataset according to date
ozone.1y.df$DateTime <- strptime(ozone.1y.df$DateTime, "%Y-%m-%d %H")
ozone.1y.df <- ozone.1y.df[order(finepart.1y.df$DateTime),]

ozone.1y.df$Date <- NULL
ozone.1y.df$Hour <- NULL

## Finally, let's join all dataframes to have all information in a single dataset - Keep it Simple!!!
pollutants.df <- nitrogen.1y.df
pollutants.df$PM25 <- finepart.1y.df$PM25
pollutants.df$O3 <- ozone.1y.df$O3
pollutants.df$Pollutant <- NULL

## Generate a new csv file to work with in the following steps!!
write.csv(pollutants.df,"./WorkingData/pollutants_31103.csv",row.names = FALSE)
