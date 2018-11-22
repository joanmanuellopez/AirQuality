#### DEALING WITH NAs AND ERRORS  ####

# Source of data: http://www.airqualityontario.com/history/index.php

# Pollutant, Nitrogen Dioxide [NO2] -- Unit, parts per billion (ppb)
# Pollutant, Fine Particulate Matter [PM2.5] -- Unit, micrograms per cubic metre (micrograms/m3)
# Pollutant, Ozone [O3] -- Unit, parts per billion (ppb)

# Remarks: -999 for missing data. 9999 for invalid data.

library("tidyverse")
library("lubridate")

# Function to calculate how many consecutive invalid values (9999) do we have
consecutive.invalid <- function(v,code) {

  #vector to be returned
  response <- c()
  positions <- c()
  #counter
  counter <- 0
  
  ## loop
  for(i in 1:length(v)) {
    if(v[i] != code & counter > 0) {
      response <- c(response, counter)
      positions <- c(positions, position)
      counter <- 0
    }
    else if(v[i] == code){
      if(counter == 0){
        position <- i
      }
      counter <- counter + 1
    }
  }
  
  # Add the last result to the response vector, in case the last values are invalid ones
  if(counter > 0) {
    response <- c(response, counter)
    positions <- c(positions, position)
  }
  
  #return (response)
  return(list(values=response,position=positions))
}

## Read the data file
pollutants.df <- read_csv("./WorkingData/pollutants_31103.csv", na = "NA")

## 1. First exploration
# Checking data, first glimpse
str(pollutants.df)
summary(pollutants.df)

pollutants.df$Station_ID <- as.factor(pollutants.df$Station_ID)

# Visualizar NAs & Medidas erroneas de NO2 - ¿Cómo visualizarlas?
ggplot(pollutants.df,aes(DateTime,NO2)) + geom_line()
ggplot(pollutants.df,aes(DateTime,PM25)) + geom_line()
ggplot(pollutants.df,aes(DateTime,O3)) + geom_line()

## Parece que los NAs de los sensores de NO2 y PM2.5 (-999) coinciden...
## En cambio los NAs de O3 son distintos
# Si logitudes son iguales y las posiciones son iguales --> NAs son los mismos!!
length(which(pollutants.df$NO2 == -999)) == length(which(pollutants.df$PM25 == -999)) 
# Given that lengths are the same... check that all are the same
all(which(pollutants.df$NO2 == -999) == which(pollutants.df$PM25 == -999))
# In one line, is it possible...?

## 2. Dealing with missing data and invalid data
# 2.1.A - Which percentage of data corresponds to NAs?
length(which(pollutants.df$NO2 == 9999 | pollutants.df$NO2 == -999)) * 100 / length(pollutants.df$NO2)
length(which(pollutants.df$PM25 == 9999 | pollutants.df$PM25 == -999)) * 100 / length(pollutants.df$PM25)
length(which(pollutants.df$O3 == 9999 | pollutants.df$O3 == -999)) * 100 / length(pollutants.df$O3)

# 2.1.B - Inspect how many consecutive invalid values do we have

invalid_pm25 <- consecutive.invalid(pollutants.df$PM25,9999)
invalid_no2 <- consecutive.invalid(pollutants.df$NO2,9999)
invalid_o3 <- consecutive.invalid(pollutants.df$O3,9999)

missing_pm25 <- consecutive.invalid(pollutants.df$PM25,-999)
missing_no2 <- consecutive.invalid(pollutants.df$NO2,-999)
missing_o3 <- consecutive.invalid(pollutants.df$O3,-999)

# Check when the longest consecutive periods of invalid periods took place
pollutants.df$DateTime[invalid_pm25$position[which(invalid_pm25$values > 4)]]
pollutants.df$DateTime[invalid_no2$position[which(invalid_no2$values > 5)]]
pollutants.df$DateTime[invalid_o3$position]

# Check when the longest consecutive periods of missing_value periods took place
pollutants.df$DateTime[missing_pm25$position[which(missing_pm25$values > 4)]]
pollutants.df$DateTime[missing_no2$position[which(missing_no2$values > 5)]]
pollutants.df$DateTime[missing_o3$position]

# Check the periodicity for O3 abnormal values
# (Applying this operation to the other pollutants yields random values)
diff(invalid_o3$position)
diff(missing_o3$position)

# Check histogram for consecutive values
ggplot(as.data.frame(as.factor(invalid_pm25$values)),aes(x=invalid_pm25$values)) + 
  geom_bar(color = "blue", fill = "orange")
ggplot(as.data.frame(as.factor(invalid_pm25$values)),aes(x=invalid_pm25$values)) + 
  geom_histogram(color="#787822",fill="#9195ff",binwidth=1) + scale_x_discrete(limits=c(1:9))

# 2.1.C - Check how many of them in a given dayweek / time
# Extract the time information of interest
pollutants.time.df <- pollutants.df %>% mutate(dayWeek = wday(pollutants.df$DateTime)) %>%
  mutate(hour = hour(pollutants.df$DateTime)) %>% mutate(month = month(pollutants.df$DateTime))

# By Weekday
pollutants.no2.nasbyday <- pollutants.time.df %>% filter(NO2 == 9999) %>% group_by(dayWeek) %>% 
  summarise(NaNO2 = n())

pollutants.pm25.nasbyday <- pollutants.time.df %>% filter(PM25 == 9999) %>% group_by(dayWeek) %>% 
  summarise(NaPM25 = n())

pollutants.nasbyday.df <- left_join(pollutants.pm25.nasbyday,pollutants.no2.nasbyday)
pollutants.nasbyday.df[is.na(pollutants.nasbyday.df)] <- 0

# By Hour
pollutants.no2.nasbyhour <- pollutants.time.df %>% filter(NO2 == 9999) %>% group_by(hour) %>% 
  summarise(NaNO2 = n())

pollutants.pm25.nasbyhour <- pollutants.time.df %>% filter(PM25 == 9999) %>% group_by(hour) %>% 
  summarise(NaPM25 = n())

pollutants.nasbyhour.df <- left_join(pollutants.pm25.nasbyhour,pollutants.no2.nasbyhour)
pollutants.nasbyhour.df[is.na(pollutants.nasbyhour.df)] <- 0

# By Month
pollutants.no2.nasbymonth <- pollutants.time.df %>% filter(NO2 == 9999) %>% group_by(month) %>% 
  summarise(NaNO2 = n())

pollutants.pm25.nasbymonth <- pollutants.time.df %>% filter(PM25 == 9999) %>% group_by(month) %>% 
  summarise(NaPM25 = n())

pollutants.nasbymonth.df <- left_join(pollutants.pm25.nasbymonth,pollutants.no2.nasbymonth)
pollutants.nasbymonth.df[is.na(pollutants.nasbymonth.df)] <- 0

# What happens in august for NO2? There are 61 consecutive unvalid values, from 18th to 20th

# TODO 2.1.D - Patterns in missing values and invalid data
## Calendar Heatmap!! Source: https://rpubs.com/haj3/calheatmap
