#### PLOTTING INFORMATION AND TIME SERIES ####

# Source of data: http://www.airqualityontario.com/history/index.php

# Pollutant, Nitrogen Dioxide [NO2] -- Unit, parts per billion (ppb)
# Pollutant, Fine Particulate Matter [PM2.5] -- Unit, micrograms per cubic metre (micrograms/m3)
# Pollutant, Ozone [O3] -- Unit, parts per billion (ppb)

# Remarks: -999 for missing data. 9999 for invalid data.

library("tidyverse")
library("lubridate")
library("plotly")

## Create dataframe from existing csv file. Leave one original version for backup
pollutants.df <- read_csv("./WorkingData/pollutants_31103.csv")
glimpse(pollutants.df)

pollutants.df$Station_ID <- as.factor(pollutants.df$Station_ID)

pollutants.plot.df <- pollutants.df

## Convert all 9999 & -999 to NA for plotting and stats purposes
pollutants.plot.df$NO2[pollutants.plot.df$NO2 == 9999 | pollutants.plot.df$NO2 == -999] <- NA
pollutants.plot.df$PM25[pollutants.plot.df$PM25 == 9999 | pollutants.plot.df$PM25 == -999] <- NA
pollutants.plot.df$O3[pollutants.plot.df$O3 == 9999 | pollutants.plot.df$O3 == -999] <- NA

summary(pollutants.plot.df)

## Plots - They are not comparable in terms of units, so plot them separately...
ggplot(pollutants.plot.df,aes(DateTime,NO2)) + geom_line()
ggplot(pollutants.plot.df,aes(DateTime,PM25)) + geom_line()
ggplot(pollutants.plot.df,aes(DateTime,O3)) + geom_line()

ggplot(pollutants.plot.df,aes(DateTime,O3)) + geom_line(color="#002266") + 
  theme(legend.position = "none", panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels = scales::date_format("%m-%Y")) +
  ggtitle("O3 - Hourly Values") + ylim(0,100)


hist(pollutants.plot.df$NO2)
boxplot(pollutants.plot.df$NO2)

hist(pollutants.plot.df$O3)

## Confusing plots, play with granularity!!
# First one: Daily mean
pollutants.daily.df <- pollutants.plot.df %>% 
  mutate(day = str_pad(day(DateTime),2,"left","0"), month = str_pad(month(DateTime),2,"left","0"), year = year(DateTime)) %>% 
  unite(DayMonth, year, month, day, sep = "-") %>% group_by(DayMonth) %>% 
  summarize(dmean.NO2 = mean(NO2, na.rm = TRUE), dmean.PM25 = mean(PM25, na.rm = TRUE), dmean.O3 = mean(O3, na.rm = TRUE))
  
pollutants.daily.df$DayMonth <- ymd(pollutants.daily.df$DayMonth)
pollutants.daily.df$dmean.NO2[is.nan(pollutants.daily.df$dmean.NO2)] <- NA

# Get the 98th percentile of the daily mean for PM2.5, according to criteria of the Canadian Govt
quantile(pollutants.daily.df$dmean.PM25,0.98)

ggplot(pollutants.daily.df,aes(DayMonth,dmean.NO2)) + geom_line()
ggplot(pollutants.daily.df,aes(DayMonth,dmean.PM25)) + geom_line()
ggplot(pollutants.daily.df,aes(DayMonth,dmean.O3)) + geom_line()

ggplot(pollutants.daily.df,aes(DayMonth,dmean.O3)) + geom_line(color="#002266") + 
  theme(legend.position = "none", panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = scales::date_format("%m-%Y")) +
  ggtitle("O3 - Daily Values") + ylim(0,100)

ggplot(pollutants.daily.df[month(pollutants.daily.df$DayMonth) == 7,],aes(DayMonth,dmean.O3)) + geom_line()
range(pollutants.daily.df[month(pollutants.daily.df$DayMonth) == 7,]$dmean.O3)

# Create a plotly visualization with daily means
p <- ggplot(pollutants.daily.df,aes(DayMonth,dmean.NO2)) + geom_line()
ggplotly(p)

# Second one: Monthly mean -- not interesting according to Canadian Government criteria...
pollutants.monthly.df <- pollutants.plot.df %>% 
  mutate(month = str_pad(month(DateTime),2,"left","0"), year = year(DateTime)) %>% 
  unite(Month, year, month, sep = "-") %>% group_by(Month) %>% 
  summarize(dmean.NO2 = mean(NO2, na.rm = TRUE), dmean.PM25 = mean(PM25, na.rm = TRUE), dmean.O3 = mean(O3, na.rm = TRUE))

pollutants.monthly.df$Month <- dym(pollutants.monthly.df$Month)
#pollutants.monthly.df$dmean.NO2[is.nan(pollutants.monthly.df$dmean.NO2)] <- NA

ggplot(pollutants.monthly.df,aes(Month,dmean.NO2)) + geom_line()
ggplot(pollutants.monthly.df,aes(Month,dmean.PM25)) + geom_line()

ggplot(pollutants.monthly.df,aes(Month,dmean.O3)) + geom_line(color="#002266") + 
  theme(legend.position = "none", panel.grid = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_x_date(labels = scales::date_format("%m-%Y")) +
  ggtitle("O3 - Monthly Values") + ylim(0,100)

# (To be done) Time series; fix all the NAs before!

## (To be done) Predictions according time series