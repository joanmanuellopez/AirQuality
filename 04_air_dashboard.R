#### CREATING A DATASET FOR DASHBOARDING ####

# Source of data: http://www.airqualityontario.com/history/index.php

# Pollutant, Nitrogen Dioxide [NO2] -- Unit, parts per billion (ppb)
# Pollutant, Fine Particulate Matter [PM2.5] -- Unit, micrograms per cubic metre (micrograms/m3)
# Pollutant, Ozone [O3] -- Unit, parts per billion (ppb)

# Remarks: -999 for missing data. 9999 for invalid data.

## ADDITIONAL INFORMATION ##

# About pollutants: https://www.canada.ca/en/environment-climate-change/services/air-pollution/pollutants.html
# About Air Quality Health Index (AQHI): http://www.airqualityontario.com/science/aqhi_description.php

library("tidyverse")
library("lubridate")

## Function to calculate the AQHI 
calculate.aqhi <- function(df) {

  result.aqhi <- c()
  
  for (i in 1:nrow(df)) {
    avg.3h <- c(NA,NA,NA)
    names(avg.3h) <- c("NO2","PM25","O3")
    aqhi.current <- NA
    
    ## -- If iteration index is greater than 2 (to avoid wrong indices)
    if (i > 2) {
      # Step 1 --> get the average of the current time and the previous 2 (3 preceding hours)
      # Avg NO2 -- Avg PM25 -- Avg O3
      last.3h.NO2 <- c(pollutants.dash.df$NO2[(i - 2):i])
      last.3h.PM25 <- c(pollutants.dash.df$PM25[(i - 2):i])
      last.3h.O3 <- c(pollutants.dash.df$O3[(i - 2):i])

      if(sum(is.na(last.3h.NO2)) < 2 & sum(is.na(last.3h.PM25)) < 2 & sum(is.na(last.3h.O3)) < 2) {
        avg.3h["NO2"] <- mean(last.3h.NO2, na.rm = TRUE)
        avg.3h["PM25"] <- mean(last.3h.PM25, na.rm = TRUE)
        avg.3h["O3"] <- mean(last.3h.O3, na.rm = TRUE)
      }

      # Step 2 (if 3 valid averages!!) --> Calculate current AQHI
      if(!anyNA(avg.3h)) {
        aqhi.current <- (1000 / 10.4) * ((exp(0.000871 * avg.3h["NO2"]) - 1) +
                                        (exp(0.000487 * avg.3h["PM25"]) - 1) +
                                        (exp(0.000537 * avg.3h["O3"]) - 1))
        if (aqhi.current <= 0.5) {
          aqhi.current <- 1
        } else {
          aqhi.current <- round(aqhi.current)
        }
      }
    
    } ## End if (i > 2)
    
    # Step 3 --> Add result to a result vector...
    result.aqhi <- c(result.aqhi, as.integer(aqhi.current))
  }
  
  return (result.aqhi)
}

## Create dataframe from existing csv file.
pollutants.dash.df <- read_csv("./WorkingData/pollutants_31103.csv")
glimpse(pollutants.dash.df)

pollutants.dash.df$Station_ID <- as.factor(pollutants.dash.df$Station_ID)

## Convert all 9999 & -999 to NA for plotting and stats purposes
pollutants.dash.df$NO2[pollutants.dash.df$NO2 == 9999 | pollutants.dash.df$NO2 == -999] <- NA
pollutants.dash.df$PM25[pollutants.dash.df$PM25 == 9999 | pollutants.dash.df$PM25 == -999] <- NA
pollutants.dash.df$O3[pollutants.dash.df$O3 == 9999 | pollutants.dash.df$O3 == -999] <- NA

## Work with levels, instead of absolute values (i.e. Low, Medium, High, Extreme)
## Models would predict High and Extreme values, severity rather than numeric values!

### Create severity column for each pollutant, based on percentile criteria (any other criteria?)
#  Ranges -------> [0%-40%: low -- 41%-75%: medium -- 76%-98%: high -- 99%-100%: Very high]
pollutants.dash.df$NO2.level <- cut(pollutants.dash.df$NO2, 
                                    c(0, quantile(pollutants.dash.df$NO2,c(0.4,0.75,0.99),na.rm = TRUE), Inf),
                                    include.lowest = TRUE,
                                    labels = c("Low","Medium","High","Very High"))

pollutants.dash.df$PM25.level <- cut(pollutants.dash.df$PM25, 
                                    c(0, quantile(pollutants.dash.df$PM25,c(0.4,0.75,0.99),na.rm = TRUE), Inf),
                                    include.lowest = TRUE,
                                    labels = c("Low","Medium","High","Very High"))

pollutants.dash.df$O3.level <- cut(pollutants.dash.df$O3, 
                                   c(0, quantile(pollutants.dash.df$O3,c(0.4,0.75,0.99),na.rm = TRUE), Inf),
                                   include.lowest = TRUE,
                                   labels = c("Low","Medium","High","Very High"))

## Calculate the AQHI column for every row with function
pollutants.dash.df$aqhi <- calculate.aqhi(pollutants.dash.df)

## Separate Day and Hour in two different columns for filtering in the Shiny app
pollutants.dash.df <- pollutants.dash.df %>% separate(DateTime, c("Date", "Hour"), sep = " ")
pollutants.dash.df$Date <- as_datetime(pollutants.dash.df$Date, format = "%F")          # %F = %Y-%m-%d
pollutants.dash.df$Hour <- hour(as_datetime(pollutants.dash.df$Hour, format = "%T"))    # %T = %H:%M:%S

## Generate a new csv file to create a dashboard in the next script!!
write.csv(pollutants.dash.df,"./WorkingData/pollutants_31103_dashboard.csv",row.names = FALSE)

#### Get insights about the air quality in Toronto Downtown according to calculated AQHI
## 1 to 3: Low Risk; 4 to 6: Moderate Risk; 7 to 10: High Risk; 10+ : Very High Risk
pollutants.risk.df <- pollutants.dash.df[-which(is.na(pollutants.dash.df$aqhi)),c(1,2,10)]

pollutants.risk.df$aqhi <- cut(pollutants.risk.df$aqhi,
                                     c(1,4,7,Inf), include.lowest = TRUE,
                                     labels = c("Low","Medium","High"),
                                     right = FALSE, ordered_result = TRUE)

# As I am going to determine evolution of aqhi level of risk monthly, get the month of each measure:
pollutants.risk.df$month <- month(pollutants.risk.df$Date)

# Get the count for each month... This is akward, although correct...
summary(pollutants.risk.df[which(pollutants.risk.df$month == 1),]$aqhi)

# More elegant way to do the previous line...
pollutants.monthly.risk.df <- pollutants.risk.df[,-1] %>% group_by(month,aqhi) %>% 
  summarise(total = n()) %>% complete(month, aqhi, fill = list(total = 0)) %>%
  mutate(freq = round(total / sum(total) * 100, 2))

## Create the plots for the results of the aqhi!!!
riskPalette <- c("#130f40","#535c68","#95afc0")

pollutants.monthly.risk.df %>% ggplot(aes(fill=aqhi, y=freq, x=factor(month))) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = riskPalette) +
  ylab("Frequency of AQHI severity (%)") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

pollutants.monthly.risk.df %>% ggplot(aes(fill=aqhi, y=freq, x=factor(month,labels = month.abb))) + 
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) + 
  scale_fill_manual(values = riskPalette) +
  ylab("Frequency of AQHI severity (%)") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
