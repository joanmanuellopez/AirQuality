Air Quality in Toronto Downtown
================
Joan M Lopez
September 2018

Project Background
------------------

Air quality is a major concern of cities nowadays, since it's well known that air pollution is an important cause of illness for people living there. In this project I would like to explore this interesting issue just analysing the information available in the [air quality open data portal](http://www.airqualityontario.com/history/index.php) of the Ontario's Ministry of the Environment, Conservation and Parks, in Canada. From this portal, I'm going to use the information for one station (named Toronto Downtown) in a single year (2012); of course, this analysis could be extended to more stations and more years.

#### The Air Quality Health Index (AQHI)

This index is used by the Canadian Government to measure the level of risk for population due to bad quality air exposure, and is calculated based on the relative risks of a combination of common air pollutants that is known to harm human health:

-   *Nitrogen Dioxide (NO2), Particulate Matters (PM2.5) and Ground Ozone (O3)*

You can learn more about this indicator in the [Canadian Government website](https://www.canada.ca/en/environment-climate-change/services/air-quality-health-index/about.html), as well as check the [current observations and forecasts](http://www.airqualityontario.com/aqhi/) of air quality for the province of Ontario.

### Business question to be answered

-   Is the air quality nearby the Toronto Downtown station safe enough for people's health?

### Source of data

All the datasets used for this project can be found on the airqualityontario website, following this [link](http://www.airqualityontario.com/history/index.php). Just choose a station, a pollutant and a period of time and you'll be ready to download your dataset in XML, CSV file or HTML.

My selection are three CSV files comprising the period starting in January 2012 and ending in December 2012, for the Toronto Downtown station, one for each one of the pollutants mentioned above: Nitrogen Dioxide (NO2), Particulate Matters (PM2.5) and Ground Ozone (O3).

I would like to remark the fact that some previous work had to be done on the original CSV files before importing them into RStudio. You can find the cleaned datasets, ready to be used for the analysis, in the repository for this project.

Main insights
-------------

#### Regarding Missing Data and Invalid Data (all together, considered as NAs)

-   Missing and Invalid Data for each pollutant:
    -   0.96% for NO2; 1.81% for PM2.5; 0.26% for O3
-   NO2 and PM2.5 measures have the same missing data.
-   3 out of 4 invalid data for NO2 are produced at the same time and with the same duration than for PM2.5
    -   *The latter bullet-points suggest a sort of connection between the data sources of NO2 and PM2.5*
-   Regular difference of 366 hours (15.25 days) between missing data for O3 measurements, as well as for invalid data, and never more than one missing value when it's produced. This is such a weird behaviour for this sensor, an astonishing precision through time. If this would be continous through time, I would say there is a buffer storing information until it's full, and the process of sending information and emptying the buffer takes between one and two hours.

<!-- #### Regarding Levels of Pollutants -->
<!-- * About Dioxide Nitrogen -->
<!-- * About Particulate Matter -->
<!-- * About Ground Ozone (senoide inside another senoide, with a period of 15 days between peaks) -->
#### Regarding Air Quality Health Index (AQHI)

I [calculate the AQHI](https://en.wikipedia.org/wiki/Air_Quality_Health_Index_(Canada)#Calculation) according to the formula determined by the Canadian Government, and later I categorize the results in 3 levels, given the Canadian criteria and taking into account that the maximum level for that year is 7: *Low Risk*, *Medium Risk* and *High Risk*.

For the Toronto Downtown station in 2012, I observe:

-   The period between June and November, but October, there are the most hours with medium and high risk levels for health
-   The period between January and April is when the air quality levels are better for people's health

#### Next steps: Ideas for further analysis

-   Evolution through time, considering data of more than one year
-   Compare AQHI and pollutants levels between different stations
-   How weather conditions such as rain, temperature, wind or atmospheric pressure affect air quality
-   How different conditions of sources of pollution affect air quality *Traffic road conditions...*
-   Air quality performance according dayweek and/or hour

Scripts description
-------------------

#### 01\_air\_preprocess - Import and tidy datasets

Transform the originals datasets provided in three different csv files (one for each pollutant) in wide-format to a single dataset in long-format so that each row has information about measures at a given hour (i.e. 1st January at 1am) for each pollutant.

#### 02\_air\_nas - Analyze missing values and invalid measures

Get deeper knowledge about those abnormal values in the datasets and understand their nature.

#### 03\_air\_viz - First exploratory analysis and some visualizations

Create plots to visualize the evolution of the pollutants levels through time. Play with granularity to get better insights with less observations.

#### 04\_air\_dashboard - Generate dataset to be used in a dashboard

Prepare proper dataset to create a dashboard. This script just takes the joint dataset and transforms data to be used in the visualizations for final users through the application. In this script there is a function that calculates the AQHI given the levels of the three pollutants.

#### 05\_air\_shinyapp - The dashboard

Last but not least, a Shiny app to create an interactive, rich visualization. It shows current values as well as the evolution through time of the three pollutants.
