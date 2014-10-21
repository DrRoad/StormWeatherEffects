---
title: "Storm Weather Effects"
author: "Alex Pittendreigh"
date: "October 2014"
output: html_document
---

## Synopsis ##

In this report I aim to explore the National Oceanic and Atmospheric Administrations (NOAA) storm database from 17 August 2007 to answer questions as to which types of events across the United States are the most harmful with respect to the health of the population and which types of events have the greatest economic consequences. 

To undertake this exploration data was obtained from NOAA's National Weather Service which can be obtained at [https://d396qusza40orc.cloudfront.net/](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) as at 17 October 2014. The data is in a comma delimited file archived in bz2 format. Documentation for the data set can also be downloaded from [https://d396qusza40orc.cloudfront.net/](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

## Loading and Processing Raw Data ##

All persistent data is located in the data subdirectory of the current working documents directory. The main raw data file is called repdata-data-StormData.csv and contains 37 variables and 902297 observations. Observation dates range from 18th April 1950 to 28th November 2011. A check is made in the data subdirectory for the .csv file and if needed, and attempt is made to download and extract the file from the above location before processing.

```{r}
if (! file.exists('data/repdata-data-StormData.csv.bz2')) {
    fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
    
    download.file(fileUrl,
                  dest = 'data/repdata-data-StormData.csv.bz2',
                  method = 'curl',
                  quiet = TRUE)
}
```

### Reading in the data ###

Before processing the data several libraries in R need to be loaded after which point we can then load the data directly from the archive file. Initially all data from 1950 to 2011 will be read in.

```{r}
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(car))

data <- read.csv('data/repdata-data-StormData.csv.bz2')
head(data)
```

The data is in a state that will need to be maipulated and cleaned up before undertaking our analysis, so lets start with the headers.

```{r}
data.titles <- c('state.__',              # ????
                 'begin.date',            # Event Beginning Date
                 'begin.time',            # Event Beginning Time
                 'time.zone',             # Event Time Zone
                 'county',                # Begin Event County Code
                 'county.name',           # Begin Event County Name
                 'state',                 # Begin Event State
                 'evtype',                # Event Type
                 'begin.range',           # Beginning Range
                 'begin.azi',             # Beginning Azimuth
                 'begin.location',        # Beginning Location
                 'end.date',              # Event Ending Date
                 'end.time',              # Event Ending Time
                 'county.end',            # End Event County Code
                 'county.end.name',       # End Event County Name
                 'end.range',             # Ending Range
                 'end.azi',               # Ending Azimuth
                 'end.location',          # Ending Location
                 'length',                # Event Length
                 'width',                 # Event Width
                 'force',                 # Wind Force of Event
                 'mag',                   # Event Magnitude
                 'fatalities',            # Number of Event Fatalities
                 'injuries',              # Number of Event Injuries
                 'prop.damage',           # Cost of Property Damage by Event
                 'prop.damage.exp',       # Property Damage Cost Category
                 'crop.damage',           # Cost of Crop Damage by Event
                 'crop.damage.exp',       # Crop Damage Cost Category
                 'wfo',                   # Weather Forecasting Office??
                 'state.office',          # State Office Name
                 'zone.names',            # Zone Names
                 'latitude',              # Beginning Latitude
                 'longitude',             # Beginning Longitude
                 'latitude.end',          # Ending Latitute
                 'longitude.end',         # Ending Longitude
                 'remarks',               # Event Remarks
                 'ref.num')               # Reference Number

colnames(data) <- data.titles
data <- tbl_df(data)                      # Make this easier to use for manipulation
data
```

