# Exploring passenger airline dataset ----------------------------------------
# Question: is Ronald Reagan National Airport stealing domestic passengers from
# other area airports?

# Laura Hughes, laura.d.hughes@gmail.com
# 19 June 2016
# (c) 2016 via MIT License


# setup libraries, global variables ---------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(readr)
library(zoo)
library(data.table)
library(seasonal)

# Focus data on the airports in the Washington DC Metro area.
airportCodes = c('DCA', 'IAD', 'BWI')

# import data -------------------------------------------------------------
# -- Airport location data, provided by Spotify --
airports = read_excel('data/airports new.xls')

# -- Carrier codes, provided by Spotify --
carriers = read_excel('data/carriers.xls')

# -- Individual Flight On-Time Arrivals --
# Flight on-time data from the U.S. Bureau of Transportation Statistics
# http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# Downloaded 20 June 2016

# All individual file names w/ flight data (ea. year, month)
flightFiles = list.files('data/', pattern = '*.csv', full.names =  TRUE)

readFlights = function(fileName) {
  # Manually specify column types, to deal with import error for Carrier/UniqueCarrier
  # read_csv gets confused if the carrier name starts w/ a number.
  flightData = read_csv(fileName, col_types = paste0('iiiiiDcdc_____ccc_c____ccc_c_cc_________cc', paste0(rep('_', 68), collapse='')))
  
  # Get rid of irrelevant columns-- should be taken care of in import, but double checking.
  
  flightData = flightData %>% 
    select(Year, Quarter, Month, 
           DayofMonth, DayOfWeek, FlightDate,
           UniqueCarrier, Origin,
           Dest, CRSDepTime, DepTime, CRSArrTime, ArrTime)
}


# Read / filter the data for all airports from all the files
flightList = lapply(flightFiles, function(x) readFlights(x))

# merge all the data together
flights = bind_rows(flightList) %>% 
  # do.call(rbind, flightList) %>%
  mutate(yr_month = zoo::as.yearmon(Year, Month)) 

# Pull out all the data from flights NOT landing in the DC area as the refernce set
flights_ref = flights %>% 
  filter(!Origin %in% airportCodes,
         !Dest %in% airportCodes) %>% 
  rename(year = Year, month = Month, quarter = Quarter, 
         origin = Origin, dest = Dest, depTime = DepTime, 
         arrTime = ArrTime, date = FlightDate)

# Create the set of flights just in the DC area  
flights_dc = flights %>% 
  filter(Origin %in% airportCodes | Dest %in% airportCodes) %>% 
  rename(year = Year, month = Month, quarter = Quarter, 
         origin = Origin, dest = Dest, depTime = DepTime, 
         arrTime = ArrTime, date = FlightDate)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Basic descriptive stats on data -----------------------------------------

# collapse function to sum over timeframe ---------------------------------

collapse = function(df, 
                    timeVar = 'year',
                    airports = airportCodes) {
  
  # explode out timeVar to component parts:
  timeVar = if(!timeVar %in% colnames(df)) {
    stop('grouping variable timeVar is not within the inputted data frame')
  } else if(timeVar == 'year'){
    var2 = var3 = 'year'
  } else if(timeVar == 'month'){
    var2 = var3 = 'year'
  } else if(timeVar == 'FlightDate'){
    var2 = 'year'
    var3 = 'month'
  }

# Group by the time variable and sum the number of flights
departures = df %>% 
  filter(origin %in% airportCodes) %>% 
  group_by_('origin', var2, var3, timeVar) %>% 
  summarise(num = n())

arrivals = df %>% 
  filter(dest %in% airportCodes) %>% 
  group_by_('dest', var2, var3, timeVar) %>% 
  summarise(num = n())

return(list(arrivals = arrivals, departures = departures))
}

# Collapse by year --------------------------------------------------------
# -- DC data --
dc_by_year = collapse(flights_dc, 
                      timeVar = 'year')

dc_arrivals_year = dc_by_year$arrivals
dc_dep_year = dc_by_year$departures

# All non-DC data
all_by_year = flights_all %>% 
  group_by(year) %>% 
  summarise(num = n())

# Collapse by month -------------------------------------------------------
dc_by_month = collapse(flights_dc, 
                      timeVar = 'month')

# All non-DC data
all_by_month = flights_all %>% 
  group_by(year, month) %>% 
  summarise(num = n())

# Collapse by date --------------------------------------------------------

dc_by_date = collapse(flights_dc, 
                       timeVar = 'FlightDate')

# All non-DC data
all_by_date = flights_all %>% 
  group_by(year, month, FlightDate) %>% 
  summarise(num = n())


# data exploration to check vars ------------------------------------------
# -- Create a small subset of the data --
subset = flights %>% slice(1:1000)


# Simple month/yearly trends ----------------------------------------------------
ggplot(arrivals, aes(x = yr_month, y = num, 
                     colour = Dest, group = Dest)) +
  geom_line() +
  theme_bw()

ggplot(departures, aes(x = yr_month, y = num, 
                       colour = Origin, group = Origin)) +
  geom_line() +
  theme_bw()


ggplot(arrivals_date %>%  filter(Year == 2015), aes(x = FlightDate, y = num,
                                                    colour = Dest, group = Dest)) + 
  geom_line() +
  theme_bw()

ggplot(departures_date %>%  filter(Year == 2015), aes(x = FlightDate, y = num,
                                                      colour = Origin, group = Origin)) + 
  geom_line() +
  theme_bw()

