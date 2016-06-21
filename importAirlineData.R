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

readFlights = function(fileName, airports) {
  flightData = read_csv(fileName)
  
  # -- Filter just the data from the interesting airports
  # (and get rid of irrelevant columns)
  flightData = flightData %>% 
    filter(Origin %in% airports | Dest %in% airports) %>% 
    select(Year, Quarter, Month, 
           DayofMonth, DayOfWeek, FlightDate,
           UniqueCarrier, Origin, OriginCityName, OriginState,
           Dest, DestCityName, DestState,
           CRSDepTime, DepTime, DepDelay, DepDel15,
           TaxiOut, WheelsOff, WheelsOn, TaxiIn,
           CRSArrTime, ArrTime, ArrDelay, ArrDel15,
           CRSElapsedTime, ActualElapsedTime,
           Cancelled, CancellationCode, Diverted, AirTime,
           Flights, Distance, CarrierDelay, WeatherDelay,
           NASDelay, SecurityDelay, LateAircraftDelay)
}

# Read / filter the data from all the files
flightList = lapply(flightFiles, function(x) readFlights(x, airports = airportCodes))

# merge all the data together
# flights = bind_rows(flightList) # dplyr thinks my data frames are corrupt?! :(
flights = do.call(rbind, flightList)


# Basic descriptive stats on data -----------------------------------------
departures = flights %>% 
  filter(Origin %in% airportCodes)

arrivals = flights %>% 
  filter(Dest %in% airportCodes) %>% 
  group_by(Dest, Year) %>% 
  summarise(num = n(), dist = mean(Distance), 
            div = mean(Diverted), 
            cancelled = mean(Cancelled),
            flightTime = mean(ActualElapsedTime),
            airtime = mean(AirTime, na.rm = TRUE))


# data exploration to check vars ------------------------------------------
# -- Create a small subset of the data --
subset = flights %>% slice(1:1000)

ggplot(subset, aes(x = CRSElapsedTime, y = ActualElapsedTime)) +
         geom_point(size = 3, alpha = 0.2) +
         theme_bw()
# Okay... so CRS != actual.

ggplot(subset, aes(x = AirTime, y = ActualElapsedTime)) +
  geom_point(size = 3, alpha = 0.2) +
  theme_bw() +
  coord_equal()
# And similarly, airtime != elapsed time.  Assuming air time = wheels up - wheels down (excluding taxiing times)