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
# flights = bind_rows(flightList) # dplyr thinks my data frames are corrupt?! :(
flights = do.call(rbind, flightList) %>% 
  mutate(yr_month = zoo::as.yearmon(Year, Month)) %>% 
  rename(year = Year, month = Month, quarter = Quarter, 
         origin = Origin, dest = Dest, depTime = DepTime, arrTime = ArrTime)

# Pull out all the data from flights NOT landing in the DC area as the refernce set
flights_ref = flights_dc = flights %>% 
  filter(!Origin %in% airports,
         !Dest %in% airports)

# Create the set of flights just in the DC area  
flights_dc = flights %>% 
  filter(Origin %in% airports | Dest %in% airports)

# Basic descriptive stats on data -----------------------------------------


# Collapse by year --------------------------------------------------------
# -- All flights --
dep_all_year = flights_ref %>% 
  filter(origin %in% airportCodes) %>% 
  group_by(origin, year) %>% 
  summarise(num = n())

arrivals = flights %>% 
  filter(Dest %in% airportCodes) %>% 
  group_by(Dest, yr_month) %>% 
  summarise(num = n(), dist = mean(Distance), 
            div = mean(Diverted), 
            cancelled = mean(Cancelled),
            flightTime = mean(ActualElapsedTime),
            airtime = mean(AirTime, na.rm = TRUE))

arrivals_date = flights %>% 
  filter(Dest %in% airportCodes) %>% 
  group_by(Dest, FlightDate, Year) %>% 
  summarise(num = n(), dist = mean(Distance), 
            div = mean(Diverted), 
            cancelled = mean(Cancelled),
            flightTime = mean(ActualElapsedTime),
            airtime = mean(AirTime, na.rm = TRUE))

departures_date = flights %>% 
  filter(Origin %in% airportCodes,
         Month %in% c(11:12)) %>% 
  group_by(Origin, FlightDate, Year) %>% 
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

