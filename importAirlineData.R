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
library(lubridate)

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
         arrTime = ArrTime, date = FlightDate, 
         dayOfMonth = DayofMonth, dayOfWeek = DayOfWeek,
         carrier = UniqueCarrier)

# Create the set of flights just in the DC area  
flights_dc = flights %>% 
  filter(Origin %in% airportCodes | Dest %in% airportCodes) %>% 
  rename(year = Year, month = Month, quarter = Quarter, 
         origin = Origin, dest = Dest, depTime = DepTime, 
         arrTime = ArrTime, date = FlightDate, 
         dayOfMonth = DayofMonth, dayOfWeek = DayOfWeek,
         carrier = UniqueCarrier)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Basic descriptive stats on data -----------------------------------------

# collapse function to sum over timeframe ---------------------------------

collapse = function(df, timeVar) {
  
  # explode out timeVar to component parts:
  if(!timeVar %in% colnames(df)) {
    stop('grouping variable timeVar is not within the inputted data frame')
  } else if(timeVar == 'year'){
    var2 = var3 = var4 = 'year'
  } else if(timeVar == 'yr_month'){
    var2 = 'year'
    var3 = var4 = 'month'
  } else if(timeVar == 'date'){
    var2 = 'year'
    var3 = 'month'
    var4 = 'dayOfWeek'
  } else if(timeVar == 'dayOfWeek'){
    var2 = 'year'
    var3 = 'month'
    var4 = 'yr_month'
  }

  summed_flights = df %>%
    mutate(airport = ifelse(dest == 'DCA' | origin == 'DCA', 'DCA',
                            ifelse(dest == 'IAD' | origin == 'IAD', 'IAD',
                                   ifelse(dest == 'BWI' | origin == 'BWI', 'BWI', NA)))) %>% 
    group_by_(var2, var3, var4, timeVar, 'airport') %>% 
    summarise(num = n())
  
  # Antiquated-- used for separately summing arrivals and departures.
  # Group by the time variable and sum the number of flights
# departures = df %>% 
#   filter(origin %in% airportCodes) %>% 
#   group_by_('origin', var2, var3, var4, timeVar) %>% 
#   summarise(num = n())
# 
# arrivals = df %>% 
#   filter(dest %in% airportCodes) %>% 
#   group_by_('dest', var2, var3, var4, timeVar) %>% 
#   summarise(num = n())

# return(list(arrivals = arrivals, departures = departures))
  return(summed_flights)
}

# Collapse by year --------------------------------------------------------
# -- DC data --
dc_by_year = collapse(flights_dc, 
                      timeVar = 'year')


# All non-DC data
all_by_year = flights_ref %>% 
  group_by(year) %>% 
  summarise(num = n())

# Collapse by month -------------------------------------------------------
dc_by_month = collapse(flights_dc, 
                      timeVar = 'yr_month') %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', '01')))


# All non-DC data
all_by_month = flights_ref %>% 
  group_by(yr_month, year, month) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', '01')))

# Collapse by date --------------------------------------------------------

dc_by_date = collapse(flights_dc, 
                       timeVar = 'date') %>% 
  ungroup()


# All non-DC data
all_by_date = flights_ref %>% 
  group_by(year, month, date, dayOfWeek) %>% 
  summarise(num = n()) %>% 
  ungroup()


# Collapse by day of week --------------------------------------------------------

dc_by_day = collapse(flights_dc, 
                      timeVar = 'dayOfWeek') %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', dayOfWeek)))


# All non-DC data
all_by_day = flights_ref %>% 
  group_by(yr_month, year, month, dayOfWeek) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', dayOfWeek)))

# Collapse by date and airline --------------------------------------------
dc_date_airline = flights_dc %>% 
  mutate(airport = ifelse(dest == 'DCA' | origin == 'DCA', 'DCA',
                          ifelse(dest == 'IAD' | origin == 'IAD', 'IAD',
                                 ifelse(dest == 'BWI' | origin == 'BWI', 'BWI', NA)))) %>% 
  group_by(year, month, date, dayOfWeek, 
           carrier, airport) %>% 
  summarise(num = n()) %>% 
  ungroup()

# All non-DC data
all_date_airline = flights_ref %>% 
  group_by(year, month, date, dayOfWeek, carrier) %>% 
  summarise(num = n()) %>% 
  ungroup()


# Merge in carrier data ---------------------------------------------------
all_date_airline = left_join(all_date_airline, 
                             carriers,
                             by = c("carrier" = "Code")) %>% 
  rename(carrierName = Description) %>% 
  mutate(carrierName = ifelse(carrier %in% c('US', 'HP'), 
                              'American Airlines Inc.', carrierName)) # Convert U.S. Airways and America West to be American Airlines, since they merged

# Check and see if merged properly
all_date_airline %>% 
  filter(is.na(carrierName)) %>% 
  group_by(carrier) %>% 
  summarise(n())

dc_date_airline = left_join(dc_date_airline, 
                             carriers,
                             by = c("carrier" = "Code")) %>% 
  rename(carrierName = Description) %>% 
  mutate(carrierName = ifelse(carrier %in% c('US', 'HP'), 
                              'American Airlines Inc.', carrierName)) # Convert U.S. Airways and America West to be American Airlines, since they merged


# Check and see if merged properly
dc_date_airline %>% 
  filter(is.na(carrierName)) %>% 
  group_by(carrier) %>% 
  summarise(n())

# Only carrier without a name is code "KH", which doesn't operate in the DC area.
# Ignoring for now.



# Export relevant data ----------------------------------------------------
# rm(flights_dc, flights_ref)

# save.image(file = 'data_out/collapsed_2016-06-22.RData')

save(dc_date_airline, dc_by_year, dc_by_month, dc_by_date, dc_by_day,
     all_date_airline, all_by_date, all_by_month, all_by_year,
     airports,
     file = 'data_out/collapsed_byAirline_2016-06-22.RData')
