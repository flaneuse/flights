# Exploring passenger airline dataset ----------------------------------------

# Laura Hughes, laura.d.hughes@gmail.com
# 19 June 2016
# (c) 2016 via MIT License


# import data -------------------------------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

# -- Individual Flight On-Time Arrivals --
# Flight on-time data from the U.S. Bureau of Transportation Statistics
# http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# Downloaded 19 June 2016
flights = read.csv('data/On_Time_On_Time_Performance_2016_1.csv')

# -- Airport location data, provided by Spotify --
airports = read_excel('data/airports new.xls')

# -- Carrier codes, provided by Spotify --
carriers = read_excel('data/carriers.xls')
