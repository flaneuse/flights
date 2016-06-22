# setup libraries, global variables ---------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(readr)
library(zoo)
library(data.table)
library(seasonal)

# further cleanup ---------------------------------------------------------
# mutations to create faux dates + add tag for whether arrivals or departures

arrivals_day = arrivals_day %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', dayOfWeek)))

dep_day = dep_day %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', dayOfWeek)))


# combine data together to get all traffic --------------------------------
# -- year --
dc_year = 3

# -- month --  
all_by_month = all_by_month %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', '01')))

arrivals_month = arrivals_month %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', '01')),
         flightType = 'arrivals') %>% 
  rename(airport = dest)

dep_month = dep_month %>% 
  ungroup() %>% 
  mutate(yr_month = ymd(paste0(year, '-', month, '-', '01')),
         flightType = 'departures') %>% 
  rename(airport = origin)

# Merge arrivals and departures
dc_month = rbind(arrivals_month, dep_month) %>% 
  group_by(yr_month, year, month, airport) %>% 
  summarise(total = sum(num))

# Basic exploratory plots -------------------------------------------------
# -- year --
ggplot(all_by_year, aes(x = year, y = num)) +
  geom_line() +
  theme_bw()

ggplot(dep_year, aes(x = year, y = num,
                     group = origin, colour = origin)) +
  geom_line() +
  theme_bw()


ggplot(arrivals_year, aes(x = year, y = num,
                          group = dest, colour = dest)) +
  geom_line() +
  theme_bw()

# -- month --
ggplot(all_by_month, aes(x = yr_month, y = num)) +
  geom_line() +
  theme_bw()

ggplot(arrivals_month, aes(x = yr_month, y = num,
                           group = dest, colour = dest)) +
  geom_line() +
  theme_bw()

# -- date --
ggplot(arrivals_date, aes(x = date, y = num,
                          group = dest, colour = dest)) +
  geom_line() +
  theme_bw()

ggplot(dep_date, aes(x = date, y = num,
                     group = origin, colour = origin)) +
  geom_line() +
  theme_bw()

# -- day of week, over year --
ggplot(arrivals_day, aes(x = yr_month, y = num,
                         group = dest, colour = dest)) +
  geom_line() +
  theme_bw()

ggplot(dep_day, aes(x = yr_month, y = num,
                    group = dest, colour = dest)) +
  geom_line() +
  theme_bw()

