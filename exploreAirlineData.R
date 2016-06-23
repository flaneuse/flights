# setup libraries, global variables ---------------------------------------
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(readr)
library(zoo)
library(data.table)
library(seasonal)



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

