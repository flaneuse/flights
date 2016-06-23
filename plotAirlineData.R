
# Create plots on airline data --------------------------------------------

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
library(llamar)
library(extrafont)

loadfonts()

# load data ---------------------------------------------------------------
load("~/GitHub/flights/data_out/collapsed_byAirline_2016-06-22.RData")


# global params -----------------------------------------------------------
dcaColour = '#a65628'
iadColour = '#377eb8'
bwiColour = '#984ea3'

# -- Sizes --
widthPlot = 6
heightPlot = 3

# refactorizing -----------------------------------------------------------
dc_by_month$airport = factor(dc_by_month$airport,
                             levels = c('DCA', 'IAD', 'BWI'),
                             labels = c('Reagan', 'Dulles', 'BWI'))

dc_by_date$airport = factor(dc_by_date$airport,
                             levels = c('DCA', 'IAD', 'BWI'),
                             labels = c('Reagan', 'Dulles', 'BWI'))


# Basic exploratory plots -------------------------------------------------


# 01- DC over entire period -----------------------------------------------
by_month = dc_by_month %>% filter(airport != 'BWI') %>% 
  spread(airport, num)

ggplot(by_month, aes(x = yr_month, y = num,
                        group = airport, colour = airport)) +
  geom_line() +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 'Reagan' = dcaColour)) +
  theme_xygridlight() +
  theme(axis.title = element_blank(),
        title = element_text(size = 13)) +
  geom_point(size = 2, data = by_month %>% 
               filter(month == 10 & year == 2004 | 
                        month == 3 & year == 2005 | 
                        month == 4 & year == 2016)) +
  geom_text(aes(label = num), 
            family = 'Segoe UI Semilight', size = 4.5,
            hjust = 0,
            data = by_month %>% 
              filter(month == 10 & year == 2004 | 
                       month == 3 & year == 2005 | 
                       month == 4 & year == 2016)) +
  ggtitle('Total flights per month at Reagan and Dulles have decreased since 2005 but more at Dulles') +
  facet_wrap(~airport)

ggsave('pdf/01_uncorrected_totalByMonth.pdf', 
       width = widthPlot,
       height = heightPlot,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# Exploratory: zoom in on variation ---------------------------------------
ggplot(dc_by_date %>% filter(year > 2014), aes(x = date, y = num,
                     group = airport, colour = airport)) +
  geom_line() +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 'Reagan' = dcaColour)) +
  theme_xygridlight()

ggplot(dc_by_month %>% filter(year > 2013), aes(x = yr_month, y = num,
                                               group = airport, colour = airport)) +
  geom_line() +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 'Reagan' = dcaColour)) +
  theme_xygridlight()


# Selecting a period as a reference ---------------------------------------
# Flights in 2004-2005 were maximal; trying to find a reasonable base year to 
# compare the most recent data to.
# 2005 seems like a reasonable comparison; outside the wacky Independence Air spike
# and before things get really sour for Dulles.
ggplot(dc_by_month %>% filter(year > 2003, year < 2010, airport != 'BWI'), aes(x = yr_month, y = num,
                                                group = airport, colour = airport)) +
  geom_line() +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 'Reagan' = dcaColour)) +
  theme_xygridlight()


# Which airlines are responsible for change in DCA traffic? ---------------

pctComp = dc_date_airline %>% 
  filter(airport != 'BWI', 
         year %in% c(2005, 2015)) %>% 
  group_by(year, airport, carrierName) %>% 
  summarise(total = sum(num)) %>% 
  spread(airport, total) %>% 
  mutate(DCA = ifelse(is.na(DCA), 0, as.numeric(DCA)), # fix NAs
         IAD = ifelse(is.na(IAD), 0, as.numeric(IAD)),
         pctDCA = DCA / (DCA + IAD)) %>%  # % of flights in area out of DCA
  ungroup() %>% 
  group_by(carrierName)

chgComp = pctComp %>% 
  select(year, carrierName, pctDCA) %>% 
  spread(year, pctDCA) %>% 
  mutate(diffDCA = `2015` - `2005`) %>% 
  ungroup() %>% 
  arrange((diffDCA))

# Merge back in the total number of flights per year
chgComp  = left_join(chgComp, pctComp %>% filter(year == 2015)) %>% 
  rowwise() %>% 
  mutate(total_DCA_IAD = sum(DCA, IAD, na.rm = TRUE))

chgComp$carrierName = factor(chgComp$carrierName, levels = chgComp$carrierName)

ggplot(chgComp, aes(y = carrierName, 
                    x = diffDCA,
                    size = total_DCA_IAD)) +
  geom_point()