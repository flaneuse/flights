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


# Basic exploratory plots -------------------------------------------------
# -- dc over the entire period --

ggplot(dc_by_month, aes(x = yr_month, y = num,
                        group = airport, colour = airport)) +
  geom_line() +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 'Reagan' = dcaColour)) +
  theme_xygridlight() +
  theme(axis.title = element_blank(),
        title = element_text(size = 15)) +
  ggtitle('Total flights per month at both Reagan and Dulles have decreased') +
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
