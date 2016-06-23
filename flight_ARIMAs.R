# adjusting for seasonality with ARIMAs -----------------------------------------------------

# Laura Hughes, laura.d.hughes@gmail.com
# 19 June 2016
# (c) 2016 via MIT License

# Clearly, there's lots of seasonal variation within airline data.  
# This adjusts for monthly or weekly variation naively and uses
# ARIMA models to look at the periodicity of the data.


# Simple, naive assumption ------------------------------------------------
# Any gross changes that happen to the number of flights in DC also happen to
# the rest of the country. For instance, post Sept. 11, all airlines had a significant
# downturn.  DCA was especially affected, due to restrictions.

# Assuming DCA and IAD are like the rest of the country, to account for the baseline
# of changes, we can look at the aberations of those two places by calculating a 
# simple ratio of DCA or IAD to the rest of the country.
# To a first approximation, this should account for typical seasonal and weekly variation.

# Quickly plotting the overall monthly pattern:
ggplot(all_by_month, aes(x = yr_month, y = num)) +
  geom_line(colour = grey75K, size = 0.5) +
  theme_xgrid() +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank())

# Flights have been decreasing since economic downturn in 2008/2009.

# Dividing one by the other!

# Merge in the natl data.
dc_by_date = left_join(dc_by_date, all_by_date,
                       by = c("year" = "year", "month" = "month",
                              "dayOfWeek" = "dayOfWeek", "date" = "date")) %>% 
  rename(natl = num.y, dc = num.x) %>% 
  mutate(ratio = dc / natl)

dc_by_month = left_join(dc_by_month, all_by_month,
                        by = c("year" = "year", "month" = "month",
                               "yr_month" = "yr_month")) %>% 
  rename(natl = num.y, dc = num.x) %>% 
  mutate(ratio = dc / natl)


# Calculating % change relative to 2005 vals ------------------------------
daysPerYr = 365.25

natl2005 = all_by_year %>% 
  filter(year == 2005) %>% 
  mutate(num2005 = num / daysPerYr) %>% 
  ungroup() %>% 
  select(num2005)

natl2005 = natl2005$num2005

# total per year
dc2005 = dc_by_year %>% 
  filter(year == 2005) %>% 
  mutate(num2005 = num / daysPerYr,
         natl = natl2005,
         ratio2005 = num2005/natl
  ) %>% 
  ungroup() %>% 
  select(airport, num2005, ratio2005)



dc2005$airport = factor(dc2005$airport,
                        levels = c('DCA', 'IAD', 'BWI'),
                        labels = c('Reagan', 'Dulles', 'BWI'))


# Merge in the values from 2005
dc_by_date = left_join(dc_by_date, dc2005, by = c("airport" = "airport"))

rateChg = dc_by_date %>% 
  filter(year > 2004) %>% 
  rowwise() %>% 
  mutate(rateNum = (dc - num2005) / num2005,
         rateRatio = (ratio - ratio2005)/ratio2005)

ggplot(rateChg, aes(x = date, y = rateNum, group = airport, colour = airport)) +
  geom_line() +
  theme_xygrid() +
  facet_wrap(~airport)

# Symmetricize the color scale
colLim = max(abs(min(rateChg$rateRatio)), max(rateChg$rateRatio))

ggplot(rateChg, aes(x = date, y = rateRatio, 
                    group = airport, colour = rateRatio)) +
  scale_colour_gradientn(colours = brewer.pal(10, 'RdYlBu'),
                         limits = c(-colLim, colLim)) +
  geom_line(size = 0.25) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('percent change from adjusted 2005 value') +
  theme_xygridlight() +
  facet_wrap(~airport)

ggsave('pdf/05_naive_correction_rate_change.pdf', 
       width = widthPlot,
       height = heightPlot/1.5,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)

ggplot(rateChg %>% filter(year > 2013), aes(x = date, y = rateRatio, 
                    group = airport, colour = airport)) +
  scale_color_manual(values = c('Dulles'= iadColour, 'BWI' = bwiColour, 
                                'Reagan' = dcaColour)) +
  
  geom_line(size = 0.25) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('percent change from adjusted 2005 value') +
  theme_xygridlight()

ggsave('pdf/06_naive_correction_rate_change_2014.pdf', 
       width = widthPlot,
       height = heightPlot*2,
       bg = 'transparent',
       paper = 'special',
       units = 'in',
       useDingbats=FALSE,
       compress = FALSE,
       dpi = 300)


# correlation of ‘corrected’ data -----------------------------------------

post2005 = dc_by_month %>% 
  filter(year > 2004) %>% 
  select(year, month, yr_month, airport, ratio) %>% 
  spread(airport, ratio) %>% 
  mutate(date_dec = decimal_date(yr_month))

post2005 = dc_by_date %>% 
  filter(year > 2004) %>% 
  select(year, month, date, airport, ratio) %>% 
  spread(airport, ratio) %>% 
  mutate(date_dec = decimal_date(date))

uncorrected = dc_by_date %>% 
  filter(year > 2004) %>% 
  select(year, month, date, airport, dc) %>% 
  spread(airport, dc) %>% 
  mutate(date_dec = decimal_date(date))

# -- Simple linear models --
uncorr_model = lm(Reagan ~ Dulles, data = uncorrected)
corr_model = lm(Reagan ~ Dulles, data = post2005)

uncorr_model_2015 = lm(Reagan ~ Dulles, data = uncorrected %>% filter(year>2014))
corr_model_2015 = lm(Reagan ~ Dulles, data = post2005 %>% filter(year>2014))

uncorr_model_2014 = lm(Reagan ~ Dulles, data = uncorrected %>% filter(year>2013))
corr_model_2014 = lm(Reagan ~ Dulles, data = post2005 %>% filter(year>2013))

# Big ole mess
ggplot(data = post2005, 
       aes(x = Reagan, y = Dulles, 
           colour = date_dec)) +
  geom_path(size = 0.25) +
  geom_point(size = 4, alpha = 0.5) +
  geom_text(aes(label = year), colour = '#ff7f00',
            data = post2005 %>% filter(date_dec %in% c(2005, 2016))) + 
  scale_colour_gradientn(colours = brewer.pal(9, 'Blues')[2:9]) +
  theme_xygrid() +
  coord_equal()


ggplot(data = post2005 %>% filter(year > 2013), 
       aes(x = Reagan, y = Dulles, 
           colour = date_dec)) +
  # geom_path(size = 0.25) +
  geom_point(size = 4, alpha = 0.5) +
  geom_text(aes(label = year), colour = '#ff7f00',
            data = post2005 %>% filter(date_dec %in% c(2014, 2016))) + 
  scale_colour_gradientn(colours = brewer.pal(9, 'Blues')[2:9]) +
  theme_xygrid() + 
  coord_equal()

ggplot(data = post2005 %>% filter(year > 2013), 
       aes(x = Reagan, y = Dulles, 
           colour = date_dec)) +
  geom_path(size = 0.25) +
  geom_smooth(method='lm',formula=y~x,
              fill = '#ff7f00', alpha = 0.18,
              colour = '#ff7f00', size = 0.5) + 
  geom_point(size = 4, alpha = 0.5) +
  geom_text(aes(label = year), colour = '#ff7f00',
            data = post2005 %>% filter(date_dec %in% c(2015, 2016))) + 
  scale_colour_gradientn(colours = brewer.pal(9, 'Blues')[2:9]) +
  theme_xygrid() +
  coord_equal()

ggplot(data = uncorrected %>% filter(year > 2013.99), 
       aes(x = Reagan, y = Dulles, 
           colour = date_dec)) +
  # geom_path(size = 0.25) +
  geom_smooth(method='lm',formula=y~x,
              fill = '#ff7f00', alpha = 0.18,
              colour = '#ff7f00', size = 0.5) + 
  geom_point(size = 4, alpha = 0.5) +
  geom_text(aes(label = year), colour = '#ff7f00',
            data = uncorrected %>% filter(date_dec %in% c(2014, 2016))) + 
  scale_colour_gradientn(colours = brewer.pal(9, 'Blues')[2:9]) +
  theme_xygrid() +
  coord_equal()

# Exploring variation in seasonality --------------------------------------


# Create a ts object to input into seasonal::seas
ts_all_month = ts(all_by_month %>% select(num), frequency = 12, start = c(2000, 1))

# Using seasonal package to calculate the seasonally-adjusted patterns.
all_month_model = ts_all_month %>% seas 
all_month_adj = all_month_model %>% final

plot(all_month_model)

all_month_adj_df = data.frame(date=date_decimal(index(all_month_adj)), y = melt(all_month_adj)$value)


# Convert DC to normalized values -----------------------------------------
correction_month = all_by_month %>% 
  ungroup() %>% 
  mutate(natl = all_month_adj_df$y,
         adj = num / all_month_adj_df$y)

arrivals_adj = dc_month %>% 
  ungroup() %>% 
  group_by(airport) %>% 
  mutate(natl = all_by_month$num,
         adj = total / natl)

ggplot(arrivals_adj %>% filter(year<2007, year>2004), aes(x = yr_month, y = adj,
                                                          group = airport, colour= airport)) +
  geom_line()+ 
  theme_bw()


# monthly pattern ----------------------------------------------------------

# For each of the airports, fit a monthly seasonal model.
# Create a ts object to input into seasonal::seas
ts_dca_month = ts(dc_by_month %>% filter(airport == 'Reagan') %>% select(num), 
                  frequency = 12, start = c(2000, 1))


# Using seasonal package to calculate the seasonally-adjusted patterns.
dca_month_model = ts_dca_month %>% seas 
dca_month_adj = dca_month_model %>% final

plot(dca_month_model)
plot(dca_month_adj)


# Weekly adjustment -------------------------------------------------------

ts_dca_date = ts(dc_by_date %>% filter(airport == 'Reagan') %>% select(num), 
                 frequency = 52, start = c(2000, 1,1))

dca_date_model = auto.arima(ts_dca_date)
