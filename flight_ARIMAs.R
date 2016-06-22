
# adjusting for seasonality with ARIMAs -----------------------------------------------------
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
  mutate(natl = correction_month$adj,
         adj = total / correction_month$adj)

ggplot(arrivals_adj, aes(x = yr_month, y = adj,
                         group = airport, colour= airport)) +
  geom_line()+ 
  theme_bw()
  

# daily pattern ----------------------------------------------------------

# Create a ts object to input into seasonal::seas
x = all_by_date %>% ungroup() %>% select(num)
ts_all_date = ts(all_by_date %>% ungroup() %>% select(num), 
                 frequency = 7, start = c(2000, 1,1))

# Using seasonal package to calculate the seasonally-adjusted patterns.
all_date_model = ts_all_date %>% seas 
all_month_adj = all_month_model %>% final

plot(all_month_model)
