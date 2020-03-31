# Look at average total cases vs average daily cases
# as a way to estimate days to double
# If cases are growing exponentially, then
# cases = r^t
# for some daily rate of growth r and time in days t
# Then cases = exp(log(r))^t = exp(log(r) * t)
# Because exponential growth is magic,
# d_case/d_t = log(r) * exp(log(r)*t)
# So d_case/d_t = log(r) * cases
# and the slope of the line daily cases vs total cases 
# is log(r)
#
# Days to double
# If cases are growing as r^t, 
# then the days to double is n
# where 2 = r^n
# so log(2) = n log(r)
# and n = log(2) / log(r)
# where log(r) is the slope of the daily vs total line
county_growth_df %>% 
  filter(Count>200, !is.na(Sliding)) %>% 
  mutate(Ratio=Sliding/Count) %>%  
  pull(Ratio) %>% median()
  ggplot(aes(Ratio)) + geom_histogram()

days=country_window
division=quo(Country)
df = to_plot_country %>% 
    filter(NumDays >= 2*days) %>% 
    group_by({{division_name}}) %>% 
    arrange(Day) %>% 
    mutate(
      SlidingCount = slide_dbl(Count, mean, 
                             .before=(days-1), .complete=TRUE),
      Change=c(NA, diff(Count)),
      Sliding=slide_dbl(Change, mean, 
                             .before=(days-1), .complete=TRUE),
      Ratio=Sliding/SlidingCount,
      Doubling=log(2)/Ratio)

latest = df %>% filter(Day==NumDays)
