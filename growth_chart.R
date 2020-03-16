# Try to recreate the Financial Times chart

library(tidyverse)
library(lubridate)

conf = read_csv(here::here('data/time_series_19-covid-Confirmed.csv'))


last_date = names(conf) %>% tail(1)
by_country = conf %>% 
  select(-`Province/State`, -Lat, -Long, 
         Country=`Country/Region`) %>% 
  mutate(Country=recode(Country, 
                        `Korea, South`='South Korea',
                        US='United States')) %>% 
  group_by(Country) %>% 
  summarize_all(sum) %>% 
  filter(.data[[last_date]] >= 100) %>% 
  pivot_longer(-Country, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

by_country %>% 
  filter(Count>0) %>% 
  ggplot(aes(Date, Count, color=Country)) +
  geom_line() +
  scale_y_log10()

# For each country, make a data series that
# starts at 100 cases
filter_country = function(df) {
  df %>% 
    filter(Count >= 100) %>% 
    select(-Date) %>% 
    mutate(Day=row_number(Count))
}

hundreds = by_country %>% 
  group_nest(Country) %>% 
  mutate(data=map(data, filter_country)) %>% 
  unnest(data) %>% 
  group_by(Country) %>% 
  mutate(NumDays=max(Day)) %>% 
  ungroup()

# How many countries?
length(unique(hundreds$Country))

# Leave China out of it, and only countries with 5 days
to_plot = hundreds %>% 
  filter(!Country %in% c('China', 'Cruise Ship'))

p = ggplot(to_plot, aes(Day, Count, color=Country)) +
  geom_line(size=1) +
  scale_y_log10() +
  silgelib::theme_plex()
plotly::ggplotly(p)

library(gghighlight)
to_plot = to_plot %>% 
  filter(NumDays>5)

ggplot(to_plot, aes(Day, Count, color=Country)) +
  geom_line(size=1) +
  gghighlight(max(Count) > 20, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(values=rep('red', length(unique(to_plot$Country)))) +
  guides(color='none') +
  labs(x='Number of days since 100th case', y='',
       title='Coronavirus cases by country',
       subtitle='Cumulative number of cases, by number of days since 100th case',
       caption='Source: Johns Hopkins CSSE\nhttps://github.com/CSSEGISandData/COVID-19') +
  facet_wrap(~Country, strip.position='bottom') +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

