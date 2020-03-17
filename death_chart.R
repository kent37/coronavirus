# Try to recreate the Financial Times chart

library(tidyverse)
library(lubridate)

death = read_csv(here::here('data/time_series_19-covid-Deaths.csv'))

min_death_cases = 20
last_date = names(death) %>% tail(1)
death_by_country = death %>% 
  rename(Country=`Country/Region`) %>% 
  mutate(Country=recode(Country, 
                        `Korea, South`='South Korea',
                        US='United States')) %>% 
  mutate(Country = 
           if_else(!is.na(`Province/State`) & 
                     `Province/State`=='Hong Kong', 'Hong Kong', Country)) %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(Country) %>% 
  summarize_all(sum) %>% 
  filter(.data[[last_date]] >= min_death_cases) %>% 
  pivot_longer(-Country, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

# For each country, make a data series that
# starts at min_death_cases cases
filter_country = function(df) {
  df %>% 
    filter(Count >= min_death_cases) %>% 
    select(-Date) %>% 
    mutate(Day=row_number(Count))
}

death_by_day = death_by_country %>% 
  group_nest(Country) %>% 
  mutate(data=map(data, filter_country)) %>% 
  unnest(data) %>% 
  group_by(Country) %>% 
  mutate(NumDays=max(Day)) %>% 
  ungroup()

# How many countries?
length(unique(death_by_day$Country))

# Only countries with >5 days >= min_death_cases
to_plot = death_by_day %>% 
  filter(!Country %in% c('Cruise Ship'),
         NumDays>5)

library(gghighlight)

ggplot(to_plot, aes(Day, Count, color=Country)) +
  geom_line(size=1) +
#  geom_abline(slope=1/8, intercept=log10(min_death_cases)) +
  gghighlight(max(Count) > 20, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(values=rep('red', length(unique(to_plot$Country)))) +
  guides(color='none') +
  labs(x=str_glue('Number of days since {min_death_cases}th death'), y='',
       title='Coronavirus deaths by country',
       subtitle=str_glue('Cumulative number of deaths, ',
                         'by number of days since {min_death_cases}th case'),
       caption=str_glue('Source: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')) +
  facet_wrap(~Country, strip.position='bottom') +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

