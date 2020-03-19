# Try to recreate the Financial Times chart

library(tidyverse)
library(lubridate)
library(gghighlight)

conf = read_csv(here::here('data/time_series_19-covid-Confirmed.csv'))

min_country_cases = 100
last_date = names(conf) %>% tail(1)
by_country = conf %>% 
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
  filter(.data[[last_date]] >= min_country_cases) %>% 
  pivot_longer(-Country, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

# For each country, make a data series that
# starts at min_country_cases cases
filter_country = function(df) {
  df %>% 
    filter(Count >= min_country_cases) %>% 
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

# Only countries with >5 days >= min_country_cases
to_plot = hundreds %>% 
  filter(!Country %in% c('Cruise Ship'),
         NumDays>=5)

growth_chart = ggplot(to_plot, aes(Day, Count, color=Country)) +
  geom_line(size=1) +
#  geom_abline(slope=1/8, intercept=log10(min_country_cases)) +
  gghighlight(max(Count) > 20, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(values=rep('red', length(unique(to_plot$Country)))) +
  guides(color='none') +
  labs(x=str_glue('Number of days since {min_country_cases}th case'), y='',
       title='Coronavirus cases by country',
       subtitle=str_glue('Cumulative number of cases, ',
                         'by number of days since {min_country_cases}th case'),
       caption=str_glue('Source: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')) +
  facet_wrap(~Country, strip.position='bottom', ncol=4) +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

totals_only = by_country %>% 
  filter(Date==mdy(last_date), Count >= min_country_cases) %>% 
  mutate(Country=fct_reorder(Country, Count))

growth_totals = ggplot(totals_only, aes(Count, Country)) +
  geom_segment(aes(xend=100, yend=Country), color='gray10', size=0.1) +
  geom_point(color='red') +
  scale_x_log10(labels=scales::comma) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by country',
       subtitle=str_glue('Showing countries with at least {min_country_cases} cases'),
       caption=str_glue('Source: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')) +
  silgelib::theme_plex() +
  theme(panel.grid=element_blank())
