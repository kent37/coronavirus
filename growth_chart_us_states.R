# Growth chart for US states only

library(tidyverse)
library(lubridate)

conf = read_csv(here::here('data/time_series_19-covid-Confirmed.csv'))

min_cases = 10
last_date = names(conf) %>% tail(1)
state_lookup = state.name %>% set_names(state.abb)
by_state = conf %>% 
  filter(`Country/Region`=='US') %>% 
  rename(State=`Province/State`) %>%  
  select(-`Country/Region`, -Lat, -Long) %>% 
  # There are many individual counties listed, we need to split them out
  extract(State, c('County', 'State_abbr'), 
          '(.+?), ([:upper:]{2})', remove=FALSE) %>% 
  mutate(State=if_else(!is.na(State_abbr) & State_abbr %in% names(state_lookup),
                       state_lookup[State_abbr], State)) %>% 
  mutate(State=recode(State, `Washington, D.C.`='District of Columbia')) %>% 
  select(-County, -State_abbr) %>% 
  group_by(State) %>% 
  summarize_all(sum) %>% 
  filter(.data[[last_date]] >= min_cases) %>% 
  pivot_longer(-State, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

# For each country, make a data series that
# starts at min_cases cases
filter_cases = function(df) {
  df %>% 
    filter(Count >= min_cases) %>% 
    select(-Date) %>% 
    mutate(Day=row_number(Count))
}

affected = by_state %>% 
  group_nest(State) %>% 
  mutate(data=map(data, filter_cases)) %>% 
  unnest(data) %>% 
  group_by(State) %>% 
  mutate(NumDays=max(Day)) %>% 
  ungroup()

# How many countries?
length(unique(affected$State))

min_days = 5
# Only countries with >=min_days days >= min_cases
# No cruise ships
to_plot = affected %>% 
  filter(!State %in% c('Diamond Princess', "Grand Princess"),
         NumDays>=min_days)

library(gghighlight)

ggplot(to_plot, aes(Day, Count, color=State)) +
  geom_line(size=1) +
#  geom_abline(slope=1/8, intercept=log10(min_cases)) +
  gghighlight(max(Count) > 20, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(values=rep('red', length(unique(to_plot$State)))) +
  guides(color='none') +
  labs(x=str_glue('Number of days since {min_cases}th case'), y='',
       title='Coronavirus cases by state',
       subtitle=str_glue('Cumulative number of cases, ',
                         'by number of days since {min_cases}th case'),
       caption=str_glue('Source: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')) +
  facet_wrap(~State, strip.position='bottom') +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

