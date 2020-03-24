# Growth chart for US states only

library(tidyverse)
library(lubridate)
library(gghighlight)

death_us = read_csv(here::here('data/covid_deaths_usafacts.csv'))

min_death_cases = 5
last_date = names(death_us) %>% tail(1)
state_lookup = state.name %>% set_names(state.abb)
death_by_state = death_us %>% 
  # filter(`Country/Region`=='US') %>%
  # rename(State=`Province/State`) %>%
  # select(-`Country/Region`, -Lat, -Long) %>% 
  select(-countyFIPS, -`County Name`, -stateFIPS) %>% 
  # There are many individual counties listed, we need to split them out
  # extract(State, c('County', 'State_abbr'), 
  #         '(.+?), ([:upper:]{2})', remove=FALSE) %>% 
  # mutate(State=if_else(!is.na(State_abbr) & State_abbr %in% names(state_lookup),
  #                      state_lookup[State_abbr], State)) %>% 
#  mutate(State=recode(State, `Washington, D.C.`='District of Columbia')) %>% 
#  select(-County, -State_abbr) %>% 
  mutate(State=if_else(State %in% names(state_lookup),
                       state_lookup[State], State)) %>% 
  mutate(State=recode(State, DC='District of Colombia')) %>% 
  group_by(State) %>% 
  summarize_all(sum) %>% 
  filter(.data[[last_date]] >= min_death_cases) %>% 
  pivot_longer(-State, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

# For each state, make a data series that
# starts at min_death_cases cases
filter_cases = function(df) {
  df %>% 
    filter(Count >= min_death_cases) %>% 
    select(-Date) %>% 
    mutate(Day=row_number(Count))
}

affected = death_by_state %>% 
  group_nest(State) %>% 
  mutate(data=map(data, filter_cases)) %>% 
  unnest(data) %>% 
  group_by(State) %>% 
  mutate(NumDays=max(Day)) %>% 
  ungroup()

# How many states?
length(unique(affected$State))

min_days = 5
# Only states with >=min_days days >= min_death_cases
# No cruise ships
to_plot_us_death = affected %>% 
  filter(!State %in% c('Diamond Princess', "Grand Princess"),
         NumDays>=min_days)

death_chart_us = ggplot(to_plot_us_death, aes(Day, Count, color=State)) +
  geom_line(size=1) +
#  geom_abline(slope=1/8, intercept=log10(min_death_cases)) +
  gghighlight(max(Count) > min_death_cases, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=partial(scales::comma, accuracy=1)) +
  scale_color_manual(values=rep('darkred', length(unique(to_plot_us_death$State)))) +
  guides(color='none') +
  labs(x=str_glue('Number of days since {min_death_cases}th case'), y='',
       title='Coronavirus deaths by state',
       subtitle=str_glue('Cumulative number of deaths, ',
                         'by number of days since {min_death_cases}th death'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  facet_wrap(~State, strip.position='bottom', ncol=4) +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

totals_only = death_by_state %>% 
  filter(Date==mdy(last_date), Count >= min_death_cases) %>% 
  mutate(State=fct_reorder(State, Count))

death_us_totals = ggplot(totals_only, aes(Count, State, 
                                          label=scales::comma(Count, accuracy=1))) +
  # geom_segment(aes(xend=min_death_cases, yend=State), color='gray10', size=0.1) +
  # geom_point(color='red') +
  geom_col(fill='steelblue') +
  geom_text(color='white', fontface='bold', size=3, hjust=1.2) +
  scale_x_log10(labels=NULL) +
  labs(x='Reported deaths (log scale)', y='',
       title='Reported coronavirus deaths by US state',
       subtitle=str_glue('Showing states with {min_death_cases} or more deaths'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  silgelib::theme_plex() +
  theme(panel.grid=element_blank())
