# Growth chart for US states only

library(tidyverse)
library(lubridate)
library(gghighlight)

conf_us = read_csv(here::here('data/covid_confirmed_usafacts.csv')) %>% 
  select(-matches('X\\d+')) # Remove extra columns

min_cases = 10
last_date = names(conf_us) %>% tail(1)
state_lookup = state.name %>% set_names(state.abb)
by_state = conf_us %>% 
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
  filter(.data[[last_date]] >= min_cases) %>% 
  pivot_longer(-State, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))

# For each state, make a data series that
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
to_plot_us = affected %>% 
  filter(!State %in% c('Diamond Princess', "Grand Princess"),
         NumDays>=min_days)

growth_chart_us = ggplot(to_plot_us, aes(Day, Count, color=State)) +
  geom_line(size=1) +
  # geom_abline(slope=log10(sqrt(2)), 
  #             intercept=log10(min_cases),
  #             color='red', alpha=0.5, linetype=3, size=0.5) +
  # geom_abline(slope=log10(2^(1/3)), 
  #             intercept=log10(min_cases),
  #             color='blue', alpha=0.5, linetype=3, size=0.5) +
  gghighlight(max(Count) > 20, 
              unhighlighted_params=list(size=0.5),
              use_direct_label=FALSE) +
  scale_x_continuous(minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma) +
  scale_color_manual(values=rep('darkred', 
                                length(unique(to_plot_us$State)))) +
  guides(color='none') +
  labs(x=str_glue('Number of days since {min_cases}th case'), y='',
       title='Coronavirus reported cases by state',
       subtitle=str_glue('Cumulative number of reported cases, ',
                         'by number of days since {min_cases}th case'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  facet_wrap(~State, strip.position='bottom', ncol=4) +
  silgelib::theme_plex() +
  theme(panel.spacing.y=unit(1, 'lines'))

totals_only = by_state %>% 
  filter(Date==mdy(last_date), Count >= min_cases) %>% 
  mutate(State=fct_reorder(State, Count))

growth_us_totals = ggplot(totals_only, 
                          aes(Count, State, 
                              label=scales::comma(Count, accuracy=1))) +
  # geom_segment(aes(xend=min_cases, yend=State), color='gray10', size=0.1) +
  # geom_point(color='red') +
  geom_col(fill='steelblue') +
  geom_text(color='white', fontface='bold', size=3, hjust=1.2) +
  scale_x_log10(labels=NULL) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by US state',
       subtitle=str_glue('Showing states with {min_cases} or more cases'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  silgelib::theme_plex() +
  theme(panel.grid=element_blank())

# selected states
selected_states = c('New York', 'Massachusetts', 'California', 
                    'Florida', 'Washington', 'Oregon')

selected_states_to_plot = to_plot_us %>% 
  filter(State %in% selected_states) %>% 
  group_by(State) %>% 
  mutate(label=if_else(Day==max(Day), State, NA_character_))

selected_states_plot = selected_states_to_plot %>% 
  ggplot(aes(Day, Count, color=State, label=label)) +
  geom_line(size=1) +
  geom_text_repel(nudge_x = 1.1, nudge_y = 0.1, 
                  segment.color = NA, size=3, show.legend=FALSE) +
  scale_y_log10(labels=scales::comma) +
  scale_color_brewer(palette='Dark2') +
    labs(x=str_glue('Number of days since {min_cases}th case'), y='',
       title='Coronavirus reported cases by state',
       subtitle=str_glue('Cumulative number of reported cases, ',
                         'by number of days since {min_cases}th case'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  silgelib::theme_plex() +
  theme(legend.position='none')

selected_states_daily_plot = selected_states_to_plot %>% 
  group_by(State) %>% 
  arrange(Day) %>% 
  mutate(Daily=c(0, diff(Count))) %>% 
  ggplot(aes(Day, Daily, fill=State, label=label)) +
  geom_col() +
  # geom_text_repel(nudge_x = 1.1, nudge_y = 0.1, 
  #                 segment.color = NA, size=3, ) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_brewer(palette='Dark2') +
  facet_wrap(~State, ncol=1, scales='free_y') +
  labs(x='Reported cases', y='',
       title='Daily reported coronavirus cases, selected states',
       subtitle=str_glue('Daily number of cases, ',
                         'by number of days since {min_cases}th case'),
       caption=str_glue('Source: USAFacts as of {last_date}\n',
                        'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')) +
  silgelib::theme_plex() +
  theme(legend.position='none')
  