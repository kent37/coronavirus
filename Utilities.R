# Utilities
library(tidyverse)
library(ggrepel)
library(lubridate)

# Clean country data
clean_country = function(df, min_cases) {
  last_date = names(df) %>% tail(1)
  
df %>% 
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
  filter(.data[[last_date]] >= min_cases) %>% 
  pivot_longer(-Country, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date))
}

# Read and clean state data from covidtracking.com
state_lookup = state.name %>% set_names(state.abb)
state_lookup['DC'] = 'District of Columbia'
state_lookup['AS'] = 'American Samoa'
state_lookup['GU'] = 'Guam'
state_lookup['MP'] = 'Northern Mariana Islands'
state_lookup['PR'] = 'Puerto Rico'
state_lookup['UM'] = 'Minor Outlying Islands'
state_lookup['VI'] = 'US Virgin Islands'

read_and_clean_covid_tracking = function() {
  df = read_csv(here::here('data/covid_tracking_daily.csv'))
  df %>% 
    select(Date=date, State=state, Cases=positive, Deaths=death) %>% 
    mutate(Date=as.Date(strptime(Date, '%Y%m%d')),
           State=state_lookup[State])
}

# Clean state data from USA Facts
clean_usa_facts_state = function(df, min_cases) {
  last_date = names(df) %>% tail(1)
  
  df %>% 
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
    filter(!is.na(State)) %>% 
    mutate(State=if_else(State %in% names(state_lookup),
                         state_lookup[State], State)) %>% 
    mutate(State=recode(State, DC='District of Colombia')) %>% 
    group_by(State) %>% 
    summarize_all(sum) %>% 
    filter(.data[[last_date]] >= min_cases) %>% 
    pivot_longer(-State, names_to='Date', values_to='Count') %>% 
    mutate(Date=mdy(Date))
}

# For each division_name, make a data series that
# starts at min_cases cases
by_day_since_min = function(df, division_name, min_cases) {
  df %>% 
    group_nest({{division_name}}) %>% 
    mutate(data=map(data, to_day_series, min_cases)) %>% 
    unnest(data) %>% 
    group_by({{division_name}}) %>% 
    mutate(NumDays=max(Day)) %>% 
    ungroup()
}

# Make a data series that
# starts at min_cases cases
to_day_series = function(df, min_cases) {
  df %>% 
    filter(Count >= min_cases) %>% 
    select(-Date) %>% 
    mutate(Day=row_number(Count))
}


# Shared theme for everything
theme_set(silgelib::theme_plex())

# Common code for growth charts
growth_chart_base = function(df, division_name, color, highlights) {
  division_name_str = rlang::as_string(ensym(division_name))
  colors = rep(color, length(unique(df[[division_name_str]])))
  
  # Truncate China
  if (division_name_str == 'Country') {
    xlim = df %>% 
      filter(Country != 'China') %>% 
      pull(Day) %>% 
      max()
    xlim = 10 * ceiling(xlim/10) # Round up to multiple of ten
  } else xlim=NA
  
  # Make some helper dataframes
  # The points at the end of the curves
  df_endpoints = df %>% 
    group_by({{division_name}}) %>%
    top_n(1, Day)
  
  # This will be the overall grey background
  df_no_div = df %>% rename(group={{division_name}})
  
  # The highlight curves
  df_highlights = df %>% 
    filter({{division_name}} %in% highlights) %>% 
    rename(group={{division_name}})
  df_highlight_endpoints = df_highlights %>% 
    group_by(group) %>%
    top_n(1, Count)
  
  ggplot(df, aes(Day, Count, color={{division_name}})) +
    # Grey background lines
    geom_line(data=df_no_div, aes(group=group),
              size = 0.2, color = "gray80") +
    # Highlight lines and points
      geom_line(data=df_highlights, aes(group=group), 
                color='gray10', size=0.2, lineend='round') +
      geom_point(data=df_highlight_endpoints, size=.2, color='gray10') +
    # Primary lines and points
      geom_line(size=0.5, lineend='round') +
      geom_point(data=df_endpoints, size=1, shape=21, fill=color) +
    # geom_abline(slope=log10(sqrt(2)), 
    #             intercept=log10(min_country_cases),
    #             color='red', alpha=0.5, linetype=3, size=0.5) +
    # geom_abline(slope=log10(2^(1/3)), 
    #             intercept=log10(min_country_cases),
    #             color='blue', alpha=0.5, linetype=3, size=0.5) +
    scale_x_continuous(minor_breaks=NULL, limits=c(NA, xlim)) +
    scale_y_log10(labels=scales::comma, minor_breaks=NULL) +
    scale_color_manual(values=colors, guid='none') +
    facet_wrap(vars({{division_name}}), strip.position='bottom', ncol=4) +
    silgelib::theme_plex() +
    theme(panel.spacing.y=unit(1, 'lines'),
          strip.text=element_text(color=color, size=10))
}
totals_chart_base = function(df, division_name, last_date, min_cases) {
  # Compute totals
  if (is.character(last_date)) last_date=mdy(last_date)
  totals_only = df %>% 
    filter(Date==last_date, Count >= min_cases) %>% 
    mutate({{division_name}} := fct_reorder({{division_name}}, Count))

  ggplot(totals_only, aes(Count, {{division_name}}, 
                        label=scales::comma(Count, accuracy=1))) +
  geom_col(fill='steelblue') +
  geom_text(color='white', fontface='bold', size=3, hjust=1.2) +
  scale_x_log10(labels=NULL) +
  silgelib::theme_plex() +
  theme(panel.grid=element_blank())
}

# Selected countries and states
selected_countries = c('China', 'United States', 'South Korea', 
             'Italy', 'Spain', 'France', 'United Kingdom')
highlight_countries = c('United States', 'Japan')

selected_states = c('New York', 'Massachusetts', 'California', 
                    'Florida', 'Washington', 'Oregon')
highlight_states = c('New York', 'Massachusetts', 'Florida')

selected_item_base = function(df, selection, division_name) {
  selected_to_plot = df %>% 
    filter({{division_name}} %in% selection) %>% 
    group_by({{division_name}}) %>% 
    mutate(label=if_else(Day==max(Day), {{division_name}}, NA_character_))

  ggplot(selected_to_plot,
         aes(Day, Count, color={{division_name}}, label=label)) +
    geom_line(size=1) +
    geom_text_repel(nudge_x = 1.1, nudge_y = 0.1, 
                    segment.color = NA, size=3, ) +
    scale_color_brewer(palette='Dark2') +
    silgelib::theme_plex() +
    theme(legend.position='none')
}

case_chart_subtitle = function(min_cases) {
  str_glue('Cumulative number of cases, ',
                         'by number of days since {min_cases}th case')
}

case_chart_x = function(min_cases) {
  str_glue('Number of days since {min_cases}th case')
}

death_chart_subtitle = function(min_cases) {
  str_glue('Cumulative number of deaths, ',
    'by number of days since {min_cases}th death')
}

death_chart_x = function(min_cases) {
  str_glue('Number of days since {min_cases}th death')
}

jhu_credit = function(last_date) {
  str_glue('Source: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')
}

covid_tracking_credit = function(last_date) {
  str_glue('Source: COVID Tracking Project as of {last_date}\n',
    'https://covidtracking.com/')
}

usafacts_credit = function(last_date) {
  str_glue('Source: USAFacts as of {last_date}\n',
    'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')
}
