# Utilities
library(tidyverse)
library(ggrepel)
library(lubridate)
library(slider)

#### Read and clean data ####

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
state_lookup['VI'] = 'Virgin Islands'

state_reverse_lookup = names(state_lookup) %>% set_names(state_lookup)

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

# Read and clean county data from NY Times
read_and_clean_ny_times_counties = function() {
  read_csv(here::here('data/ny_times_counties.csv')) %>% 
    rename_all(str_to_title) %>% 
    rename(FIPS=Fips) %>% 
    mutate(County_State=str_glue('{County}, {state_reverse_lookup[State]}'))
}

#### Data munging ####

# For each division_name, make a data series that
# starts at min_cases cases
by_day_since_min = function(df, division_name, min_cases) {
  df %>% 
    filter(!is.na(Count)) %>% 
    group_nest({{division_name}}) %>% 
    mutate(data=map(data, to_day_series, min_cases)) %>% 
    unnest(data) %>% 
    group_by({{division_name}}) %>% 
    mutate(NumDays=max(Day, na.rm=TRUE)) %>% 
    ungroup()
}

# Make a data series that
# starts at min_cases cases
to_day_series = function(df, min_cases) {
  df[df$Count >= min_cases,] %>% 
    #filter(Count >= min_cases) %>% 
  mutate(Day=row_number(Date))
}

# Order counties by state, then alpha
order_counties = function(df) {
  df %>% 
  select(County, State, County_State) %>% 
  mutate(State=state_reverse_lookup[State]) %>% 
  distinct() %>% 
  arrange(State, County)
}

# Sliding average of new cases
with_sliding_window = function(df, division_name, days) {
  df %>% 
    filter(NumDays >= 2*days) %>% 
    group_by({{division_name}}) %>% 
    arrange(Day) %>% 
    mutate(Change=c(NA, diff(Count)),
           Sliding=slide_dbl(Change, mean, 
                             .before=(days-1), .complete=TRUE),
           ChangeSliding=c(NA, diff(Sliding)))
}

# Sliding window sizes
country_window = 5
country_window_str = 'Five'
state_window = 3
state_window_str = 'Three'

#### Charting code ####
# Shared theme for everything
theme_set(silgelib::theme_plex())

# Common code for growth charts
growth_chart_base = function(df, division_name, color, highlights) {
  division_name_str = rlang::as_string(ensym(division_name))

  # Truncate China
  if (division_name_str == 'Country') {
    xmax = df %>% 
      filter(Country != 'China') %>% 
      pull(Day) %>% 
      max()
    xmax = 10 * ceiling(xmax/10) # Round up to multiple of ten
  } else xmax=NA
  
  facet_chart_base(df, Day, Count, {{division_name}}, color, 
                   highlights) +
    scale_x_continuous(labels=scales::label_comma(accuracy=1),
                       minor_breaks=NULL, limits=c(NA, xmax))
}

# Chart of new cases vs day
new_cases_base = function(sliding, division_name, color, highlights, days) {
  facet_chart_base(sliding, Day, Sliding, {{division_name}}, 
                   color, highlights) +
    scale_x_continuous(labels=scales::label_comma(accuracy=1),
                       minor_breaks=NULL, limits=c(days, NA))
}

# Chart of new cases vs total cases on log-log scale
new_vs_count_base = function(df, division_name, color) {
  # Highlight the single highest item only
  highlight = df %>% ungroup() %>% top_n(1, Count) %>% pull({{division_name}})
  facet_chart_base(df, Count, Sliding, {{division_name}}, 
                   color, highlight) +
    scale_x_log10(labels=scales::label_number_si(),
                  minor_breaks=NULL)
 }

# Common code for faceted charts with grey background lines
facet_chart_base = function(df, x_name, y_name, division_name, color='darkred', 
                            highlights) {
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
  
  ggplot(df, aes({{x_name}}, {{y_name}})) +
    # Grey background lines
    geom_line(data=df_no_div, aes(group=group),
              size = 0.2, color = "gray80") +
    # Highlight lines and points
    geom_line(data=df_highlights, aes(group=group), 
              color='gray20', size=0.2, lineend='round') +
    # Primary lines and points
    geom_line(size=0.8, lineend='round', color=color) +
    geom_point(data=df_endpoints, size=1, shape=21, fill=color) +
    my_y_log10() +
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

#### Selected countries and states ####

selected_countries = c('China', 'United States', 'South Korea', 
             'Italy', 'Spain', 'France', 'United Kingdom', 'Germany')
highlight_countries = c('United States', 'Japan')

selected_states = c('New York', 'Massachusetts', 'California', 
                    'Florida', 'Washington', 'Oregon')
highlight_states = c('New York', 'Massachusetts', 'Florida')

highlight_counties = c('New York City, NY', 'Middlesex, MA')
selected_counties = c(highlight_counties,
                      'Suffolk, MA', 'Benton, OR',
                      'Suffolk, NY', 'Westchester, NY',
                      'Sarasota, FL', 'Orleans, LA')

selected_item_base = function(df, selection, division_name, x_name, y_name) {
  selected_to_plot = df %>% 
    filter({{division_name}} %in% selection) %>% 
    group_by({{division_name}}) %>% 
    mutate(label=if_else(Day==max(Day, na.rm=TRUE), 
                         as.character({{division_name}}), NA_character_))

  # Dark2 palette only has 8 colors, Paired has 12
  n_to_plot = n_distinct(selected_to_plot %>% pull({{division_name}}))
  stopifnot(n_to_plot <= 12) # Too many items to color
  palette_name = if (n_to_plot <= 8) 'Dark2' else 'Paired'
  ggplot(selected_to_plot,
         aes({{x_name}}, {{y_name}}, color={{division_name}}, label=label)) +
    geom_line(size=1) +
    geom_text_repel(nudge_x = 1.1, nudge_y = 0.1, 
                    segment.color = NA, size=3) +
    scale_color_brewer(palette=palette_name) +
    silgelib::theme_plex() +
    theme(legend.position='none')
}

#### Chart helpers ####
my_y_log10 = function() {
  scale_y_log10(labels=scales::label_comma(accuracy=1),
                minor_breaks=NULL)
}

case_chart_subtitle = function(min_cases) {
  str_glue('Cumulative number of cases, ',
                         'by number of days since {min_cases}th case')
}

selected_daily_chart_subtitle = function(day_str, min_cases) {
  str_glue('New reported coronavirus cases, {str_to_lower(day_str)} day average')
}

case_chart_x = function(min_cases) {
  str_glue('Number of days since {min_cases}th case')
}

death_chart_subtitle = function(min_cases) {
  str_glue('Cumulative number of deaths, ',
    'by number of days since {min_cases}th death')
}

selected_death_chart_subtitle = function(day_str, min_cases) {
  str_glue('Number of new deaths, {str_to_lower(day_str)} day average')
}

death_chart_x = function(min_cases) {
  str_glue('Number of days since {min_cases}th death')
}

jhu_credit = function(last_date) {
  str_glue('Chart: @kent3737 | Data: Johns Hopkins CSSE as of {last_date}\n',
                        'https://github.com/CSSEGISandData/COVID-19')
}

covid_tracking_credit = function(last_date) {
  str_glue('Chart: @kent3737 | Data: COVID Tracking Project as of {last_date}\n',
    'https://covidtracking.com/')
}

ny_times_credit = function(last_date) {
  str_glue('Chart: @kent3737 | Data: The New York Times as of {last_date}\n',
    'https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html')
}

usafacts_credit = function(last_date) {
  str_glue('Chart: @kent3737 | Data: USAFacts as of {last_date}\n',
    'https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/')
}
