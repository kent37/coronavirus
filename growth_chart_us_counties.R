# Growth chart for US counties

by_county = read_and_clean_ny_times_counties() %>% 
  select(-Deaths, -FIPS, Count=Cases)
min_county_cases = 100
last_date = max(by_county$Date)

# For each county, make a data series that
# starts at min_county_cases cases
county_cases = by_day_since_min(by_county, County_State, min_county_cases) 

# How many countries?
length(unique(county_cases$County_State))

min_days = 5
# Only countries with >=min_days days >= min_county_cases
to_plot_county = county_cases %>% 
  filter(NumDays>=min_days)

# Order counties by state, then alpha
county_order = order_counties(to_plot_county)

to_plot_county$County_State = 
  factor(to_plot_county$County_State,
         levels=county_order$County_State)

highlight_counties = c('New York City, NY', 'Middlesex, MA')
growth_chart_county = growth_chart_base(to_plot_county, County_State, 'darkred',
                                    highlight_counties) +
  labs(x=case_chart_x(min_county_cases), y='',
       title='Coronavirus reported cases by US county',
       subtitle=case_chart_subtitle(min_county_cases),
       caption=ny_times_credit(last_date))

county_growth_df = to_plot_county %>% with_sliding_window(County_State, 5)
daily_growth_county = new_cases_base(county_growth_df, County_State, 'darkred', 
                             highlight_counties, 5) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily new cases',
       title='New reported cases by US county',
       subtitle='Five day average of daily reported cases',
       caption=ny_times_credit(last_date))

growth_county_totals = totals_chart_base(by_county, County_State, 
                              last_date, min_county_cases) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by US county',
       subtitle=str_glue(
         'Showing counties with {min_county_cases} or more cases'),
       caption=ny_times_credit(last_date)) 

selected_counties = county_order %>% 
  filter(State %in% c('MA', 'NY')) %>% 
  pull(County_State) %>% 
  as.character()

selected_counties_base_plot = 
  selected_item_base(to_plot_county, selected_counties, County_State) +
    labs(x=case_chart_x(min_county_cases), y='Reported cases',
       title='Coronavirus reported cases by US county',
       subtitle=case_chart_subtitle(min_county_cases),
       caption=ny_times_credit(last_date))

selected_counties_log_plot = selected_counties_base_plot +
  my_y_log10()

selected_counties_plot = selected_counties_base_plot +
  scale_y_continuous(labels=scales::comma)