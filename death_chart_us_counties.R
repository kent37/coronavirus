# Death chart for US counties

death_by_county = read_and_clean_ny_times_counties() %>% 
  select(-Cases, -FIPS, Count=Deaths)
min_county_deaths = 10
last_date = max(death_by_county$Date)

# For each county, make a data series that
# starts at min_county_deaths cases
county_deaths = by_day_since_min(death_by_county, County_State, 
                                 min_county_deaths) 

# How many countries?
length(unique(county_deaths$County_State))

min_days = 5
# Only countries with >=min_days days >= min_county_deaths
to_plot_county_deaths = county_deaths %>% 
  filter(NumDays>=min_days)

# Order counties by state, then alpha
county_order = order_counties(to_plot_county_deaths)

to_plot_county_deaths$County_State = 
  factor(to_plot_county_deaths$County_State,
         levels=county_order$County_State)

death_chart_county = growth_chart_base(to_plot_county_deaths, 
                                        County_State, 'darkred',
                                    highlight_counties) +
  labs(x=case_chart_x(min_county_deaths), y='',
       title='Coronavirus reported deaths by US county',
       subtitle=death_chart_subtitle(min_county_deaths),
       caption=ny_times_credit(last_date))

county_death_df = to_plot_county_deaths %>% 
  with_sliding_window(County_State, state_window)
daily_death_county = new_cases_base(county_death_df, 
                                    County_State, 'darkred', 
                             highlight_counties, state_window) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily deaths',
       title='New reported deaths by US county',
       subtitle=str_glue(
         '{state_window_str} day average of daily reported deaths'),
       caption=ny_times_credit(last_date))

new_vs_all_county_death_chart = 
  new_vs_count_base(county_death_df, County_State, 'darkred') +
  labs(x='Total reported deaths', y='Daily new deaths', 
       title='New reported deaths vs all deaths by US county',
       subtitle=str_glue(
         '{state_window_str} day average of daily reported deaths ',
         'vs all deaths, log-log scale'),
       caption=ny_times_credit(last_date))

death_county_totals = totals_chart_base(death_by_county, County_State, 
                              last_date, min_county_deaths) +
  labs(x='Reported deaths (log scale)', y='',
       title='Reported coronavirus deaths by US county',
       subtitle=str_glue(
         'Showing counties with {min_county_deaths} or more deaths'),
       caption=ny_times_credit(last_date)) 

selected_counties_death_base_plot = 
  selected_item_base(county_death_df, selected_counties, County_State, Sliding) +
    labs(x=case_chart_x(min_county_deaths), y='Reported deaths',
       title='Daily reported deaths, selected US counties',
       subtitle=selected_death_chart_subtitle(state_window_str, min_county_deaths),
       caption=ny_times_credit(last_date))

selected_counties_death_log_plot = selected_counties_death_base_plot +
  my_y_log10()

selected_counties_death_plot = selected_counties_death_base_plot +
  scale_y_continuous(labels=scales::comma)
