# Death charts by country

death = read_csv(here::here('data/time_series_covid19_deaths_global.csv'))

min_death_cases = 20
last_date = names(death) %>% tail(1) %>% mdy()
death_by_country = death %>% clean_country(min_death_cases)

# For each country, make a data series that
# starts at min_death_cases cases
death_by_day = by_day_since_min(death_by_country, Country, min_death_cases)

# How many countries?
length(unique(death_by_day$Country))

# Only countries with >min_death_days days >= min_death_cases
min_death_days = 5
to_plot_deaths = death_by_day %>% 
  filter(!Country %in% c('Cruise Ship'),
         NumDays>=min_death_days)

death_chart = growth_chart_base(to_plot_deaths, Country, 'darkred', 
                                highlight_countries) +
  labs(x=death_chart_x(min_death_cases), y='',
       title='Coronavirus deaths by country',
       subtitle=death_chart_subtitle(min_death_cases),
       caption=jhu_credit(last_date))

country_deaths_df = to_plot_deaths %>% 
  with_sliding_window(Country, country_window)
daily_deaths = new_cases_base(country_deaths_df, Country, 'darkred', 
                             highlight_countries, country_window) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily deaths',
       title='Daily reported deaths by country',
       subtitle=str_glue(
         '{country_window_str} day average of daily reported deaths'),
       caption=jhu_credit(last_date))

new_vs_all_death_chart = 
  new_vs_count_base(country_deaths_df, Country, 'darkred') +
  labs(x='Total reported deaths', y='Daily deaths', 
       title='New deaths vs all deaths by country',
       subtitle=str_glue(
         '{country_window_str} day average of daily reported deaths ',
         'vs all deaths, log-log scale'),
       caption=jhu_credit(last_date))

total_deaths = death_by_country %>% 
  filter(Date==last_date) %>% 
  pull(Count) %>% 
  sum(na.rm=TRUE)

death_totals = totals_chart_base(death_by_country, Country,
                      last_date, min_death_cases) +
  labs(x='Reported deaths (log scale)', y='',
    title='Reported coronavirus deaths by country',
    subtitle=str_glue('Showing countries with {min_death_cases} or more deaths\n',
         'World total deaths: {scales::comma(total_deaths)}'),
    caption=jhu_credit(last_date)) 

selected_death_base_plot = 
  selected_item_base(country_deaths_df, selected_countries, Country, Sliding) +
  labs(x=death_chart_x(min_death_cases), y='New reported deaths',
       title='Daily coronavirus deaths, selected countries',
       subtitle=selected_death_chart_subtitle(country_window_str, min_death_cases),
       caption=jhu_credit(last_date))

selected_death_plot = selected_death_base_plot +
  scale_y_continuous(labels=scales::comma)

selected_death_log_plot = selected_death_base_plot +
  my_y_log10()
