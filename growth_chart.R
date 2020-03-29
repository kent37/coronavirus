# Try to recreate the Financial Times chart
conf = read_csv(here::here('data/time_series_covid19_confirmed_global.csv'))

min_country_cases = 100
last_date = names(conf) %>% tail(1)
by_country = conf %>% clean_country(min_country_cases)

# For each country, make a data series that
# starts at min_country_cases cases
country_cases = by_day_since_min(by_country, Country, min_country_cases)

# How many countries?
length(unique(country_cases$Country))

# Only countries with >min_days days >= min_country_cases
min_days = 5
to_plot_country = country_cases %>% 
  filter(!Country %in% c('Cruise Ship', 'Diamond Princess'),
         NumDays>=min_days)

growth_chart = growth_chart_base(to_plot_country, Country, 'darkred',
                 highlight_countries) +
  labs(x=case_chart_x(min_country_cases), y='',
       title='Coronavirus cases by country',
       subtitle=case_chart_subtitle(min_country_cases),
       caption=jhu_credit(last_date))

country_daily_df = to_plot_country %>% with_sliding_window(Country, 7)
daily_chart = new_cases_base(country_daily_df, Country, 'darkred', 
                             highlight_countries, 7) +
  labs(x=case_chart_x(min_country_cases),
       y='Daily new cases',
       title='New reported cases by country',
       subtitle='Seven day average of daily reported cases',
       caption=jhu_credit(last_date))

growth_total = totals_chart_base(by_country, Country, 
                                    last_date, min_country_cases) +
  labs(x='Reported cases (log scale)', y='',
       title='Reported coronavirus cases by country',
       subtitle=str_glue(
         'Showing countries with {min_country_cases} or more cases'),
       caption=jhu_credit(last_date))

selected_country_base_plot = 
  selected_item_base(to_plot_country, selected_countries, Country) +
  labs(x=case_chart_x(min_country_cases), y='Reported cases',
       title='Cumulative reported coronavirus cases, selected countries',
       subtitle=case_chart_subtitle(min_country_cases),
       caption=jhu_credit(last_date))

selected_country_plot = selected_country_base_plot +
  scale_y_continuous(labels=scales::comma)

selected_country_log_plot = selected_country_base_plot +
  my_y_log10()
