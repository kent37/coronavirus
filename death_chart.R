# Death charts by country

death = read_csv(here::here('data/time_series_covid19_deaths_global.csv'))

min_death_cases = 20
last_date = names(death) %>% tail(1)
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

death_chart = growth_chart_base(to_plot_deaths, Country, 'darkred') +
  labs(x=death_chart_x(min_death_cases), y='',
       title='Coronavirus deaths by country',
       subtitle=death_chart_subtitle(min_death_cases),
       caption=jhu_credit(last_date))

death_totals = totals_chart_base(death_by_country, Country,
                      last_date, min_death_cases) +
  labs(x='Reported deaths (log scale)', y='',
    title='Reported coronavirus deaths by country',
    subtitle=str_glue('Showing countries with {min_death_cases} or more deaths'),
    caption=jhu_credit(last_date)) 

selected_death_base_plot = 
  selected_item_base(to_plot_deaths, selected_countries, Country) +
  labs(x=death_chart_x(min_death_cases), y='Reported deaths',
       title='Reported coronavirus deaths, selected countries',
       subtitle=death_chart_subtitle(min_death_cases),
       caption=jhu_credit(last_date))

selected_death_plot = selected_death_base_plot +
  scale_y_continuous(labels=scales::comma)

selected_death_log_plot = selected_death_base_plot +
  scale_y_log10(labels=scales::comma)
