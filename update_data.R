# Get data from Johns Hopkins CSSE GitHub repo
library(curl)

confirmed_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

deaths_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'

curl_download(confirmed_url, 
              here::here('data/time_series_covid19_confirmed_global.csv'))
curl_download(deaths_url, 
              here::here('data/time_series_covid19_deaths_global.csv'))

# JHU does not have state-level time series ATM. 
# This is an alternate.
confirmed_us_url = 'https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv'

deaths_us_url = 'https://static.usafacts.org/public/data/covid-19/covid_deaths_usafacts.csv'

curl_download(confirmed_us_url, 
              here::here('data/covid_confirmed_usafacts.csv'))
curl_download(deaths_us_url, 
              here::here('data/covid_deaths_usafacts.csv'))
