# Get data from Johns Hopkins CSSE GitHub repo
library(curl)

confirmed_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv'

deaths_url = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv'

curl_download(confirmed_url, here::here('data/time_series_19-covid-Confirmed.csv'))
curl_download(deaths_url, here::here('data/time_series_19-covid-Deaths.csv'))

