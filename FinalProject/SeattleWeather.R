
library(tidyverse)
library(knitr)
library(lubridate)
library(rvest)
library(stringr) 
library(rebus)
#library(RCurl)


AQIdata <- read_csv("AQI_KING.csv")
colnames((AQIdata))

SeattleWeather <- read_csv("SeattleWeather_1948_2017.csv")

str(SeattleWeather)

PerfectWeather <- inner_join(SeattleWeather, AQIdata, by = c("DATE" = "Date"))

SeattleWeather_noRain <- SeattleWeather %>%
    filter(PRCP < 0.05)

count(SeattleWeather_noRain)

SeattleWeather$DATE <- mdy(SeattleWeather$DATE)

######
library(lubridate)
library(chron)
require(devtools)
install_github("Displayr/flipTime")
library(flipTime)

baseurl <- 'https://www.events12.com/seattle/'
months <- c('january','february','march','april','may','june',
            'july','august','september','october','november','december')
dates <- c()

for( month in months ) {
    print(month)
    url <- paste0(baseurl,month,'/')
    events_data <- read_html(url)
    datestext <- events_data %>% html_nodes(".date") %>% html_text()
    dates <- c(dates, datestext)
}

ds <- c()
for( datetxt in dates ) {
    date <- AsDate(datetxt, on.parse.failure="warn")
    print(date)
    ds <- c(ds,as_date(date))
}

dates_df = as.data.frame(ds)
dates_df %<>% transmute(date = as_date(ds)) %>% distinct() %>%
    filter(!is.na(date)) %>%
    filter(year(date) > 2017 & year(date) < 2020) %>%
    mutate(month = month(date), day = day(date), year = 2017 ) %>%
    mutate(date = mdy( paste0(month,"-",day,"-",year) ) ) 

