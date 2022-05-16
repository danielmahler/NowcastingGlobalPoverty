"http://api.worldbank.org/en/Indicators/SP.POP.TOTL?date=&?format=csv"
library(httr) 
library(readr)

web <- "http://api.worldbank.org/en/countries/all/Indicators/SP.POP.TOTL?date=&format=csv"
a <- read_csv(web)
a
str(a)
content(a, as = "text")


GET("http://api.worldbank.org/en/countries/all/Indicators/SP.POP.TOTL")

GET("http://api.worldbank.org/en/countries/all/Indicators/{id}")
