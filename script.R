# Fusion des bases de données 

library(tidyverse)
library(dplyr)

#weekends
amsterdam_weekends <- read.csv("data/amsterdam_weekends.csv", stringsAsFactors=TRUE)
athens_weekends <- read.csv("data/athens_weekends.csv", stringsAsFactors=TRUE)
barcelona_weekends <- read.csv("data/barcelona_weekends.csv",stringsAsFactors=TRUE)
berlin_weekends <- read.csv("data/berlin_weekends.csv", stringsAsFactors=TRUE)
budapest_weekends <- read.csv("data/budapest_weekends.csv", stringsAsFactors=TRUE)
lisbon_weekends <- read.csv("data/lisbon_weekends.csv", stringsAsFactors=TRUE)
paris_weekends <- read.csv("data/paris_weekends.csv", stringsAsFactors=TRUE)
london_weekends <- read.csv("data/london_weekends.csv", stringsAsFactors=TRUE)
rome_weekends <- read.csv("data/rome_weekends.csv", stringsAsFactors=TRUE)
vienna_weekends <- read.csv("data/vienna_weekends.csv", stringsAsFactors=TRUE)

weekends <- rbind(amsterdam_weekends,athens_weekends,barcelona_weekends,budapest_weekends,berlin_weekends,lisbon_weekends, paris_weekends,london_weekends,rome_weekends,vienna_weekends)

#weekdays

amsterdam_weekdays <- read.csv("data/amsterdam_weekdays.csv", stringsAsFactors=TRUE)
athens_weekdays <- read.csv("data/athens_weekdays.csv", stringsAsFactors=TRUE)
barcelona_weekdays <- read.csv("data/barcelona_weekdays.csv",stringsAsFactors=TRUE)
berlin_weekdays <- read.csv("data/berlin_weekdays.csv", stringsAsFactors=TRUE)
budapest_weekdays <- read.csv("data/budapest_weekdays.csv", stringsAsFactors=TRUE)
lisbon_weekdays <- read.csv("data/lisbon_weekdays.csv", stringsAsFactors=TRUE)
paris_weekdays <- read.csv("data/paris_weekdays.csv", stringsAsFactors=TRUE)
london_weekdays <- read.csv("data/london_weekdays.csv", stringsAsFactors=TRUE)
rome_weekdays <- read.csv("data/rome_weekdays.csv", stringsAsFactors=TRUE)
vienna_weekdays <- read.csv("data/vienna_weekdays.csv", stringsAsFactors=TRUE)

weekdays <- rbind(amsterdam_weekdays,athens_weekdays,barcelona_weekdays,budapest_weekdays,berlin_weekdays,lisbon_weekdays, paris_weekdays,london_weekdays,rome_weekdays,vienna_weekdays)

data <- bind_rows(weekdays=weekdays, weekends=weekends, .id= "period")
write.csv(data, file = "data/data_R_bnb.csv")
