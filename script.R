# Fusion des bases de données 

library(tidyverse)
library(dplyr)

#weekends
amsterdam_weekends <- read.csv("data/amsterdam_weekends.csv", stringsAsFactors=TRUE)
amsterdam_weekends$pays <- "Pays-Bas"
athens_weekends <- read.csv("data/athens_weekends.csv", stringsAsFactors=TRUE)
athens_weekends$pays <- "Grece"
barcelona_weekends <- read.csv("data/barcelona_weekends.csv",stringsAsFactors=TRUE)
barcelona_weekends$pays <- "Espagne"
berlin_weekends <- read.csv("data/berlin_weekends.csv", stringsAsFactors=TRUE)
berlin_weekends$pays <- "Allemagne"
budapest_weekends <- read.csv("data/budapest_weekends.csv", stringsAsFactors=TRUE)
budapest_weekends$pays <- "Hongrie"
lisbon_weekends <- read.csv("data/lisbon_weekends.csv", stringsAsFactors=TRUE)
lisbon_weekends$pays <- "Portugal"
paris_weekends <- read.csv("data/paris_weekends.csv", stringsAsFactors=TRUE)
paris_weekends$pays <- "France"
london_weekends <- read.csv("data/london_weekends.csv", stringsAsFactors=TRUE)
london_weekends$pays <- "Royaume-Uni"
rome_weekends <- read.csv("data/rome_weekends.csv", stringsAsFactors=TRUE)
rome_weekends$pays <- "Italie"
vienna_weekends <- read.csv("data/vienna_weekends.csv", stringsAsFactors=TRUE)
vienna_weekends$pays <- "Autriche"

weekends <- rbind(amsterdam_weekends,athens_weekends,barcelona_weekends,budapest_weekends,berlin_weekends,lisbon_weekends, paris_weekends,london_weekends,rome_weekends,vienna_weekends)

#weekdays

amsterdam_weekdays <- read.csv("data/amsterdam_weekdays.csv", stringsAsFactors=TRUE)
amsterdam_weekdays$pays <- "Pays-Bas"
athens_weekdays <- read.csv("data/athens_weekdays.csv", stringsAsFactors=TRUE)
athens_weekdays$pays <- "Grece"
barcelona_weekdays <- read.csv("data/barcelona_weekdays.csv",stringsAsFactors=TRUE)
barcelona_weekdays$pays <- "Espagne"
berlin_weekdays <- read.csv("data/berlin_weekdays.csv", stringsAsFactors=TRUE)
berlin_weekdays$pays <- "Allemagne"
budapest_weekdays <- read.csv("data/budapest_weekdays.csv", stringsAsFactors=TRUE)
budapest_weekdays$pays <- "Hongrie"
lisbon_weekdays <- read.csv("data/lisbon_weekdays.csv", stringsAsFactors=TRUE)
lisbon_weekdays$pays <- "Portugal"
paris_weekdays <- read.csv("data/paris_weekdays.csv", stringsAsFactors=TRUE)
paris_weekdays$pays <- "France"
london_weekdays <- read.csv("data/london_weekdays.csv", stringsAsFactors=TRUE)
london_weekdays$pays <- "Royaume-Uni"
rome_weekdays <- read.csv("data/rome_weekdays.csv", stringsAsFactors=TRUE)
rome_weekdays$pays <- "Italie"
vienna_weekdays <- read.csv("data/vienna_weekdays.csv", stringsAsFactors=TRUE)
vienna_weekdays$pays <- "Autriche"

weekdays <- rbind(amsterdam_weekdays,athens_weekdays,barcelona_weekdays,budapest_weekdays,berlin_weekdays,lisbon_weekdays, paris_weekdays,london_weekdays,rome_weekdays,vienna_weekdays)
weekdays <- data.frame(weekdays)
weekends <- data.frame(weekends)
data <- merge(weekdays, weekends[,c("realSum",'lng', 'lat')], by = c("lng", "lat"))
data <-  data[,-3]
names(data)[c(3,21)] <- c("prix_semaine","prix_weekend")

write.csv(data, file = "data/data_R_bnb.csv", fileEncoding = "utf-8")

