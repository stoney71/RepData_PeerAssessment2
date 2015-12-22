setwd("~/RFolder/RepData_PeerAssessment2")
dataset <- read.csv("./repdata_data_StormData.csv.bz2")

library(dplyr)
library(ggplot2)
library(datasets)
library(lattice)

## Clean the data.
## First convert the BGN_DATE into a meaningful value.

dataset$BGN_DATE <- as.Date(dataset$BGN_DATE, "%m/%d/%Y")


## Next, review how many events recorded per year.

hist(dataset$BGN_DATE, breaks = 65)

dataset <- filter(dataset, BGN_DATE > "1990-01-01")

## Part 1: Effect on population health

healthds <- select(dataset, BGN_DATE, EVTYPE, FATALITIES, INJURIES, REMARKS) %>% 
        filter( (FATALITIES != 0) | (INJURIES != 0))

healthds <- healthds %>% group_by(EVTYPE) %>% 
        summarise(tfat = sum(FATALITIES), tinj = sum(INJURIES)) %>% 
        arrange(desc(tfat), desc(tinj))

all_fatalities <- sum(healthds$tfat)
all_injuries <- sum(healthds$tinj)


healthdssum <- head(healthds, n = 20)

healthds$EVTYPE <- gsub(".*HEAT.*", "EXCESSIVE HEAT", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*RIP CURRENT.*", "RIP CURRENT", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*THUNDER.*", "THUNDERSTORM WIND", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*TSTM.*", "THUNDERSTORM WIND", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*EXTREME COLD.*", "EXTREME COLD/WIND CHILL", healthds$EVTYPE, ignore.case = TRUE)

healthds <- healthds %>% group_by(EVTYPE) %>% 
        summarise(tfat = sum(tfat), tinj = sum(tinj)) 

top5fat <- healthds %>% top_n(5, tfat) %>% select(-tinj) %>% arrange(desc(tfat))
top5inj <- healthds %>% top_n(5, tinj) %>% select(-tfat) %>% arrange(desc(tinj))

library(lattice)
library(datasets)
barplot(top5fat$tfat, names = top5fat$EVTYPE)
barplot(top5inj$tinj, names = top5fat$EVTYPE)


## Part 1a: Repeat for dataset2.

healthds2 <- select(dataset2, BGN_DATE, EVTYPE, FATALITIES, INJURIES, REMARKS)

healthds2 <- filter(healthds2, (FATALITIES != 0) | (INJURIES != 0))

healthds2 <- healthds2 %>% group_by(EVTYPE) %>% 
        summarise(totf = sum(FATALITIES), toti = sum(INJURIES)) %>% 
        arrange(desc(totf), desc(toti))

sum(healthds2$totf)
sum(healthds2$toti)

healthds2 <- head(healthds2, n = 20)

qplot(EVTYPE, totf, data = healthds2)


grep("HEAT", healthds2$EVTYPE, ignore.case = TRUE)


## Part 2: Effect on Economic

econds <- select(dataset, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS)

econds <- filter(econds, (PROPDMG != 0) | (CROPDMG != 0))

econds$PROPDMGEXP <- as.character(econds$PROPDMGEXP)
econds$CROPDMGEXP <- as.character(econds$CROPDMGEXP)

myeconds <- econds %>% mutate(PROPMULT = ifelse(PROPDMGEXP == "H" | PROPDMGEXP == "h", 100, 
                ifelse(PROPDMGEXP == "K" | PROPDMGEXP == "k", 1000, 
                ifelse(PROPDMGEXP == "M" | PROPDMGEXP == "m", 1000000,
                ifelse(PROPDMGEXP == "B" | PROPDMGEXP == "b", 1000000000,
                ifelse(PROPDMGEXP == "0", 0,
                1)))))) %>%
                mutate(CROPMULT = ifelse(CROPDMGEXP == "H" | CROPDMGEXP == "h", 100, 
                ifelse(CROPDMGEXP == "K" | CROPDMGEXP == "k", 1000, 
                ifelse(CROPDMGEXP == "M" | CROPDMGEXP == "m", 1000000,
                ifelse(CROPDMGEXP == "B" | CROPDMGEXP == "b", 1000000000,
                ifelse(CROPDMGEXP == "0", 0,
                1)))))) %>%
                mutate (TOTPROPDMG = PROPDMG * PROPMULT, TOTCROPDMG = CROPDMG * CROPMULT) %>%
                select(-PROPMULT, -CROPMULT) %>%
                arrange(desc(TOTPROPDMG))





