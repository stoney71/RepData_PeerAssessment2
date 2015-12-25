setwd("~/RFolder/RepData_PeerAssessment2")
dataset <- read.csv("./repdata_data_StormData.csv.bz2")

library(dplyr)
library(ggplot2)
library(graphics)
library(lattice)

## Next, review how many events recorded per year.

dataset$BGN_DATE <- as.Date(dataset$BGN_DATE, "%m/%d/%Y")
hist(dataset$BGN_DATE, breaks = 65)

dataset <- filter(dataset, BGN_DATE > "1990-01-01")

## Part 1: Effect on population health

healthds <- select(dataset, BGN_DATE, EVTYPE, FATALITIES, INJURIES, REMARKS) %>% 
        filter( (FATALITIES != 0) | (INJURIES != 0))

healthds <- healthds %>% group_by(EVTYPE) %>% 
        summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) %>% 
        arrange(desc(FATALITIES), desc(INJURIES))

total_fatalities <- sum(healthds$FATALITIES)
total_injuries <- sum(healthds$INJURIES)

head(healthds, n = 20)

healthds$EVTYPE <- gsub(".*HEAT.*", "EXCESSIVE HEAT", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*RIP CURRENT.*", "RIP CURRENT", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*THUNDER.*", "THUNDERSTORM WIND", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*TSTM.*", "THUNDERSTORM WIND", healthds$EVTYPE, ignore.case = TRUE)
healthds$EVTYPE <- gsub(".*EXTREME COLD.*", "EXTREME COLD/WIND CHILL", healthds$EVTYPE, ignore.case = TRUE)

healthds <- healthds %>% group_by(EVTYPE) %>% 
        summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES)) 

top10fat <- healthds %>% top_n(10, FATALITIES) %>% select(-INJURIES) %>% arrange((FATALITIES))
top10inj <- healthds %>% top_n(10, INJURIES) %>% select(-FATALITIES) %>% arrange((INJURIES))

par(las=2)
par(mar=c(5,10,4,2))
barplot(top10fat$FATALITIES, names = top10fat$EVTYPE, horiz = TRUE)
barplot(top10inj$INJURIES, names = top10fat$EVTYPE, horiz = TRUE)


## Part 2: Effect on Economic

econds <- select(dataset, BGN_DATE, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, REMARKS)

econds <- filter(econds, (PROPDMG != 0) | (CROPDMG != 0))

econds$PROPDMGEXP <- as.character(econds$PROPDMGEXP)
econds$CROPDMGEXP <- as.character(econds$CROPDMGEXP)

econds <- econds %>% mutate(PROPMULT = ifelse(PROPDMGEXP == "H" | PROPDMGEXP == "h", 100, 
                ifelse(PROPDMGEXP == "K" | PROPDMGEXP == "k", 1000, 
                ifelse(PROPDMGEXP == "M" | PROPDMGEXP == "m", 1000000,
                ifelse(PROPDMGEXP == "B" | PROPDMGEXP == "b", 1000000000,
                1000))))) %>%
                mutate(CROPMULT = ifelse(CROPDMGEXP == "H" | CROPDMGEXP == "h", 100, 
                ifelse(CROPDMGEXP == "K" | CROPDMGEXP == "k", 1000, 
                ifelse(CROPDMGEXP == "M" | CROPDMGEXP == "m", 1000000,
                ifelse(CROPDMGEXP == "B" | CROPDMGEXP == "b", 1000000000,
                1000))))) %>%
                mutate (TOTPROPDMG = PROPDMG * PROPMULT, TOTCROPDMG = CROPDMG * CROPMULT) %>%
                arrange(desc(TOTPROPDMG))

econds$TOTPROPDMG[1] <- econds$PROPDMG[1] * 1000000

econds <- select(econds, BGN_DATE, EVTYPE, TOTPROPDMG, TOTCROPDMG)

econds$EVTYPE <- gsub(".*HEAT.*", "EXCESSIVE HEAT", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*RIP CURRENT.*", "RIP CURRENT", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*THUNDER.*", "THUNDERSTORM WIND", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*TSTM.*", "THUNDERSTORM WIND", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*EXTREME COLD.*", "EXTREME COLD/WIND CHILL", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*FIRE.*", "WILDFIRE", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*HURRICANE.*", "HURRICANE/TYPHOON", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*TYPHOON.*", "HURRICANE/TYPHOON", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*RIVER FLOOD.*", "FLOOD", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*FLASH FLOOD.*", "FLASH FLOOD", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*STORM SURGE.*", "STORM SURGE", econds$EVTYPE, ignore.case = TRUE)
econds$EVTYPE <- gsub(".*FREEZE.*", "FROST/FREEZE", econds$EVTYPE, ignore.case = TRUE)

econds <- econds %>% group_by(EVTYPE) %>% summarise(TOTPROPDMG = sum(TOTPROPDMG), TOTCROPDMG = sum(TOTCROPDMG)) %>%
        arrange(desc(TOTPROPDMG), desc(TOTCROPDMG))


head(econds, n=20)

top10prop <- econds %>% top_n(10, TOTPROPDMG) %>% select(-TOTCROPDMG) %>% arrange((TOTPROPDMG))
top10crop <- econds %>% top_n(10, TOTCROPDMG) %>% select(-TOTPROPDMG) %>% arrange((TOTCROPDMG))

par(las=2)
par(mar=c(5,10,4,2))
barplot(top10prop$TOTPROPDMG, names = top10prop$EVTYPE, horiz = FALSE)
barplot(top10crop$TOTCROPDMG, names = top10crop$EVTYPE, horiz = FALSE)



