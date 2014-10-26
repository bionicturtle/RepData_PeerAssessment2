
# load datafile and simplify by eliminating un-necessary columns

Sys.setlocale("LC_ALL","English")
stormDataRaw <- read.csv("repdata_data_StormData.csv.bz2")
stormData <- stormDataRaw[, c("BGN_DATE", "BGN_TIME", "TIME_ZONE" , "COUNTY" , 
                            "COUNTYNAME" , "STATE", "EVTYPE", "FATALITIES", 
                            "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", 
                            "CROPDMGEXP", "WFO" , "STATEOFFIC", "ZONENAMES", 
                            "LATITUDE", "LONGITUDE", "REFNUM")]

write.csv(stormData, "stormData.csv")



# getting the smaller file, which is only ~ 165 MB compared to 549 MB for the original, unpacked
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
sdata <- read.csv("stormData.csv", stringsAsFactors = FALSE)
sdata$YEAR <- strptime(sdata$BGN_DATE, format = "%m/%d/%Y")
sdata$YEAR_N <- year(sdata$YEAR)

# Only need the events that produced human harm or economic damage
# This reduces from 902,297 rows to 254,633 rows
sdata.e <- sdata[(sdata$FATALITIES > 0) | (sdata$INJURIES > 0) | 
                     (sdata$PROPDMG > 0) | (sdata$CROPDMG >0 ), ]

# Mapping event types to "Major Events" 
# EV_MAJOR has 43 Factor levels; i.e., 43 major events
sdata.e$EVTYPE <- toupper(sdata.e$EVTYPE)
sdata.e$EV_MAJOR <- NULL
s.char <- c("ASTRONOMICAL", "AVALANCHE", "BLIZZARD", "BURST", "COLD", "DRIZZLE", "DROUGHT", "DUST", "EROSION", "EXPOSURE", "FIRE",
            "FLOOD", "FOG", "FREEZE", "FROST", "GLAZE", "HAIL", "HEAT", "HURRICANE", "ICE", "LANDSLIDE",
            "LIGHTNING", "MIX", "MUD", "OTHER", "RAIN", "RIP", "ROCK", "SHOWER", "SLEET", "SMOKE", "SNOW",
            "STORM", "SURF", "TIDE", "TORNADO", "TROPICAL", "TSUNAMI", "TYPHOON", "SEAS", "URBAN", 
            "VOLCANIC", "WATER", "WIND", "WINTER")

for (x in s.char) {
    sdata.e$EV_MAJOR[grepl(x,sdata.e$EVTYPE)] <- x
}

sdata.e$EV_MAJOR[grepl("ICY ROADS",sdata.e$EVTYPE)] <- "ICE"
sdata.e$EV_MAJOR[grepl("HEAVY SWELLS|HIGH SWELLS|ROGUE WAVE|SEICHE|HIGH WAVES|COASTAL SURGE|MARINE MISHAP|MARINE ACCIDENT",sdata.e$EVTYPE)] <- "SEAS"
sdata.e$EV_MAJOR[grepl("FREEZING SPRAY",sdata.e$EVTYPE)] <- "FREEZE"
sdata.e$EV_MAJOR[grepl("WARM WEATHER",sdata.e$EVTYPE)] <- "HEAT"
sdata.e$EV_MAJOR[grepl("FUNNEL CLOUD|GUSTNADO|TORNDAO",sdata.e$EVTYPE)] <- "TORNADO"
sdata.e$EV_MAJOR[grepl("WET|DROWNING|DAM BREAK",sdata.e$EVTYPE)] <- "WATER"
sdata.e$EV_MAJOR[grepl("LOW TEMPERATURE",sdata.e$EVTYPE)] <- "COLD"
sdata.e$EV_MAJOR[grepl("LAND",sdata.e$EVTYPE)] <- "ROCK"
sdata.e$EV_MAJOR[grepl("HEAVY PRECIPITATION",sdata.e$EVTYPE)] <- "RAIN"
sdata.e$EV_MAJOR[grepl("AVALANCE",sdata.e$EVTYPE)] <- "AVALANCHE"
sdata.e$EV_MAJOR[grepl("LIGNTNING|LIGHTING",sdata.e$EVTYPE)] <- "LIGHTNING"
sdata.e$EV_MAJOR[grepl("TSTMW|HIGH|APACHE COUNTY|SEVERE TURBULENCE",sdata.e$EVTYPE)] <- "OTHER"
sdata.e$EV_MAJOR[is.na(sdata.e$EV_MAJOR)] <- "OTHER"

sdata.e$EV_MAJOR <- as.factor(sdata.e$EV_MAJOR)

# creating accurate exponent for damage values
sdata.e$PROPDMGEXP_N <- NULL
sdata.e$CROPDMGEXP_N <- NULL
sdata.e$PROPDMGEXP_N <- as.numeric(sdata.e$PROPDMGEXP)
sdata.e$CROPDMGEXP_N <- as.numeric(sdata.e$CROPDMGEXP)
sdata.e$PROPDMGEXP_N[sdata.e$PROPDMGEXP == "H" | sdata.e$PROPDMGEXP == "h"] <- 10^2
sdata.e$PROPDMGEXP_N[sdata.e$PROPDMGEXP == "3" | sdata.e$PROPDMGEXP == "K" | sdata.e$PROPDMGEXP == "k"] <- 10^3
sdata.e$PROPDMGEXP_N[sdata.e$PROPDMGEXP == "M" | sdata.e$PROPDMGEXP == "M"] <- 10^6
sdata.e$PROPDMGEXP_N[sdata.e$PROPDMGEXP == "B" | sdata.e$PROPDMGEXP == "b"] <- 10^9
sdata.e$PROPDMGEXP_N[sdata.e$PROPDMGEXP == "-" | sdata.e$PROPDMGEXP == "+" |sdata.e$PROPDMGEXP == "0"] <- 1
sdata.e$PROPDMGEXP_N[is.na(sdata.e$PROPDMGEXP_N)] <- 1

sdata.e$CROPDMGEXP_N[sdata.e$CROPDMGEXP == "k" | sdata.e$CROPDMGEXP == "K"] <- 10^3
sdata.e$CROPDMGEXP_N[sdata.e$CROPDMGEXP == "m" | sdata.e$CROPDMGEXP == "M"] <- 10^6
sdata.e$CROPDMGEXP_N[sdata.e$CROPDMGEXP == "B" ] <- 10^9
sdata.e$CROPDMGEXP_N[sdata.e$CROPDMGEXP == "0" ] <- 1
sdata.e$CROPDMGEXP_N[is.na(sdata.e$CROPDMGEXP_N)] <- 1

# library(Quandl)
# Quandl.auth("G5xW3q6kgbALQ8L2srWD")
# inflation <- Quandl("SGE/USACPIC", collapse="annual")
# write.csv(inflation, "inflation.csv")
cpi_index <- read.csv("cpiindex.csv")
sdata.i <- merge(sdata.e, cpi_index, by.x = "YEAR_N", by.y = "year", all.x = TRUE)

# inflated damagaes
sdata.i$PDAMAGE <- NULL
sdata.i$CDAMAGE <- NULL
sdata.i$PDAMAGE <- sdata.i$PROPDMG * sdata.i$PROPDMGEXP_N
sdata.i$CDAMAGE <- sdata.i$CROPDMG * sdata.i$CROPDMGEXP_N
sdata.i$PDAMAGE.i <- sdata.i$PROPDMG * sdata.i$PROPDMGEXP_N * sdata.i$index
sdata.i$CDAMAGE.i <- sdata.i$CROPDMG * sdata.i$CROPDMGEXP_N * sdata.i$index

# Time series: frequency of relevant reports
series.freq <- table(sdata.i$YEAR_N)
series.freq.df <- data.frame(series.freq)
labels <- seq(1950, 2011, by=10)
ggplot(series.freq.df, aes(x=Var1, y=Freq)) + 
    geom_bar(stat="identity") +
    ylab("Frequency of events during year") +
    xlab("") +
    scale_x_discrete(breaks=labels, labels=as.character(labels)) +
    theme(axis.text.x = element_text(angle=90, size=rel(2.0))) +
    ggtitle ("Ramp-up in reporting frequency")
    
# harmful user-defined statistics
sdata.i$health <- sdata.i$INJURIES + (sdata.i$FATALITIES * 10)
sdata.i$economics <- sdata.i$PDAMAGE + sdata.i$CDAMAGE
sdata.i$economics.i <- sdata.i$PDAMAGE.i + sdata.i$CDAMAGE.i

# inflation
economic.by.year <- aggregate(sdata.i["economics"], by=sdata.i["YEAR_N"], FUN="sum", na.rm = TRUE)
economic.by.year.1 <- aggregate(sdata.i["economics.i"], by=sdata.i["YEAR_N"], FUN="sum", na.rm = TRUE)
df.economic <- data.frame(economic.by.year, economic.by.year.1)
colnames(df.economic) <- c("Year", "Raw.damage", "Year.1", "Inflation.adjusted")
# write.csv(df.economic, "comparison_inf.csv") # for excel examination
ggplot(df.economic, aes(x = Year)) +
    geom_line(aes(y = Raw.damage, color="Raw")) +
    geom_line(aes(y = Inflation.adjusted, color="CPI Adjusted")) +
    scale_color_manual("",
                        breaks = c("Raw", "CPI Adjusted"),
                        values = c("blue", "red")) +
    ylab("Total Damages (Property + Cost)") +
    xlab("Year") +
    ggtitle ("Inflation impact overwhelmed by ramp-up in event frequency")


# aggregate
economic.bymajor <- aggregate(sdata.i["economics.i"], by=sdata.i["EV_MAJOR"], FUN="sum", na.rm = TRUE)
economic.bymajorL <- economic.bymajor[economic.bymajor$economics > 90000000,]
health.bymajor <- aggregate(sdata.i["health"], by=sdata.i["EV_MAJOR"], FUN="sum", na.rm = TRUE)
health.bymajorL <- health.bymajor[health.bymajor$health > 300,]

# plot
plot1 <- ggplot(economic.bymajorL, aes(x = economics.i, y = reorder(EV_MAJOR, economics.i))) +
    geom_point(size=4, color="red") +
    ylab("Major Event Category") +
    xlab("Inflation-adjusted Tot. Damage") +
    theme(axis.text.x = element_text(angle=90, size=rel(2.0)))
plot2 <- ggplot(health.bymajorL, aes(x = health, y = reorder(EV_MAJOR, health))) + 
    ylab("") +
    xlab("Human Harm Index") +
    geom_point(size=4, color="green") +
    theme(axis.text.x = element_text(angle=90, size=rel(2.0)))
grid.arrange(plot1, plot2, ncol=2)