# setwd("E:/")
cal <- readRDS("calferien.rds")
head(cal)

fuel <- read.xlsx("E:/KIVAS/Daten/Kraftstoffpreise Cent je Liter 2015-2018.xlsx",
                         colNames=T)
fuel1 <- fuel
sapply(fuel1, class)
fuel1$Super.E10 <- as.numeric(fuel$Super.E10)
# Warning message:
#   NAs introduced by coercion 
# ---  why?
fuel1$Super.E10[which(is.na(fuel1$Super.E10))]; fuel$Super.E10[which(is.na(fuel1$Super.E10))]
# --- wrong decimal
fuel$Super.E10[which(is.na(fuel1$Super.E10))] <- 135.3
fuel1$Super.E10 <- as.numeric(fuel$Super.E10) # call again

fuel1$Diesel <- as.numeric(fuel$Diesel)
# again NAs introduced by coercion
fuel1$Diesel[which(is.na(fuel1$Diesel))]; fuel$Diesel[which(is.na(fuel1$Diesel))]
fuel$Diesel[which(is.na(fuel1$Diesel))] <- 115.6
fuel1$Diesel <- as.numeric(fuel$Diesel) # call again

fuelyear <- which(fuel1$Jahr == 2015 | fuel1$Jahr == 2016)
fuel2 <- fuel1[fuelyear,]
fuel3 <- fuel2[c(which(fuel2$Jahr == 2015 & fuel2$Monat == c(9:12)), which(fuel2$Jahr == 2016)),]

cal$year <- year(cal$Date)
cal2 <- merge(cal, fuel3, by.x=c("year","Month"), by.y=c("Jahr","Monat"))
head(cal2)
saveRDS(cal2, file = "calferiensunfuel.rds")
