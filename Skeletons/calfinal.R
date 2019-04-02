# setwd("E:/")
cal <- readRDS("cal.rds")

cal$msum <- days_in_month(cal$Date)

schulferien <- read.xlsx("E:/KIVAS/Daten/schulferien bayern.xlsx",
                         colNames=T)
schulferien$Start <- as.Date(schulferien$Start,origin="1899-12-30")
schulferien$Stop <- as.Date(schulferien$Stop,origin="1899-12-30")
int <- interval(ymd(schulferien$Start),ymd(schulferien$Stop))

cal$ferien <- 0
for (i in 1:nrow(cal)){
  for(j in 1:length(int)){
    if (cal$Date[[i]] %within% int[j])
      cal$ferien[i] <- 1
  }
}

sunshine <- read.csv("E:/KIVAS/Daten/produkt_klima_tag_19490101_20181231_03668.csv",
                     header=T, sep=";",na.strings = c("-999"))
sunshine1 <- sunshine[,c("MESS_DATUM","SDK", "RSK", "RSKF", "UPM")]
sunshine1$MESS_DATUM <- ymd(sunshine$MESS_DATUM)
sunshinestart <- which(sunshine1$MESS_DATUM == "2015-09-17")
sunshinestop <- which(sunshine1$MESS_DATUM == "2016-12-31")
int <- sunshinestart:sunshinestop
sunshine2 <- sunshine1[int,] # only store sunshine duration for time frame of our data


cal <- merge(cal, sunshine2, by.x="Date", by.y="MESS_DATUM")

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
cal <- merge(cal, fuel3, by.x=c("year","Month"), by.y=c("Jahr","Monat"))

leitzins1 <- read.csv("E:/KIVAS/Daten/key interest rates mod.csv",
                      header=F, sep=",")
leitzins2 <- read.csv("E:/KIVAS/Daten/key interest rates 2 mod.csv",
                      header=F, sep=",")
leitzins3 <- read.csv("E:/KIVAS/Daten/key interest rates 3 mod.csv",
                      header=F, sep=",")

leitzins <- merge(leitzins1, leitzins2, by="V1")
leitzins <- merge(leitzins, leitzins3, by="V1")
head(leitzins[leitzins$V1 == "2015-01-01",])
head(leitzins1[leitzins1$V1 == "2015-01-01",])
head(leitzins2[leitzins2$V1 == "2015-01-01",])
head(leitzins3[leitzins3$V1 == "2015-01-01",])

colnames(leitzins) <- c("Date", "leitzins1", "leitzins2", "leitzins3")

leitzinsstart <- which(leitzins$Date == "2015-09-17")
leitzinsstop <- which(leitzins$Date == "2016-12-31")
int <- leitzinsstart:leitzinsstop
leitzinsshort <- leitzins[int,]
leitzinsshort$Date <- as.Date(leitzinsshort$Date)
length(cal$Date); length(leitzinsshort$Date)

max(leitzinsshort$Date)

tail(leitzinsshort)
tail(cal)
cal <- merge(cal, leitzinsshort, by="Date")

db <- read.csv("E:/KIVAS/Daten/DB CARGO delays.csv",
               header=T, sep=";")
head(db)

db2 <- db[db$Land == "DEUTSCHLAND" | db$Land == "PZ Halle/Saale" | db$Land == "PZ Mannheim",]
db2$Date <- as.Date(db2$PROD_DATUM, "%d.%m.%Y")
head(db2)
db2 <- db2[,c(1,4,5,6)]
head(db2)
db3 <- aggregate(db2[,c(2,3)], by=list(db2$"Date"), sum)


head(db3)

db3$Zugfahrten[db3$Group.1 == "2016-01-15"]
sum(db2$Zugfahrten[db2$Date == "2016-01-15"])
colnames(db3) <- c("Date", "zugfahrten", "Verspätungsminuten")
db3$meandelay <- round(db3$Verspätungsminuten/db3$zugfahrten, 2)

min(db3$Date)
max(db3$Date)

cal <- merge(cal, db3, by="Date", all.x=T)

bip <- read.csv("E:/KIVAS/Daten/bip quartale 14-18 arima.csv", header=T, sep=";")
head(bip2)
bip2 <- bip[bip$Jahr == 2015 | bip$Jahr == 2016,]
range(bip2$Jahr)
bip3 <- bip2[(bip2$Jahr == 2015 & bip2$Quartal == c(3,4)) | (bip2$Jahr == 2016),]
range(bip3$Quartal[bip3$Jahr == 2015])

colnames(bip3)

bip4 <- bip3[c(1,2,3,14)]
bip4

cal <- merge(cal, bip4, by.x=c("year", "Quarter"), by.y=c("Jahr", "Quartal"))

verpreisby <- read.csv("E:/KIVAS/Daten/verbraucherpreisindex bayern mod.csv", header=F, ,sep=";" )
verpreisnahr <- read.csv("E:/KIVAS/Daten/verbraucherpreisindex nahrung mod.csv", header=F, sep=";")
tail(verpreisby)
tail(verpreisnahr)
verpreisby <- verpreisby[c(1:24),]
verpreisby$V2 <- rep(c(1:12),2)

colnames(verpreisby) <- c("year", "Month", "VPIby")
colnames(verpreisnahr) <- c("year", "Month", "VPInahr")

cal <- merge(cal, verpreisby, by=c("year", "Month"))
head(cal)
any(is.na(cal$VPIby))

cal <- merge(cal, verpreisnahr, by=c("year", "Month"))
head(cal)
any(is.na(cal$VPIby))

bev <- read.csv("E:/KIVAS/Daten/bev bayern mod.csv", header=T, sep=";")
bev

cal <- merge(cal, bev, by.x="year", by.y="Jahr", all.x=T)
head(cal)
tail(cal)

change <- read.xlsx("E:/KIVAS/Daten/wechselkurs.xlsx", colNames=T)
head(change)
class(change$Datum)

change2 <- change
change2$Datum <- as.integer(change2$Datum)
change2$Datum <- as.Date(change2$Datum, origin="1899-12-30")
head(change2)

cal <- merge(cal, change2, by.x="Date", by.y="Datum")
head(cal)

saveRDS(cal, "E:/KIVAS/Daten/calfinal.rds")

