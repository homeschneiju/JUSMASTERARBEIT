# setwd("E:/")
cal <- readRDS("calferien.rds")
head(cal)

sunshine <- read.csv("E:/KIVAS/Daten/produkt_klima_tag_19490101_20181231_03668.csv",
                         header=T, sep=";",na.strings = c("-999"))
sunshine1 <- sunshine[,c("MESS_DATUM","SDK")]
sunshine1$MESS_DATUM <- ymd(sunshine$MESS_DATUM)
sunshinestart <- which(sunshine1$MESS_DATUM == "2015-09-17")
sunshinestop <- which(sunshine1$MESS_DATUM == "2016-12-31")
int <- sunshinestart:sunshinestop
sunshine2 <- sunshine1[int,] # only store sunshine duration for time frame of our data


cal <- merge(cal, sunshine2, by.x="Date", by.y="MESS_DATUM")

saveRDS(cal, file = "calferiensun.rds")
