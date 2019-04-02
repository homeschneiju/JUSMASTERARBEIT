cal <- readRDS(cal2, "calferienmeteofuelmonthzinsdb.rds")

bip <- read.csv("E:/KIVAS/Daten/bip quartale 14-18 arima.csv", header=T, sep=";")
head(bip2)
bip2 <- bip[bip$Jahr == 2015 | bip$Jahr == 2016,]
range(bip2$Jahr)
bip3 <- bip2[(bip2$Jahr == 2015 & bip2$Quartal == c(3,4)) | (bip2$Jahr == 2016),]
range(bip3$Quartal[bip3$Jahr == 2015])

colnames(bip3)

bip4 <- bip3[c(1,2,3,14)]
bip4

cal2 <- merge(cal, bip4, by.x=c("year", "Quarter"), by.y=c("Jahr", "Quartal"))

colnames(cal2)
head(cal2)
saveRDS(cal2, "calferienmeteofuelmonthzinsdbbip.rds")
