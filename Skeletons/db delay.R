cal <- readRDS("calferienmeteofuelmonthzins.rds")
colnames(cal)
length(colnames(cal))
head(cal)

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

cal2 <- merge(cal, db3, by="Date", all.x=T)
head(cal2)

saveRDS(cal2, "calferienmeteofuelmonthzinsdb.rds")
