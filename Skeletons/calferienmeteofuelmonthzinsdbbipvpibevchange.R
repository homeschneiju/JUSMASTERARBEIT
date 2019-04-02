cal <- readRDS("calferienmeteofuelmonthzinsdbbipvpibev.rds")

change <- read.xlsx("E:/KIVAS/Daten/wechselkurs.xlsx", colNames=T)
head(change)
class(change$Datum)

change2 <- change
change2$Datum <- as.integer(change2$Datum)
change2$Datum <- as.Date(change2$Datum, origin="1899-12-30")
head(change2)

cal2 <- merge(cal, change2, by.x="Date", by.y="Datum")
head(cal2)

saveRDS(cal2, "calfinal.rds")
