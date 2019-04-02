cal <- readRDS("calferienmeteofuelmonth.rds")
head(cal)
leitzins1 <- read.csv("C:/Users/Juliana Schneider/Documents/KIVAS/Daten/key interest rates mod.csv",
                         header=F, sep=",")
leitzins2 <- read.csv("C:/Users/Juliana Schneider/Documents/KIVAS/Daten/key interest rates 2 mod.csv",
                      header=F, sep=",")
leitzins3 <- read.csv("C:/Users/Juliana Schneider/Documents/KIVAS/Daten/key interest rates 3 mod.csv",
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
cal2 <- merge(cal, leitzinsshort, by="Date")
head(cal2)
saveRDS(cal2, file = "calferienmeteofuelmonthzins.rds")
