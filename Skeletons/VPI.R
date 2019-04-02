cal <- readRDS("calferienmeteofuelmonthzinsdbbip.rds")

verpreisby <- read.csv("E:/KIVAS/Daten/verbraucherpreisindex bayern mod.csv", header=F, ,sep=";" )
verpreisnahr <- read.csv("E:/KIVAS/Daten/verbraucherpreisindex nahrung mod.csv", header=F, sep=";")
tail(verpreisby)
tail(verpreisnahr)
verpreisby <- verpreisby[c(1:24),]
verpreisby$V2 <- rep(c(1:12),2)

colnames(verpreisby) <- c("year", "Month", "VPIby")
colnames(verpreisnahr) <- c("year", "Month", "VPInahr")

cal2 <- merge(cal, verpreisby, by=c("year", "Month"))
head(cal2)
any(is.na(cal2$VPIby))

cal3 <- merge(cal2, verpreisnahr, by=c("year", "Month"))
head(cal3)
any(is.na(cal3$VPIby))
saveRDS(cal3,"calferienmeteofuelmonthzinsdbbipvpi.rds")
