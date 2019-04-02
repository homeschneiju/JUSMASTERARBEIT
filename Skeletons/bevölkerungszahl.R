cal <- readRDS("calferienmeteofuelmonthzinsdbbipvpi.rds")

bev <- read.csv("E:/KIVAS/Daten/bev bayern mod.csv", header=T, sep=";")
bev

cal2 <- merge(cal, bev, by.x="year", by.y="Jahr", all.x=T)
head(cal2)
tail(cal2)

saveRDS(cal2, "calferienmeteofuelmonthzinsdbbipvpibev.rds")
