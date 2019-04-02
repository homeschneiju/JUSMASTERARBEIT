cal <- readRDS("calferienmeteofuel.rds")

cal$msum <- days_in_month(cal$Date)
saveRDS(cal, file = "calferienmeteofuelmonth.rds")

