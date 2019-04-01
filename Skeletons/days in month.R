cal <- readRDS("calferiensunfuel.rds")

cal$msum <- days_in_month(cal$Date)

