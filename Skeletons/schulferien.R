# setwd("E:/")
cal <- readRDS("cal.rds")
head(cal)

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

calferien <- cal
saveRDS(calferien, file = "calferien.rds")
