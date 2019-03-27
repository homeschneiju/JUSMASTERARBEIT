#sa90 <- read.table("//netappn1/SCS/50_Abteilungen/52_Analytics/843_DataScienceOpt/Hiwi/Juliana Schneider/KIVAS/Daten/data_SA90_NeuroNet.csv",sep= ";",header=TRUE, dec=",")
#sa90 <- read.table("C:/Users/schneiju/Desktop/KIVAS/Daten/data_SA90_NeuroNet.csv",sep= ";",header=TRUE, dec=",")


setwd("E:/KIVAS/Daten")
data_SA_90_NeuroNet_raw <- read.csv2(paste(
  
  # If the data files are in a subdirectory of the R-files, run the next line.
  getwd()
  
  # If you need to navigate higher and into a different folder use the following
  # commented lines:
  
  # substr(getwd(), 1, nchar(getwd()) - Remove_Characters) 
  #,"Kundendaten"
  ,"data_SA90_NeuroNet.csv"
  , sep = "/")
  , header = T
  , check.names = F
  , sep = ";"
  , dec = ",")

data_SA_90_NeuroNet_pre <- data_SA_90_NeuroNet_raw


#data_SA_90_NeuroNet_pre <- sa90
data_SA_90_NeuroNet_pre[[1]] <- as.factor(data_SA_90_NeuroNet_pre[[1]])
data_SA_90_NeuroNet_pre[[4]] <- as.factor(data_SA_90_NeuroNet_pre[[4]])
data_SA_90_NeuroNet_pre[[29]] <- as.factor(data_SA_90_NeuroNet_pre[[29]])
data_names <- names(data_SA_90_NeuroNet_pre)
data_SA_90_NeuroNet_pre$Date <- as.Date(data_SA_90_NeuroNet_pre$LST_D, "%d.%m.%Y")
#data_2016 <- data_SA_90_NeuroNet_pre[data_SA_90_NeuroNet_pre$jahr == 2016, ]
#library(reshape2)
splitdate <- colsplit(data_SA_90_NeuroNet_pre$LST_D," ",names=c("date","time"))
#data_2016 <- cbind(data_2016, splitdate)

#library(lubridate)
cal <- data.frame(Date = seq(as.Date("2015-09-17"), as.Date("2016-12-31"), "day" ))
cal$Weekday <- weekdays(cal$Date)
cal$Weekday_No <- wday(cal$Date)
cal$Month <- month(cal$Date)
cal$Quarter <- quarter(cal$Date)
cal$Weekend <- 0
cal$Weekend[ cal$Weekday_No %in% c(1,7) ] <- 1
cal$Weekend <- as.factor(cal$Weekend)
cal$Holiday <- 0
# This variable will be filled in later
cal$HolidayWeek <- 0
for (i in seq(dim(cal)[1])) {
  # browser()
  if (any(which(cal$Date[i] == data_SA_90_NeuroNet_pre$Date))) {
    cal$HolidayWeek[i] <- data_SA_90_NeuroNet_pre$FeiertagsWoche[
      which(cal$Date[i] == data_SA_90_NeuroNet_pre$Date)[1]
      ]
  } else {
    cal$HolidayWeek[i] <- NA
  } # Stop if-else
}


cal$Month <- as.factor(cal$Month)
cal$Quarter <- as.factor(cal$Quarter)
cal$Weekday <- as.factor(cal$Weekday)
cal$Weekend <- as.factor(cal$Weekend)
# cal$Holiday <- as.factor(cal$Holiday)
cal$HolidayWeek <- as.factor(cal$HolidayWeek)

cal$Quantity <- 0
Quantity <- as.data.frame( table( sort(data_SA_90_NeuroNet_pre$Date) ) )

Quantity$Var1 <- as.Date(as.character(Quantity$Var1))
for (i in seq(dim(Quantity)[1])) {
  cal$Quantity[ which(  cal$Date == Quantity[[1]][i] ) ] <- Quantity[[2]][i]
}


# Compute and store sum of shipped weights (= Weight)
cal$Weight <- 0
Weight <- aggregate( gewicht ~ Date, data_SA_90_NeuroNet_pre, sum )
for (i in seq(dim(Weight)[1])) {
  cal$Weight[ which(  cal$Date == Weight[[1]][i] ) ] <- Weight[[2]][i]
}

# Compute and store average size of shipping as Weight/Quantity
cal$Size <- cal$Weight / cal$Quantity

# Compute and store the average temperatur for each day
Temperature <- aggregate( Temp ~ Date, data_SA_90_NeuroNet_pre, mean )
cal$Temp <- NA
for (i in seq(dim(Temperature)[1])) {
  cal$Temp[ which(  cal$Date == Temperature[[1]][i] ) ] <- Temperature[[2]][i]
}

holidays <- cal$Date[cal$Quantity < 500 & cal$Weekend == 0]
cal$Holiday[ cal$Date %in% holidays ] <- 1
cal$Holiday <- as.factor(cal$Holiday)


library("openxlsx")
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

calts <- ts(cal)
