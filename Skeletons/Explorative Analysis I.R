############################################################################### -
# Project:    "JUSTMASTERARBEIT"
# Author:     Juliana Schneider
# Dataset:    data SA90 NeuroNet
# Topic:      "Explorative Analysis - Original Data"
################################################################################ -


################################### -
# 2.1 Quantities over all Days -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Quantities_over_all_Days.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
with(cal, plot(Date, Quantity
               , type = "l" 
               , main = "Shipments per Day w/o Weekends and Holidays"
               , ylab = "Quantitiy"
               , xlab = "Date"
               , lwd = 2
))
# Conlusion:
# - We can see that there is a clear pattern between workday and weekend quantities.
# - We can see some spikes in quantities around end of march, mid may, mid august.
# - Interestingly, we observe a decline in quantities towards the end of the year
#   and no spike around christmas.

# dev.off()

################################### -
# 2.2. Quantities per Workday -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Quantities_per_Workday.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Quantity ~ Weekday_No, cal, sum)[[2]]
         , main = "Shipments per Weekday"
         , ylab = "Quantity"
         , xlab = "Weekday"
         , names.arg = aggregate(Quantity ~ Weekday_No, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Quantity ~ Weekday_No, cal, sum)[[2]]), col = "red")
# Insert mean of workdays.
abline(h = mean(aggregate(Quantity ~ Weekday_No, cal[cal$Weekend == 0,], 
                          sum)[[2]]), col = "blue")
# Conclusion:
# - Again, we observe a distinction between weekdays and weekends.
# - Tuesday and Wednesday the Weights are highest (above average).

# dev.off()


################################### -
# 2.3. Quantities per Months -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Quantities_per_Month.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Quantity ~ Month, cal, sum)[[2]]
         , main = "Shipments per Month"
         , ylab = "Quantity"
         , xlab = "Month"
         , names.arg = aggregate(Quantity ~ Month, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Quantity ~ Month, cal, sum)[[2]]), col = "red")

# dev.off()

################################### -
# 2.4. Quantities per Quarter -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Quantities_per_Quarter.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Quantity ~ Quarter, cal, sum)[[2]]
         , main = "Shipments per Quarter"
         , ylab = "Quantity"
         , xlab = "Quarter"
         , names.arg = aggregate(Quantity ~ Quarter, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Quantity ~ Quarter, cal, sum)[[2]]), col = "red")

# dev.off()

################################### -
# 2.5. Quantities w/o Weekends -------------
################################### -


# Plot number of shipments without weekends
# windows()
# setEPS()
# postscript("./Figures/Quantities_wo_Weekends.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot( cal$Quantity[cal$Weekend == 0]
      , type = "l" 
      , main = "Shipments per Day w/o Weekends"
      , ylab = "Quantitiy"
      , xlab = "Date"
      , lwd = 2
)
# Conclusion:
# - We can clearly see the effect of holidays:
# - On a holiday shipments are very low.
# - Prior to holidays shipments increase steadily

# dev.off()

################################### -
# 2.7. Quantities w/o Weekends and Holidays -------------
################################### -


# windows()
# setEPS()
# postscript("Quantities_wo_Weekends_Holidays.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)
# pdf("./Figures/Quantities_wo_Weekends_Holidays.pdf",paper = "a4r", width = 400, height = 350, onefile = T
# , pointsize = 15)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
with(cal[cal$Holiday == 0 & cal$Weekend == 0,], plot(Date, Quantity
                                                     , type = "l" 
                                                     , main = "Shipments per Day w/o Weekends and Holidays"
                                                     , ylab = "Quantitiy"
                                                     , xlab = "Date"
                                                     , lwd = 2
)
)
abline(h = mean(cal[cal$Holiday == 0 & cal$Weekend == 0,]$Quantity), col = "red")

par(new = TRUE)
with(cal, plot(Date, Temp
               , type = "h"
               , axes = FALSE
               , bty = "n"
               , xlab = ""
               , ylab = ""
               , col = alpha("blue", .3)
)
)
axis(side = 4, at = pretty(range(cal$Temp, na.rm = T )))
mtext("Temp", side = 4, line = 3)
abline(h = mean(cal$Temp, na.rm = T), col = "blue")

# dev.off()

############################################################ -
# 3. Weight over Time -----------------
############################################################ -



################################### -
# 3.1 Weight over all Days -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Weights_over_all_Days.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
with(cal, plot(Date, Weight
               , type = "l" 
               , main = "Weight per Day w/o Weekends and Holidays"
               , ylab = "Weight"
               , xlab = "Date"
               , lwd = 2
))
# Conlusion:
# - XXX

# dev.off()

################################### -
# 3.2. Weights per Weekday -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Weights_per_Weekday.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Weight ~ Weekday_No, cal, sum)[[2]]
         , main = "Weight per Weekday"
         , ylab = "Weight"
         , xlab = "Weekday"
         , names.arg = aggregate(Weight ~ Weekday_No, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Weight ~ Weekday_No, cal, sum)[[2]]), col = "red")
# Insert mean of workdays.
abline(h = mean(aggregate(Weight ~ Weekday_No, cal[cal$Weekend == 0,], 
                          sum)[[2]]), col = "blue")
# Conclusion:
# - XXX

# dev.off()

################################### -
# 3.3. Weights per Month -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Weights_per_Month.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Weight ~ Month, cal, sum)[[2]]
         , main = "Weight per Month"
         , ylab = "Weight"
         , xlab = "Month"
         , names.arg = aggregate(Weight ~ Month, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Weight ~ Month, cal, sum)[[2]]), col = "red")

# dev.off()

################################### -
# 3.4. Weights per Quarter -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Weights_per_Quarter.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Weight ~ Quarter, cal, sum)[[2]]
         , main = "Weight per Quarter"
         , ylab = "Weight"
         , xlab = "Quarter"
         , names.arg = aggregate(Weight ~ Quarter, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Weight ~ Quarter, cal, sum)[[2]]), col = "red")

# dev.off()


################################### -
# 3.5. Weight per Day w/o Weekends -------------
################################### -


# Plot number of shipments without weekends
# windows()
# setEPS()
# postscript("./Figures/Weights_wo_Weekends.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
# par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot( cal$Weight[cal$Weekend == 0]
      , type = "l" 
      , main = "Weight per Day w/o Weekends"
      , ylab = "Weight"
      , xlab = "Date"
      , lwd = 2
)
# Conclusion:
# - XXX

# dev.off()



################################### -
# 3.6 Weights w/o Weekends and Holidays -------------
################################### -


# windows()
# pdf("./Figures/Weights_wo_Weekends_Holidays.pdf",paper = "a4r", width = 400, height = 350, onefile = T
#     , pointsize = 15)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
with(cal[cal$Holiday == 0 & cal$Weekend == 0,], plot(Date, Weight
                                                     , type = "l" 
                                                     , main = "Weight per Day w/o Weekends and Holidays"
                                                     , ylab = "Weight"
                                                     , xlab = "Date"
                                                     , lwd = 2
)
)
abline(h = mean(cal[cal$Holiday == 0 & cal$Weekend == 0,]$Weight), col = "red")

par(new = TRUE)
with(cal, plot(Date, Temp
               , type = "h"
               , axes = FALSE
               , bty = "n"
               , xlab = ""
               , ylab = ""
               , col = alpha("blue", .3)
)
)
axis(side = 4, at = pretty(range(cal$Temp, na.rm = T )))
mtext("Temp", side = 4, line = 3)
abline(h = mean(cal$Temp, na.rm = T), col = "blue")
# Conclusion:
# - XXX

# dev.off()




############################################################ -
# 4. Size over Time -----------------
############################################################ -



################################### -
# 4.1 Size over all Days -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Size_over_all_Days.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
with(cal, plot(Date, Size
               , type = "l" 
               , main = "Size per Day"
               , ylab = "Size"
               , xlab = "Date"
               , lwd = 2
))
# Conlusion:
# - XXX

# dev.off()

################################### -
# 4.2. Size per Day -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Size_per_Weekday.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Size ~ Weekday_No, cal, sum)[[2]]
         , main = "Size per Weekday"
         , ylab = "Size"
         , xlab = "Weekday"
         , names.arg = aggregate(Size ~ Weekday_No, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Size ~ Weekday_No, cal, sum)[[2]]), col = "red")
# Insert mean of workdays.
abline(h = mean(aggregate(Size ~ Weekday_No, cal[cal$Weekend == 0,], 
                          sum)[[2]]), col = "blue")

# Conclusion:
# - XXX

# dev.off()

################################### -
# 4.3. Size per Month -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Size_per_Month.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Size ~ Month, cal, sum)[[2]]
         , main = "Size per Month"
         , ylab = "Size"
         , xlab = "Month"
         , names.arg = aggregate(Size ~ Month, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Size ~ Month, cal, sum)[[2]]), col = "red")

# dev.off()

################################### -
# 4.4. Size per Quarter -------------
################################### -

# windows()
# setEPS()
# postscript("./Figures/Size_per_Quarter.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
barplot( aggregate(Size ~ Quarter, cal, sum)[[2]]
         , main = "Size per Quarter"
         , ylab = "Size"
         , xlab = "Quarter"
         , names.arg = aggregate(Size ~ Quarter, cal, sum)[[1]]
)

# Insert mean of all days.
abline(h = mean(aggregate(Size ~ Quarter, cal, sum)[[2]]), col = "red")

# dev.off()


################################### -
# 4.5. Size per Day Without Weekends -------------
################################### -


# Plot number of shipments without weekends
# windows()
# setEPS()
# postscript("./Figures/Size_wo_Weekends.eps", onefile = T, paper = "a4", horizontal = T, width = 400,
#            height = 350,pointsize = 15)

par(mar = c(5.1, 4.1, 4.1, 2.1))
plot( cal$Size[cal$Weekend == 0]
      , type = "l" 
      , main = "Size per Day w/o Weekends"
      , ylab = "Size"
      , xlab = "Date"
      , lwd = 2
)

# Conclusion:
# - XXX

# dev.off()


################################### -
# 4.6 Size w/o Weekends and Holidays -------------
################################### -


# windows()
# pdf("./Figures/Size_wo_Weekends_Holidays.pdf",paper = "a4r", width = 400, height = 350, onefile = T
#     , pointsize = 15)

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
with(cal, plot(Date, Temp
               , type = "h"
               , axes = FALSE
               , bty = "n"
               , xlab = ""
               , ylab = ""
               , col = alpha("blue", .3)
)
)
axis(side = 4, at = pretty(range(cal$Temp, na.rm = T )))
mtext("Temp", side = 4, line = 3)
abline(h = mean(cal$Temp, na.rm = T), col = "blue")

par(new = TRUE)
with(cal[cal$Holiday == 0 & cal$Weekend == 0,], plot(Date, Size
                                                     , type = "l" 
                                                     , main = "Size per Day w/o Weekends and Holidays"
                                                     , ylab = "Size"
                                                     , xlab = "Date"
                                                     , lwd = 2
)
)
abline(h = mean(cal[cal$Holiday == 0 & cal$Weekend == 0,]$Size), col = "red")


# Conclusion:
# - XXX

# dev.off()


