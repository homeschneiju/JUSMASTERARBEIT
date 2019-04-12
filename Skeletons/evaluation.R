#splitflosa <- createTimeSlices(flosa,1) # from caret
far1 <- function(x,h) (forecast(Arima(x,c(1,0,3), list(order = c(1,0,1))), h = h))
splitflosa <- tsCV(flosa, forecastfunction = far1, h = 1)
splitflosa

far2 <- function(x,h) (forecast(flosasvm, h = h))
splitflosa <- tsCV(flosa, forecastfunction = far2, h = 1)
splitflosa

# compare RMSE of tsCV with regular residuals on complete data set
rmse <- function(error)
{
  sqrt(mean(error^2, na.rm = TRUE))
}

rmse(splitflosa)
rmse(residuals(far2(flosa, h=1)))
#As expected, the RMSE from the residuals is smaller, as the corresponding "forecasts" are based on a model fitted to the entire 
# data set, rather than being true forecasts. (Rob Hyndman)

AIC(logLik(calsvm))
rmsesvm <- rmse(calsvm$residuals)
rmsearima <- rmse(autofit$residuals)
rmsenet <- rmse(netfit$residuals)

min(c(rmsenet, rmsesvm, rmsearima))
colnames(cal)
auto.arima(calts,xreg=cal[,c(2,3,4,6,7,8,9,13:24,28,30:36)])