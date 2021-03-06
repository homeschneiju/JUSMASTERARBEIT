#####################
#### TSA WITH SARIMA
#### ACCORDING TO 
#### https://rpubs.com/SunnyBingoMe/sarima
####################### 

############
### PRELIMINARIES - LOAD
###############


# load data
data(Seatbelts)
summary(Seatbelts)

# interpolate missing values

data_interp <- data_SA_90_NeuroNet_pre
data_interp <- na.interp(data_interp, lambda="auto")



###################
#### Convert Data to TS object
#### DAILY BASIS
###################


#ts_freight <- ts(data[complete.cases(data),])
flosa <- Seatbelts[,"DriversKilled"]
jausa <- Seatbelts
# GET OVERVIEW
class(flosa)
start(flosa); end(flosa)
summary(flosa)
frequency(flosa) # 12 months per year

# PLOT TIME SERIES AND BOXPLOT
windows()
par(mfrow=c(2,1))
plot(flosa, type= "b")
boxplot(flosa)
dev.off()

windows()
par(mfrow=c(2,1))
plot(aggregate(flosa,FUN=mean)) # monthly plot
boxplot(flosa~cycle(flosa)) # boxplot by month


# seasonality?
findfrequency(flosa)
# 12
plot(flosa[1:24])
plot(flosa[24:36])
# -> besonders of am Ende des Monats


# TREND /stationarity
lmtrend = lm(flosa ~ time(flosa))
plot(flosa)
abline(reg = lmtrend, col="red")
# leicht abschüssig

#cycle(flosa)
adf.test(flosa, alternative="stationary")

par(mfrow = c(2,1))
acf(flosa)
acf(difflosa)


# adf.test(diff(flosa), alternative="stationary", k=0)
# adf.test(diff(log(flosa)), alternative="stationary", k=0)

# apply differencing - not needed tho
difflosa = diff(flosa, differences = 12)
windows()
par(mfrow=c(2,2))
plot(flosa)
plot(difflosa)
boxplot(flosa); 
boxplot(difflosa)
par(mfrow=c(2,1))
acf(difflosa) # 5
pacf(difflosa, plot = T) # 3
# -> ARIMA(3,1,5)
difautofit <- auto.arima(difflosa, seasonal=FALSE)
diffit <- arima(difflosa, c(5,0,0), include.mean=F)
difautofit # ARIMA(5,0,0)
diffit # ARIMA(5,0,0)



# look for p and q
par(mfrow=c(2,1))
acf(flosa) # 3
pacf(flosa, plot = T) # 1
#looks like ARMA(5,2)
autofit <- auto.arima(flosa)
fit <- arima(flosa,c(1,0,3), list(order = c(1,0,1)))
summary(autofit)
summary(fit)
coef(autofit)
#ARIMA(3,1,1)(2,0,0)[12]  

# for complex seasonality: alternative fitting approach
bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fitfour <- auto.arima(flosa, xreg=fourier(flosa, K=i), seasonal=FALSE)
  if(fitfour$aicc < bestfit$aicc)
    bestfit <- fitfour
  else break;
}



#flosalag = lag(flosa)

#############################################
######### 3. MODEL DIAGNOSTICS ##############
#############################################

checkresiduals(fit)
Box.test(fit$residuals,type="Ljung-Box")
jarque.bera.test(fit$residuals)
# residuals are  normally distributed and iid

#############################################
############## 4. FORECASTS #################
#############################################

fc <- forecast(bestfit, xreg=fourier(flosa, K=5, h=60))

fc2 <- forecast(fit,h=60)
fc3 <- forecast(autofit,h=60)
fc4 <- forecast(diffit,h=60)
fc5 <- forecast(difautofit,h=60)

par(mfrow=c(3,2))
plot(fc)
plot(fc2)
plot(fc3)
plot(fc4)
plot(fc5)

#############################################
########### VARIABLE SELECTION ##############
#############################################

sizes <- c(1:5)
ctrl <- rfeControl(functions = caretFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

arimarfe <- rfe(jausa[,-1], jausa[,1], sizes = sizes, rfeControl = ctrl)

lm <- lm(formula = DriversKilled ~ ., data = jausa)
step(lm)
step(arimaxfit)
# rfe AND lm choose drivers, kms and law

##############
######## TRY CUSTOM RFE
##############

train <- jausa[1:150,]
test <- jausa[151:192,]
arimaxfit <- update(fit, xreg =jausa[,-1])
arimaxfit$coef
lowest <- rank(arimaxfit$coef)
lowest <- which(lowest == max(lowest))
which(colnames(jausa) == labels(lowest))

jausa2 <- jausa
head(jausa2[,-which(colnames(jausa) == labels(lowest))])

featselec <- function(output, features, fit){
  feat = features
  if (class(fit) == "Arima") {
    aic <- 2
   # aic2 <- 1
    while (aic < aic2) {
    arimaxfit <- update(fit, features)
    coefrank <- rank(arimaxfit$coef)
    lowest <- which(coefrank == max(coefrank))
    aic2 <- arimaxfit$aic
    # feat <- feat[,-which(colnames(feat) == labels(feat))]
    # arimaxfit2 <- update(arimaxfit, features)
    # coefrank2 <- rank(arimaxfit2$coef)
    # lowest2 <- which(coefrank2 == max(coefrank2))
    # aic2 <- arimaxfit2$aic
    # aic <- aic2
    }
  }
  }
  
featselec(flosa, jausa[-1], arimaxfit)

###################
#### DATA FOR 2016
#### MONTHS
###################

monsa = tapply(flosa,INDEX=list(flosa[,1]),FUN= function(x) mean(x,na.rm=T))
monsa
plot(monsa, type="l")
# strong seasonality??


frequency(monsa)
# 1

linearModel = lm(monsa ~ time(monsa))
plot(monsa, type="l")
abline(reg = linearModel, col="red")



monNum = as.numeric(monsa)
monDiff = diff(monsa, differences = 1)
par(mfrow=c(1,2))
acf(monDiff,  plot = T)
pacf(monDiff, plot = T)

adf.test(diff(log(monsa)), alternative="stationary", k=0)
# Null hypothesis = NOT stationary
# not significant = flosa is not stationary

par(mfrow=c(2,2))
plot(monsa, type="l")
plot(log(monsa), type="l") # same as the one above
plot(diff(monsa), type="l")
plot(diff(log(monsa)), type="l") # same as the one above


acf(log(monNum))
acf(diff(log(monNum)))
pacf(diff(log(monNum)))

autoArimaModel1 = auto.arima(monsa)
autoArimaModel2 = auto.arima(log(monsa))
autoArimaModel3 = auto.arima(diff(log(monsa)))
autoArimaModel4 = auto.arima(diff(log(monsa)))
# negative BICs are okay. still take the smaller value though!


###################
#### DATA FOR 2016
#### WEEKS
###################

weeksa <- ts(data_2016[complete.cases(data_2016),c(8,28)])
weeksa <- tapply(weeksa[,2],INDEX=list(weeksa[,1]),FUN= function(x) mean(x,na.rm=T))

plot(weeksa, type="p")
# krasser ausreißer ganz zum schluss?


frequency(weeksa)
# 1

linearModel = lm(weeksa[1:52] ~ time(weeksa[1:52]))
plot(weeksa[c(1:52)], type="l")
abline(reg = linearModel, col="red")

adf.test(diff(log(weeksa)), alternative="stationary", k=0)
# not stationary
adf.test(log(weeksa), alternative="stationary", k=0) 
# not stationary
adf.test(diff(weeksa), alternative="stationary", k=0) # stationary
# stationary
adf.test(weeksa, alternative="stationary", k=0) 
# not stationary

weeksa2 <- weeksa[-53]
adf.test(diff(log(weeksa2)), alternative="stationary", k=0)
# stationary
adf.test(log(weeksa2), alternative="stationary", k=0) 
# not stationary
adf.test(diff(weeksa2), alternative="stationary", k=0) # stationary
# stationary
adf.test(weeksa2, alternative="stationary", k=0) 
# not stationary

par(mfrow=c(2,2))
plot(weeksa, type="l")
plot(log(weeksa), type="l") # same as the one above
plot(diff(weeksa), type="l")
plot(diff(log(weeksa)), type="l") # same as the one above

par(mfrow=c(2,2))
plot(weeksa2, type="l")
plot(log(weeksa2), type="l") # same as the one above
plot(diff(weeksa2), type="l")
plot(diff(log(weeksa2)), type="l") # same as the one above



weekNum = as.numeric(weeksa)
weekDiff = diff(weeksa, differences = 1)
par(mfrow=c(3,2))
acf(weekDiff,  plot = T)
pacf(weekDiff, plot = T)
# lag 1, lag 4??
acf(log(weekNum))
pacf(log(weekNum))
acf(diff(log(weekNum)))
pacf(diff(log(weekNum)))

weekNum2 = as.numeric(weeksa2)
weekDiff2 = diff(weeksa2, differences = 1)
par(mfrow=c(3,2))
acf(weekDiff2,  plot = T)
pacf(weekDiff2, plot = T)
# lag 1, lag 4??
acf(log(weekNum2))
pacf(log(weekNum2))
acf(diff(log(weekNum2)))
pacf(diff(log(weekNum2)))


autoArimaModel1 = auto.arima(weeksa)
autoArimaModel2 = auto.arima(log(weeksa))
autoArimaModel3 = auto.arima(diff(log(weeksa)))
autoArimaModel4 = auto.arima(diff(log(weeksa)))
