###################################################################
########## PRELIMINARIES
###################################################################

packs <- c("tseries", "forecast", "reshape2", "zoo", "lubridate", "tidyverse", "e1071", "openxlsx", "caret")

Install_And_Load <- function(packages) {
  k <- packages[!(packages %in% installed.packages()[,"Package"])];
  if(length(k))
  {install.packages(k, repos='https://cran.rstudio.com/');}
  
  for(package_name in packages)
  {library(package_name,character.only=TRUE, quietly = TRUE);}
}

Install_And_Load(packs)

 cal <- readRDS("E:/calfinal.rds")
 cal <- cal[,c(2,4:ncol(cal))]
 cal <- cal[,c(1,4:ncol(cal))]

cal3 <- readRDS("E:/cal3.rds")
fac <- as.vector(which(sapply(cal,class)=="factor"))

#cal3[,c(fac)] <- sapply(cal3[,c(fac)], as.numeric)
num <- as.vector(which(sapply(cal3[,c(calx)],class)=="numeric"))

#nonerr <- colnames(cal3[,c(1:14,16:25)])
nonerr <- colnames(cal[,c(2:ncol(cal))])

loo <- function(xvars) {
  loo <- list()
  for (i in 1:length(xvars)){
    loo[[i]] <- xvars[-i]
  }
  return(loo)
}



cal <- cal3


#############################################################
################ ARIMA
#############################################################

# itvars <- function(targ, cal3, nonerr) {
#   target <- ts(targ)
#   cal3 <- cal3
#   #print(head(target))
#   nonerr1 <- nonerr
#   # print(nonerr1)
#   fc <- function(y, h, xreg, xreg_ncol){
#     X <- matrix(xreg[1:length(y), ], ncol = xreg_ncol)
#     if(NROW(xreg) < length(y) + h)
#       stop("Not enough xreg data for forecasting")
#     newX <- matrix(xreg[length(y) + (1:h), ], ncol = xreg_ncol)
#     fit <- auto.arima(y, xreg=X)
#     forecast(fit, xreg = newX)}
#   firstmod <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
#   #print(firstmod)
#   nonerr2 <- loo(nonerr)
#   for (j in 1:length(nonerr2)) {
#     nonerr1 <- nonerr2[[j]]
#     # print(nonerr1)
#     # far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(xreg[,nonerr1]), h = h))}
#     model <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
#     #print(model)
#     if (rmse(model) < rmse(firstmod)) {
#       firstmod <- model
#       thisisit <- j
#     }else if (j >1 ){print("here1");return(nonerr2[j-1])} else if (j == 1) {print("here2");return(nonerr)}
#   }
#   print("here3");return(nonerr1)
# }



itvarsarima <- function(targ, cal3, nonerr) {
  target <- ts(targ)
  cal3 <- cal3
  #print(head(target))
  nonerr1 <- nonerr
  # print(nonerr1)
  fc <- function(y, h, xreg, xreg_ncol){
    X <- matrix(xreg[1:length(y), ], ncol = xreg_ncol)
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    newX <- matrix(xreg[length(y) + (1:h), ], ncol = xreg_ncol)
    fit <- auto.arima(y, xreg=X)
    forecast(fit, xreg = newX)}
  firstmod <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
  #print(firstmod)
  nonerr2 <- loo(nonerr)
  thisisit <- nonerr
  for (j in 1:length(nonerr2)) {
    nonerr1 <- nonerr2[[j]]
    # print(nonerr1)
    # far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(xreg[,nonerr1]), h = h))}
    model <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
    #print(model)
    if (rmse(model) < rmse(firstmod) || rmse(model) == rmse(firstmod) ) {
      firstmod <- model
      thisisit <- nonerr1
    }
  }
  return(thisisit)
}

whazzupi <- itvars(cal3[,26], cal3, nonerr)
riri <- itvarsarima(cal[,1], cal, nonerr)

iteratevars <- function(target, cal3, nonerr) {
  firstmod <- auto.arima(ts(target), xreg=as.matrix(ts(cal3[,nonerr])))
  thisisit <- nonerr
  nonerr2 <- loo(nonerr)
  for (j in 1:length(nonerr2)) {
    model <- auto.arima(ts(target), xreg=as.matrix(ts(cal3[,nonerr2[[j]]])))
    if (model$aic < firstmod$aic || model$aic == firstmod$aic) {
      firstmod <- model
      thisisit <- nonerr2[[j]]
      
    }
  }
  return(thisisit)
}

rira <- iteratevars(cal[,1], cal, nonerr[-c(15,16)])

ririmod <- auto.arima(ts(cal[,1]), xreg=as.matrix(ts(cal[,riri[-15]])))
riramod <- auto.arima(ts(cal[,1]), xreg=as.matrix(ts(cal[,rira])))
ririmod$aic; riramod$aic
rmse(ririmod$residuals); rmse(riramod$residuals)
# ririmod beide male besser

ririmod <- tsCV(cal[,1], forecastfunction = fc, xreg= cal[,riri[-15]], xreg_ncol=length(riri[-15]), h = 1, window=1)
riramod <- tsCV(cal[,1], forecastfunction = fc, xreg= cal[,rira], xreg_ncol=length(rira), h = 1, window=1)
rmse(ririmod); rmse(riramod)
AIC(ririmod)

ririmod$aic; riramod$aic
rmse(ririmod$residuals); rmse(riramod$residuals)

whazzup <- iteratevars(calts[,26], cal3, nonerr)
answer <- names(whazzup$coef)
answer <- answer[4:length(answer)]
huh <- which(calx %in% answer)
calx[huh]
answer




fc <- function(y, h, xreg, xreg_ncol){
  X <- matrix(xreg[1:length(y), ], ncol = xreg_ncol)
  if(NROW(xreg) < length(y) + h)
    stop("Not enough xreg data for forecasting")
  newX <- matrix(xreg[length(y) + (1:h), ], ncol = xreg_ncol)
  fit <- auto.arima(y, xreg=X)
  forecast(fit, xreg = newX)}

largerarima <- tsCV(calts[,26], cal3[,whazzupi], xreg_ncol=length(whazzupi), forecastfunction=fc, h=1, window=1)
rmse(largerarima)
smallerarima <- tsCV(calts[,26], cal3[,answer], xreg_ncol=length(answer), forecastfunction=fc, h=1, window=1)
rmse(smallerarima)

whazzup$aic # gehört zu answer

he <- auto.arima(calts[,26], xreg=as.matrix(cal3[,whazzupi]))
he$aic

whazzupi
answer # hat geringeres aic und gleichen rmse!

##############################################################################################
############### SVM #########
##############################################################################################


################## 1 - funktioniert am besten


calcor <- cor(as.matrix(cal[,nonerr]))
findCorrelation(calcor, cutoff=0.75)
findLinearCombos(cal)
nz <- nearZeroVar(cal)
# exclude holidays
cali <- cal[,-nz]

itvarssvm <- function(targ, cal3, nonerr) {
myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 14,
                              horizon = 1,
                              fixedWindow = T)
target <- ts(targ)
cal3 <- cal3
nonerr1 <- nonerr
firstsvm <- train(targ~.,
                    data = cbind(targ,cal[,nonerr1]),
                    method = "svmRadial",
                   # preProc = c("dummyVars"),
                    trControl = myTimeControl)
nonerr2 <- loo(nonerr)
thisisit <- nonerr1
for (j in 1:length(nonerr2)) {
  nonerr1 <- nonerr2[[j]]
  # print(nonerr1)
  # far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(xreg[,nonerr1]), h = h))}
  model <- train(targ~.,
                 data = cbind(targ,cal[,nonerr1]),
                 method = "svmRadial",
                # preProc = c("dummyVars"),
                 trControl = myTimeControl)
  if (model$results$RMSE[model$results$C == min(model$results$C)] < firstsvm$results$RMSE[firstsvm$results$C == min(firstsvm$results$C)] || model$results$RMSE[model$results$C == min(model$results$C)] == firstsvm$results$RMSE[firstsvm$results$C == min(firstsvm$results$C)]) {
    firstsvm <- model
    thisisit <- nonerr1
  }
  } 
return(thisisit)
}

hi3 <- itvarssvm(cal[,1], cal, nonerr)



################## 2
itvarssvm <- function(targ, cal3, nonerr) {
  target <- ts(targ)
  cal3 <- cal3
  print(head(target))
  nonerr1 <- nonerr
   print(nonerr1)
  fc <- function(y, h, xreg, xreg_ncol){
    X <- matrix(xreg[1:length(y), ], ncol = xreg_ncol)
    if(NROW(xreg) < length(y) + h)
      stop("Not enough xreg data for forecasting")
    newX <- matrix(xreg[length(y) + (1:h), ], ncol = xreg_ncol)
    fit <- svm(y~X)
    forecast(fit, xreg = newX)}
  firstmod <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
  print(firstmod)
  nonerr2 <- loo(nonerr)
  for (j in 1:length(nonerr2)) {
    nonerr1 <- nonerr2[[j]]
    # print(nonerr1)
    # far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(xreg[,nonerr1]), h = h))}
    model <- tsCV(target, forecastfunction = fc, xreg= cal3[,nonerr1], xreg_ncol=length(nonerr1), h = 1, window=1)
    #print(model)
    if (rmse(model) < rmse(firstmod)) {
      firstmod <- model
      thisisit <- j
    }else if (j >1 ){print("here1");return(nonerr2[j-1])} else if (j == 1) {print("here2");return(nonerr)}
  }
  print("here3");return(nonerr1)
}

rfesvm <- itvarssvm(cal3[,26], cal3, nonerr)

################## 3
fc <- function(y, h, xreg, xreg_ncol){
  X <- matrix(xreg[1:length(y), ], ncol = xreg_ncol)
  if(NROW(xreg) < length(y) + h)
    stop("Not enough xreg data for forecasting")
  newX <- matrix(xreg[length(y) + (1:h), ], ncol = xreg_ncol)
  fit <- svm(y~X)
  forecast(fit, xreg = newX)}

far1 <- function(target,xreg,h, nonerr1) {predict(svm(target~as.matrix(xreg[,nonerr1])), h = h)}

firstmod <- tsCV(ts(cal3[,26]), forecastfunction = far1, xreg= cal3, nonerr1 =nonerr, h = 1)
print(firstmod)

hi <- svm(target~as.matrix(xreg[,nonerr1]))

################## 4
calts <- ts(cal)

svmslice <- createTimeSlices(cal[,26], initialWindow =1, fixedWindow = F, list=F)

far2 <- function(y, x,h) (forecast(svm(y~as.matrix(x)), h = h))
splitflosa <- tsCV(y=ts(calts[,26]), x = ts(cal[,nonerr]),forecastfunction = far2, h = 1)
splitflosa


hi <- svm(cal[,26]~as.matrix(cal[,nonerr]))
predict(hi, )
##################################################################################################################
####################### NNETAR ###########################
##########################################################################################################
