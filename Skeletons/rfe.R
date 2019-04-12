setwd("D:/")
cal <- readRDS("E:/calfinal.rds")
length(colnames(cal))
sapply(cal,function(x) any(is.na(x)))



# reorder columns of cal to set target variables first
cal2 <- cal[,c(10:12,1:4,6:9,14:28,30:33)]
sapply(cal2,function(x) any(is.na(x)))

colnames(cal2)

# choose numeric X-variables
calx <- colnames(cal2)[c(6:30)]
set.seed(7)

num <- as.vector(which(sapply(cal2[,c(calx)],class)=="numeric"))
# calculate correlation matrix
correlationMatrix <- cor(as.matrix(cal2[,calx[num]]), method = "pearson")
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
sort(highlyCorrelated)
calfilter <- calx[num][-highlyCorrelated]
calgone <- calx[num][highlyCorrelated]

fac <- as.vector(which(sapply(cal2[,c(calx)],class)=="factor"))
calfac <- colnames(cal2[,calx])[fac]
chisq.test(table(cal2[,"Month"], cal2[,"Quarter"]))
chisq.test(table(cal2[,"Weekday_No"], cal2[,"Weekend"]))
chisq.test(table(cal2[,"Holiday"], cal2[,"HolidayWeek"]), simulate.p.value = T)

chis = list()
twofac <- combn(calfac,2)
for (i in 1:ncol(twofac)){
  
  chis[i] <- chisq.test(table(cal2[,twofac[1,i]], cal2[,twofac[2,i]]), simulate.p.value = T)$p.value
}
sigchis <- which(chis < 0.05)
twofac[,sigchis]

# prepare training scheme
controlimp <- trainControl(method="timeslice", initialWindow = 7, horizon=1, fixedWindow=T)

# train the model
model <- train(Weight~., data=cal3, method="cforest", trControl=controlimp)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


controlrfe <- rfeControl(functions=caretFuncs, method="cv")
# run the RFE algorithm
results <- rfe(cal3[,1:25], cal3[,26], rfeControl=controlrfe)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


# leave out one variable:


cal3 <- cal2[,c(calx,"Weight")]
saveRDS(cal3, "E:/cal3.rds")
fac <- as.vector(which(sapply(cal3[,c(calx)],class)=="factor"))

cal3[,c(fac)] <- sapply(cal3[,c(fac)], as.numeric)
num <- as.vector(which(sapply(cal3[,c(calx)],class)=="numeric"))


loo <- function(xvars) {
  loo <- list()
  for (i in 1:length(xvars)){
    loo[[i]] <- xvars[-i]
  }
  return(loo)
}

foo <- loo(calx)
calts <- ts(cal3)

nonerr <- calx[num][c(1:13,15:18)]

iteratevars <- function(target, cal3, nonerr) {
  firstmod <- auto.arima(target, xreg=as.matrix(cal3[,nonerr ]))
  nonerr <- loo(nonerr)
  for (j in 1:length(nonerr)) {
    model <- auto.arima(target, xreg=as.matrix(cal3[,nonerr[[j]]]))
    if (model$aic < firstmod$aic) {
      firstmod <- model
      thisisit <- j
      
    }
  }
  return(firstmod)
}


whazzup <- iteratevars(calts[,26], cal3, nonerr)
answer <- names(whazzup$coef)
answer <- answer[4:length(answer)]
huh <- which(calx %in% answer)
calx[huh]
answer

whazzup$aic
firstmod <- auto.arima(calts[,26], xreg=as.matrix(cal3[,nonerr ]))
firstmod$aic

far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(cal3[,nonerr])), h = h)}
firstmod <- tsCV(calts[,26], forecastfunction = far1, h=1, window=1)


itvars <- function(target, cal3, nonerr) {
  nonerr1 <- nonerr
  far1 <- function(target1,h) {forecast(auto.arima(target1, xreg=as.matrix(cal3[,nonerr1]), h = h))}
  firstmod <- tsCV(target, forecastfunction = far1, h = 1, window=1)
  nonerr2 <- loo(nonerr)
  for (j in 1:length(nonerr)) {
    nonerr1 <- nonerr[[j]]
    model <- tsCV(target, forecastfunction = far1, h = 1, window=1)
    if (rmse(model) < rmse(firstmod)) {
      firstmod <- model
      thisisit <- j
    }
  }
  return(firstmod)
}

whazzupi <- itvars(calts[,26], cal3, nonerr)

nonerr1 <- nonerr
far1 <- function(target,h) {forecast(auto.arima(target, xreg=as.matrix(cal3[,nonerr1])), h = h)}
firstmod <- tsCV(calts[,26], forecastfunction = far1, h=1, window=1)
nonerr <- loo(nonerr)
  
nonerr1 <- nonerr[[1]]
  model <- tsCV(calts[,26], forecastfunction = far1, h = 1, window=1)
  if (rmse(model) < rmse(firstmod)) {
    firstmod <- model
    thisisit <- j
  }
}

# 
# nonerr2 <- loo(nonerr1[[thisisit]])
# for (k in 1:length(nonerr2)) {
#   model <- auto.arima(calts[,26], xreg=as.matrix(cal3[,nonerr2[[k]]]))
#   if (model$aic < firstmod$aic) {
#     firstmod <- model
#     thisisit <- k
#   } else{print(thisisit); break}
# }
# firstmod
# nonerr1[[j-1]]
# #print(any(is.na(cal[complete.cases(cal[,j]),j])))
# 
# 
# model <- auto.arima(calts, xreg=as.matrix(cal2[complete.cases(cal2[,foo[[1]]]),foo[[1]]]))
# 
# 
# calts <- ts(cal2$Weight)
# sapply(cal[complete.cases(cal[,foo[[1]]]),foo[[1]]], class)
# sapply(foo, class)
# # computationally intense:
# # install.packages("sets")
# # library(sets)
# # set_power(calx)
# # set_power(calx[1:5])
