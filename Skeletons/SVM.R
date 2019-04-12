
calsvm <- svm(calts~cal$year+cal$Month)
prognoza <- predict(calsvm, newdata=data.frame(mean(calts)))

par(mfrow=c(1,1))
ylim <- c(min(calts), max(calts))
xlim <- c(min(mean(calts)),max(mean(calts)))
plot(calts, col="blue", type="l", ylim=ylim)
 par(new=TRUE)
plot(prognoza, col="red", ylim=ylim)

year(calts)
calts
summary(calts)

