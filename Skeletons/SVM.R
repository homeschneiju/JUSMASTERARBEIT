
years = sapply(c(1969:1984), function(x) rep(x,12))
years <- as.vector(years)

flosasvm <- svm(flosa~years+months)
prognoza <- predict(flosasvm, newdata=data.frame(nd))

par(mfrow=c(1,1))
ylim <- c(min(flosa), max(flosa))
xlim <- c(min(nd),max(nd))
plot(flosa, col="blue", type="l")
par(new=TRUE)
plot(prognoza, col="red", ylim=ylim, xlim=xlim)
