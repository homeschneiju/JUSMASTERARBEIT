hi2 <- hi[-24]
long<- train(Weight~.,
               data = cal[,c(hi2,"Weight")],
               method = "svmRadial",
               # preProc = c("dummyVars"),
               trControl = myTimeControl)
longrmse <- long$results$RMSE[long$results$C == min(long$results$C)]


longer <-  train(Weight~.,
                 data = cal[,c(hi3,"Weight")],
                 method = "svmRadial",
                 # preProc = c("dummyVars"),
                 trControl = myTimeControl)
longerrmse <- longer$results$RMSE[longer$results$C == min(longer$results$C)]


calx <- colnames(cal)[c(3:ncol(cal))]
set.seed(7)

num <- as.vector(which(sapply(cal[,c(calx)],class)=="numeric"))
# calculate correlation matrix
correlationMatrix <- cor(as.matrix(cal[,calx[num]]), method = "pearson")
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
#print(highlyCorrelated)
#sort(highlyCorrelated)
calfilter <- calx[num][-highlyCorrelated]

shorter <-  train(Weight~.,
                  data = cal[,c(calfilter,"Weight")],
                  method = "svmRadial",
                  # preProc = c("dummyVars"),
                  trControl = myTimeControl)
shortrmse <- shorter$results$RMSE[shorter$results$C == min(shorter$results$C)]


which.min(c(shortrmse,longrmse,longerrmse))
