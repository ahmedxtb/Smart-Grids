setwd("C:/Users/Mugdha/Desktop/Forecasting/Using Neural Networks")
install.packages("RSNNS")
install.packages("Rcpp")
library(Rcpp)
library(RSNNS)

Hrdat <- read.csv("Farm6_Actual_MayJunJuly_13_June14.csv")
index <- 1:round(nrow(Hrdat)*0.75)
Traindatx <- Hrdat[index,-9]
Traindaty <- Hrdat[index,9]
Testdat <- Hrdat[-index,-9]
Actual <- Hrdat[-index,9]

Rbfnn1 <- rbf(Traindatx,Traindaty, size=c(60), maxit=20000)
Predicted <- predict(Rbfnn1,Testdat)
WSreport <- cbind(PredictWS = Predicted,ActualWS = Actual,Diff = Predicted-Actual)
Ws.report <- as.data.frame(WSreport)
RMSE <- sqrt(mean(Ws.report$V3^2))
MAPE <- mean(abs(Ws.report$V3)/Ws.report$ActualWS)*100
