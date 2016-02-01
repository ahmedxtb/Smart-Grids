#library(kernlab)

#Appproach to getting data from database into a dataset
#library(RODBC)
#sql <- "select FORMAT(dtdate,'dd-MMM-yyyy') as Date,dtStartTime as Time,avg(davgwindspeed) as dWindSpeed from datastore d,turbine t where dtdate between '01-mar-2014' and '31-mar-2014' and d.iturbineid =t.iturbineid and ientityid=2 and davgwindspeed is not null group by FORMAT(dtdate,'dd-MMM-yyyy'),dtStartTime order by  FORMAT(dtdate,'dd-MMM-yyyy'),dtStartTime \"";
#cn <- odbcDriverConnect('driver={SQL Server Native Client 11.0};server=ea5h5fo14k.database.windows.net,1433;database=AEWind-MytrahU;uid=algoengines;pwd=')"
#Wind <- sqlQuery(cn,sql)

#install & open SVM package - kernlab & ggplot2 package
install.packages("kernlab")
library(kernlab)
install.packages("ggplot2")
library(ggplot2)

#change R working directory
setwd("C:/Users/varun/Desktop/EST&P/Smart Grids & Future Electric Energy Systems/Term Project")

#Read file to populate dataset
HourlyData <- read.csv("Windfarm_Nov13_Oct14.csv", header=TRUE)

#80% for training model, 20% for testing
index <- sample(1:round((dim(HourlyData)[1]*0.8)))
train <- HourlyData[index,]
test <-  HourlyData[-index,]

#Computing number of rows for final table
TVal <- nrow(test)

#Training the SVM model
svmRbftune <- ksvm(p3 ~ .,data = train, type = "eps-svr", method = "rbfdot",nu = 0.2, epsilon = 0.3)
                  

# Asking SVM to predict value 
svmpredict<-predict(svmRbftune,test)

#Computing accuracy of prediction
Test <- test$p3
RbfData <- cbind(svmpredict,Test,(svmpredict - Test) )
RbfDataM <- as.data.frame(RbfData)
qplot(svmpredict, Test, data = RbfDataM, geom =c("point", "smooth"))

RMSE <- sqrt(mean((RbfDataM$V3[1:TVal-1])^2))
MAPE <- mean(abs(RbfDataM$V3[1:TVal-1])/RbfDataM$Test[1:TVal-1])*100

hist(RbfDataM$V3,breaks = 18)

#Creating a new table with actual values: Apply inverse transform
#sd <- 1.74488
#meanVal <- 4.924895
  
#Pred <- log((1/RbfDataM$V1)-1)*sd + meanVal
#Actual <- log((1/RbfDataM$Test)-1)*sd + meanVal

#Invdat <- as.data.frame(cbind(Pred, Actual, Difference = Pred - Actual))

#Calculating RMSE & MAPE values for re-transformed data
#RMSE <- sqrt(mean(Invdat$Difference^2))
#MAPE <- mean(abs(Invdat$Difference)/Invdat$Actual)*100


# Other checks
#plot(density(RbfDataM$V3[1:1220]))
#write.csv(RbfDataM,file = "Predicted_dat.csv")
#summary(RbfDataM$V3)
#summary(RbfDataM$Test)
#MeanPer <- 100*(mean(RbfDataM$V3)/mean(RbfDataM$Test))

