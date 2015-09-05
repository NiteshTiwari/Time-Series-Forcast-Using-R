# =====================================================================
# CSE487/587
# Author: Vishwas Shanbhog
# Email: vishwass@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO

# just read one file
filespath = "/gpfs/courses/cse587/spring2015/data/hw2/data/"


setoffiles <- list.files(path = filespath,pattern = "*.csv")
MAEarima <- vector()
MAEholt <- vector()
MAElin <- vector()
MAElistname<-vector()


for(file in setoffiles)
{

filename<-file.path("/gpfs/courses/cse587/spring2015/data/hw2/data/",file)
# if file is not empty
if((file.info(filename)[1]>0) && (length(readLines(filename)) == 755)){

  # read one csv file into variable (DO NOT EDIT)
  textData=read.csv(file=filename, header=T)
  
  # convert txt data to time-series data, in day unit (DO NOT EDIT)
  tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
  
  # define train data (DO NOT EDIT)
  trainData = window(tsData, end=c(2014,14))
  
  # define test data (DO NOT EDIT)
  testData = window(tsData, start=c(2014,15))
  trainDatalength <- length(trainData)
  testDatalength <-length(testData) 
  trainingvector<-c(1:trainDatalength)
  test=c(745:754)
  
    
  
             
  # MAE row vector (DO NOT EDIT)
  MAE = matrix(NA,1,testDatalength)
  MAE1 = matrix(NA,1,testDatalength)
  MAE2 = matrix(NA,1,length(testData) )
  
  # apply ARIMA model (DO NOT EDIT)
  fitData = auto.arima(trainData)
  linregfitData = lm(trainData ~ trainingvector)
  forcastdatalm = test*as.numeric(linregfitData$coefficients[2])+as.numeric(linregfitData$coefficients[1])
  
  #apply holtwinters model
  Holtwintersdata = HoltWinters(trainData,gamma=FALSE)
  
 
  linregdata = test*as.numeric(linregfitData$coefficients[2])+as.numeric(linregfitData$coefficients[1])
  
  # the other two models
  ### TO DO
  
  # apply forecast(DO NOT EDIT)
  forecastData = forecast(fitData, h=testDatalength)
  forecastDataholtwinters = forecast(Holtwintersdata,h=length(testData))
  
 
  
  # calculate Mean Absolute Error 
  for(i in 1:length(testData))
  {
    MAE[1,i] = abs(forecastData$mean[i] - testData[i])
    MAE1[1,i] = abs(forecastDataholtwinters$mean[i]-testData[i])
    MAE2[1,i]=abs(forcastdatalm[i]-testData[i])
  }
 
  
  # this is the result you need for stock AAPL
  MAEarima<-c(MAEarima,sum(MAE[1,1:10]))
  MAElistname <-c(MAElistname,file)
  
  MAEholt<-c(MAEholt,sum(MAE1[1,1:10]))
  
   MAElin<-c(MAElin,sum(MAE2[1,1:10]))
  
}

}
 MAEdataframearima <- data.frame(MAEarima,MAElistname)
 MAEdataframearima<- MAEdataframearima[order(MAEdataframearima$MAEarima),]
 jpeg('arima.jpg')
 plot(MAEdataframearima$MAEarima[1:10], col = "blue")
 lines(MAEdataframearima$MAEarima[1:10], lw = 2, col = "red")
 dev.off()
 
 
 MAEdataframeholt <- data.frame(MAEholt,MAElistname)
 MAEdataframeholt<- MAEdataframeholt[order(MAEdataframeholt$MAEholt),]
 jpeg('hw.jpg')
 plot(MAEdataframeholt$MAEholt[1:10], col = "blue")
 lines(MAEdataframeholt$MAEholt[1:10], lw = 2, col = "red")
 dev.off()
 
 MAEdataframelin <- data.frame(MAElin,MAElistname)
 MAEdataframelin<- MAEdataframelin[order(MAEdataframelin$MAElin),]
 jpeg('lm.jpg')
 plot(MAEdataframelin$MAElin[1:10], col = "blue")
 lines(MAEdataframelin$MAElin[1:10], lw = 2, col = "red")
 dev.off()


print(MAEdataframearima[1:10,])
print(MAEdataframeholt[1:10,])
print(MAEdataframelin[1:10,])
 
