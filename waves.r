library(ggplot2)
library(hexbin)

#Loading data
waves <- read.csv('weatherbuoy.csv') #reading in csv
#checking data
head(waves) 
class(waves)
summary(waves)

#Cleaning data
waves <- na.omit(waves) #removing NaN values from dataframe
waves <- waves[1:10000,] #taking first 10,000 rows
head(waves)

#Exploring data
pairs(waves[5:14], pch = 21) #Checking relationship of variables


#Moving Average
moving_average <- function(x, n = 7) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}
waves$MVA <- moving_average(waves$WindSpeed) #initiating moving average value as new column in dataframe
waves$MVA

#plotting MVA versus actual (first 1000 only)
plot(waves$WindSpeed[0:100],type = "l",col = "red", ylab = "Speed", xlab = "Time", main = "Wind Speed with Moving Average")
lines(waves$MVA[0:100], type = "l", col = "blue")
legend("topleft", legend=c("Wind Speed", "Moving Average"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#plot graph
plot(waves$WaveHeight, waves$WindSpeed, pch=21,       
     bg=c("red","blue"), 
     main="Relationship between wave height and wind speed", 
     xlab="Height", ylab="Speed")

#hexo plot
ggplot(waves, aes(WaveHeight, WindSpeed)) +
  geom_hex( bins=30 )


#Linear Regression
LinReg <- lm(WaveHeight ~ WindSpeed, data = waves) #model
LinReg #results

summary(LinReg) #further results
confint(LinReg) #confidence intervals

waves$prediction <- predict(LinReg) #initiating prediction results
waves$difference <- waves$WaveHeight - waves$prediction #difference between results

#plotting actual vs prediction (first 1000 only)
plot(waves$WaveHeight[0:1000],type = "l",col = "red", ylab = "Height", xlab = "Time", main = "Actual vs Predicted Wave Height")
lines(waves$prediction[0:1000], type = "l", col = "blue")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

#testing different wind speeds for wave height results
speeds <- data.frame(WindSpeed <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60))
predict(LinReg, speeds)

#plotting relationship
plot(WaveHeight ~ WindSpeed, data = waves)
abline(LinReg)

yint = LinReg$coefficients[1] #y intercept
slope = LinReg$coefficients[2] #slope

#forecast funtion
forecast <- function(x) {
  y = yint + slope * x
  return(y)
}
forecast(7)