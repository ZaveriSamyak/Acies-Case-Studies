#Setting up working directory
options(max.print = 1000000)
setwd("D:/samyak world/Acies R Proj")
getwd()

#Install following packages:
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("moments")
# install.packages("reshape2")
# install.packages("lmtest")
# install.packages("tseries")
# install.packages("car")
# install.packages("fdaACF")

#Step 1: Obtaining data for Portfolio value from .csv file
pv = read.csv("Portfolio values.csv")

#Performing Exploratory Data Analysis
head(pv)
str(pv)
dim(pv)
colnames(pv)
tail(pv)
summary(pv)

#Step 2: Obtaining data for Forecasts & Historical Macroeconomic Variables from a .xlsx file
library(readxl)
excel_sheets("ForecastsforMacroeconomicVariables.xlsx")

forecast_base = read_excel("ForecastsforMacroeconomicVariables.xlsx", "Baseline" )
forecast_adv = read_excel("ForecastsforMacroeconomicVariables.xlsx", "Adverse " )
forecast_severe = read_excel("ForecastsforMacroeconomicVariables.xlsx", "Severely Adverse" )

head(forecast_base)
head(forecast_adv)
head(forecast_severe)

Historical = read_excel("HistoricalMacroeconomicData.xlsx")

#Performing Exploratory Data Analysis
head(Historical)
str(Historical)
dim(Historical)
colnames(Historical)
tail(Historical)
summary(Historical)

#Step 3: Sorting data according to dates
class(Historical$Date)


#Since the date is a character variable we need to turn it into numerical date

library(tidyverse)

Hist_sep = separate(Historical,Date,into = c("Quarters","Year"),sep = " ")

head(Hist_sep)

Hist_sep[Hist_sep$Quarters=="Q1","Quarters"] = "1/1/"

Hist_sep[Hist_sep$Quarters=="Q2","Quarters"] = "1/4/"

Hist_sep[Hist_sep$Quarters=="Q3","Quarters"] = "1/7/"

Hist_sep[Hist_sep$Quarters=="Q4","Quarters"] = "1/10/"

head(Hist_sep)

#Now since we've replaced quarters into numerical format
#we can match it with it's respective years.

library(stringr)

Hist_sep$combined <- str_c(Hist_sep$Quarters, Hist_sep$Year)

Hist_new = subset(Hist_sep, select=  -c(Quarters,Year))

colnames(Hist_new)[26] <- "Date"

df_Hist <- Hist_new %>%
  select(Date, everything())

head(df_Hist)

df_Hist$Date = as.Date(df_Hist$Date, "%m/%d/%Y")

class(df_Hist$Date)


#Portfolio values face a similar issue. 
#As a result, we can repeat the process to sort dates.

pv_sep = separate(pv,Quarter,into = c("Quarters","Year"),sep = " ")
head(pv_sep)

pv_sep[pv_sep$Quarters=="Q1","Quarters"] = "1/1/"

pv_sep[pv_sep$Quarters=="Q2","Quarters"] = "1/4/"

pv_sep[pv_sep$Quarters=="Q3","Quarters"] = "1/7/"

pv_sep[pv_sep$Quarters=="Q4","Quarters"] = "1/10/"

head(pv_sep)

library(stringr)
pv_sep$combined <- str_c(pv_sep$Quarters, pv_sep$Year)

pv_new = subset(pv_sep, select=  -c(Quarters,Year))

dim(pv_new)

colnames(pv_new)[2] <- "Date"

df_pv <- pv_new %>%
  select(Date, everything())

head(df_pv)

class(df_pv$Date)

df_pv$Date = as.Date(df_pv$Date, "%m/%d/%Y")

class(df_pv$Date)

#Step 4: Filtering Macroeconomic data as per availability of Portfolio Values.
library(dplyr)

Hist_data = df_Hist %>%
  filter(df_Hist$Date > "2004-01-10")

#We've date column common between Portfolio values and Historical data which we can use to join both data sets as one data frame in R.

#Step 5: Joining both the data sets.
MyData <- merge(Hist_data,df_pv,by="Date")

#Saving this data set as a .csv file for future use.
write.csv(MyData,"D:/samyak world/Acies R Proj/My Data.csv", row.names = FALSE)

#Step 6: Analyzing the portfolio value using graphs
plot(df_pv, type = "l", lty = 1, xlab="Date", ylab="Portfolio Values", main="Analyzing Portfolio Values", col="Blue")

#The line plot tells us the following:
#there are NA/ Missing values in the dataset
#there are few major outliers in the dataset
#still we can confirm it using analytical methods

#Step 7: checking missing values from the data set
any(is.na(MyData))

#checking finally if there are any non-numeric data
sapply(MyData,function(y) class(y))

#we found out that portfolio values has character values
#But we already know that the values are numerical
#so we can expect few non-missing value/ unreliable values

a <- which(MyData$Portfolio.Values=="?")

#Step 8: Replacing missing values using linear interpolation technique method considering this is time series data

MyData1 <- MyData 

#Formula- Y = Y1 + (X-X1)*(Y2-Y1)/(X2-X1)
NA1 = 1057.08 + ((20 - 19)*(1169.43-1057.08))/(21-19)
NA1

NA2 = 2098.86 + ((47-46)*(2238.83-2098.86))/(48-46)
NA2

MyData1[a,27] <- c(NA1, NA2)
MyData1[,27] <- as.numeric(MyData1[,27]) 

#Checking if the values are replaced properly
MyData1$Portfolio.Values
dim(MyData) 
dim(MyData1)
class(MyData1$Portfolio.Values)

plot(MyData1$Portfolio.Values, type = "l", lty = 1, xlab="Date", ylab="Portfolio Values", main="Analyzing Portfolio Values", col="Blue")

## Detecting outliers using direct R command
MyData_out = boxplot(MyData1$Portfolio.Values,plot = FALSE)$out
MyData_out

#Step 9: Replacing the values by changing decimal places
MyData1[c(35,36),27] <- c(1681.55, 1848.36)  
MyData1

CleanedData <- MyData1

#Step 10: Analyzing historical portfolio using central tendency measures
summary(CleanedData)

#we can get mean, median and mode for each column and other statistical values to help us infer about how efficient and reliable our data is.

#summary also considers NA values while making calculations but since the data is cleaned and ready we can easily analyze the data

library(psych)
#Few more statistical values for understanding the data set.
describe(CleanedData, type=2) 

library(moments)
skewness(CleanedData$Portfolio.Values)

#High kurtosis indicates more outliers & vice-versa for low kurtosis

#Mesokurtic has properties similar to normal distribution (K=3)
#Leptokurtic indicates more outliers (K>3)
#Platykurtic lacks outliers (K<3)

kurtosis(CleanedData$Portfolio.Values) #2.5681 (which is <3)
#We follow a platykurtic kurtosis which indicates lesser outliers in the dataset

#Visual Representation of the data

hist(CleanedData$Portfolio.Values, col="red", main="Analysing Historical Portfolio values", xlab = "Portfolio Values", ylab = "Number of Portfolios")

plot(CleanedData$Date, CleanedData$Portfolio.Values, type = "l", lty = 1, xlab="Date", ylab="Portfolio Values", main="Analyzing Portfolio Values", col="Blue")

barplot(CleanedData$Portfolio.Values, main = "Analyzing Portfolio Values", xlab = "Date", ylab = "Portfolio Values", names.arg= CleanedData$Date, col = "darkred")

#Step 11: Correlation Analysis between Independant & Dependant Data
sapply(CleanedData,function(y) class(y))

#Removing the date data as correlation only approves numerical data
CorrData = CleanedData[-1]

#Correlation analysis
mcor<-round(cor(CorrData),2)
Portfolio.corr <- mcor[ ,26]

#Step 12: Choosing variables with a correlation coefficient of higher than absolute value of 0.6
Portfolio.corr[abs(Portfolio.corr)>(0.6) & Portfolio.corr != 1]

# "Commercial Real Estate Price Index"
# "Money Supply"                      
# "consumer confidence index"  
# (USD/euro) #-0.62
#"Business Confidence Index"
# BBB Corporate Yield


#Step 13: Performing linear regression analysis on these independent variables w.r.t the portfolio valur which is our dependent variable

lmCREPI = lm(CleanedData$Portfolio.Values~ CleanedData$`Commercial Real Estate Price Index`)
summary(lmCREPI) #p-value: 7.156e-12
resettest(lmCREPI)

lmMoney = lm(CleanedData$Portfolio.Values~ CleanedData$`Money Supply`)

summary(lmMoney) #p-value: 2.994e-16
resettest(lmMoney)

lmCCI = lm(CleanedData$Portfolio.Values~ CleanedData$`consumer confidence index`)
summary(lmCCI) #p-value: 4.352e-13
resettest(lmCCI)

lmBCY = lm(CleanedData$Portfolio.Values~ CleanedData$`BBB Corporate Yeild`)
summary(lmBCY) #p-value: 1.219e-09
resettest(lmBCY)

lmBCI = lm(CleanedData$Portfolio.Values~ CleanedData$`Business Confidence Index`)
summary(lmBCI) #p-value: 4.316e-07
resettest(lmBCI)

lmUSD = lm(CleanedData$Portfolio.Values~ CleanedData$`(USD/euro)`)
summary(lmUSD) #p-value: 1.203e-06
resettest(lmUSD)

#Since all the values are lesser than 0.05 we do not remove any variables

#Step 14: Plotting line graphs of dependent variable against each chosen independent variable to see relationship between them.

library(reshape2)
df1 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`Commercial Real Estate Price Index`)
df2 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`Money Supply`)
df3 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`consumer confidence index`)
df4 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`(USD/euro)`)
df5 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`Business Confidence Index`)
df6 <- data.frame(Date=CleanedData$Date, CleanedData$Portfolio.Values, CleanedData$`BBB Corporate Yeild`)

#melt data frame into long format
df1 <- melt(df1,  id.vars = 'Date', variable.name = 'series')
df2 <- melt(df2,  id.vars = 'Date', variable.name = 'series')
df3 <- melt(df3,  id.vars = 'Date', variable.name = 'series')
df4 <- melt(df4,  id.vars = 'Date', variable.name = 'series')
df5 <- melt(df5,  id.vars = 'Date', variable.name = 'series')
df6 <- melt(df6,  id.vars = 'Date', variable.name = 'series')

#create line plot for each column in data frame
ggplot(df1, aes(Date, value)) +
  geom_line(aes(colour = series))

ggplot(df2, aes(Date, value)) +
  geom_line(aes(colour = series))
#df2 which is money supply is an unintuitive graph

ggplot(df3, aes(Date, value)) +
  geom_line(aes(colour = series))

ggplot(df4, aes(Date, value)) +
  geom_line(aes(colour = series))

ggplot(df5, aes(Date, value)) +
  geom_line(aes(colour = series))

ggplot(df6, aes(Date, value)) +
  geom_line(aes(colour = series))


#Step 15: Performing regression on all the independent variables at once.
lmAll = lm(CleanedData$Portfolio.Values~ CleanedData$`Commercial Real Estate Price Index` + CleanedData$`Money Supply` + CleanedData$`BBB Corporate Yeild` + CleanedData$`consumer confidence index`+ CleanedData$`(USD/euro)`+ CleanedData$`Business Confidence Index`)
summary(lmAll)

#Step 16: Yes, all the variables have p-values lesser than 0.05 and hence no variables are needed to be removed.

#Step 17: Checking for high correlation between these list of variables
head(CleanedData)

Corrdata = CleanedData[c(11,15,17,22,23,24)]

Corrdata1 = cor(Corrdata)
Corrdata1

#Since money supply has high correlation with other variables & the graph plotted is unintuitive it has to be removed.

#Step 18: Removing money supply that has high correlation and choosing only those variables with lower p-values

Iteration1 = lm(CleanedData$Portfolio.Values~ CleanedData$`Commercial Real Estate Price Index` + CleanedData$`BBB Corporate Yeild` + CleanedData$`consumer confidence index`+ CleanedData$`(USD/euro)`+ CleanedData$`Business Confidence Index`)
summary(Iteration1)

#Since Business Confidence Index & USD/euro have high p-values so we'll remove them too.

Iteration2 = lm(CleanedData$Portfolio.Values~ CleanedData$`Commercial Real Estate Price Index` + CleanedData$`BBB Corporate Yeild` + CleanedData$`consumer confidence index`)
summary(Iteration2)

#To get an efficient linear model we may also remove consumer confidence index

Iteration3 = lm(CleanedData$Portfolio.Values~ CleanedData$`Commercial Real Estate Price Index` + CleanedData$`BBB Corporate Yeild`)
summary(Iteration3)
resettest(Iteration3, type="regressor")


#Step 19: Checking for linearity of model using Ramsay-Reset Test

#Ramsay Reset test
#H0: There is linearity in our model
#H1: Model lacks for linearity
library("lmtest")

resettest(lmCREPI)
resettest(lmBCY)
resettest(lmCCI)
resettest(lmBCI)
resettest(lmUSD)

#As we can see that only BBB corporate Yield and Commercial Real Estate Price Index have p-values have lesser than 0.05 p-value which means our selection is appropriate.

#Applying log transformation to the dependent variable
CleanedData$logPV = log(CleanedData$Portfolio.Values)
head(CleanedData)

Iteration4 = lm(CleanedData$logPV~ CleanedData$`Commercial Real Estate Price Index` + CleanedData$`BBB Corporate Yeild`)
summary(Iteration4)
resettest(Iteration4, type="regressor")

#Log transformation has improved our results of our regression model

#Now applying log transformation to independent non-linear variables.
CleanedData$logRealEstate = log(CleanedData$`Commercial Real Estate Price Index`)
CleanedData$logCorporateYield = log(CleanedData$`BBB Corporate Yeild`)
head(CleanedData)

FinalModel = lm(CleanedData$logPV~ CleanedData$logRealEstate + CleanedData$logCorporateYield)
summary(FinalModel)
resettest(FinalModel, type="regressor")

head(CleanedData)

#Step 20: Conducting Quantitative & Qualitative statistical tests on the final model to test for assumptions

#RAMSAY RESET
resettest(FinalModel,type="fitted",data = CleanedData)

kpss.test(log(CleanedData$Portfolio.Values))

#The model is stationary
pp.test(log(CleanedData$Portfolio.Values))

#BREUSCH PAGAN TEST
bptest(FinalModel)

library(tseries)

#JARQUE BERA TEST
jarque.bera.test(log(CleanedData$Portfolio.Values))

#ADF TEST
adf.test(log(CleanedData$Portfolio.Values))

#DURBIN WATSON TEST
dwtest(FinalModel)

#SHAPIRO
shapiro.test(log(CleanedData$Portfolio.Values))

#The relationship between the fitted values and residuals is largely a straight line indicating a linear relationship between dependent and independent variables
#For Outlier Analysis, fitted values & QQ plot
par(mfrow = c(2,2))
plot(FinalModel)
par(mfrow = c(1,1))

#ACF values die out at lag 2, PACF values die out at lag 6. 
library("fdaACF")

acf(FinalModel$residuals)

pacf(FinalModel$residuals)

CleanedData$Portfolio.Values

#Step 21: Performing sample Analysis

#In Sample Analysis
predicted.values = predict(FinalModel)

prediction = exp(predicted.values)

df_predict = data.frame(prediction)

head(df_predict)

#Visual representation of predicted values
par(mfrow= c(1,2))
plot(CleanedData$Portfolio.Values, type="l", col="red", ylab="Portfolio Values")

plot(df_predict$prediction, type="l", col="green", ylab="Predicted Values")

par(mfrow= c(1,1))

#Out Sample Analysis

#step 22: Conducting sample analysis using 13 years of data to train the model & 2 years of data to test model results

Finaldata = CleanedData[-c(28,29,30)]

Trainingdata = Finaldata[1:42, ]
head(Trainingdata)

Testingdata = Finaldata[43:52, ]
head(Trainingdata)

#Building a Training model
Training.model = lm(log(Trainingdata$Portfolio.Values) ~ log(Trainingdata$`Commercial Real Estate Price Index`)+log(Trainingdata$`BBB Corporate Yeild`))
summary(Training.model)

Training.model$coefficients

#Sample Analysis: Y = A + BX1 + CX2
#Y is the dependent variable
#A is our intercept
#X1 and X2 are independent variables
#B & C are the slope of the line

A = 3.6630432
B = 0.9289497
C = -0.8054579

Y = A + B*log(Testingdata$`Commercial Real Estate Price Index`) + C*log(Testingdata$`BBB Corporate Yeild`)
Y

#Since these are log values we need to apply exp
fitted_values = exp(Y)
fitted_values

#Errors = Observed data - Predicted data
Error1 = Testingdata$Portfolio.Values - fitted_values 
Error1

#Absolute Errors
Error2 = abs(Error1/Testingdata$Portfolio.Values)
Error2

#Mean Square Errors
Error3 = mean(Error1^2)
Error3

#Step 23: Forecasting portfolio values using model calibration results and forecasts provided under three scenarios

head(forecast_adv)
adv.model = A + B*log(forecast_adv$`Commercial Real Estate Price Index`) + C*log(forecast_adv$`BBB Corporate Yeild`)
Adverse1 = exp(adv.model)

head(forecast_base)
Base.model = A + B*log(forecast_base$`Commercial Real Estate Price Index`) + C*log(forecast_base$`BBB Corporate Yeild`)
Base1 = exp(Base.model)

head(forecast_severe)
severelyadv.model = A + B*log(forecast_severe$`Commercial Real Estate Price Index`) + C*log(forecast_severe$`BBB Corporate Yeild`)
Severe1 = exp(severelyadv.model)

FinalForecast = data.frame("Quarter"= forecast_adv$Date, Adverse1, Base1, Severe1)
head(FinalForecast)

#Subsetting the values
InitialPV = Finaldata$Portfolio.Values[Finaldata$Date=="2017-01-10"]
InitialPV

QAdv = FinalForecast$Adverse1[FinalForecast$Quarter=="Q1 2020"]
QAdv

QBase = FinalForecast$Severe1[FinalForecast$Quarter=="Q1 2020"]
QBase

QSevere = FinalForecast$Base1[FinalForecast$Quarter=="Q1 2020"]
QSevere

#Calculating losses

lossAdv = InitialPV - QAdv
lossAdv

lossBase = InitialPV - QBase
lossBase

lossSevere = InitialPV - QSevere
lossSevere

#Step 24: Computing Required Capital as per formula in Case study

Required.Capital = 0.2*(0.35*lossAdv + 0.5*lossBase + 0.15*lossSevere)
Required.Capital
