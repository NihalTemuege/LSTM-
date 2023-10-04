setwd("C:/Users/rneb3bv/Desktop/Inflation_Trade_Thesis")
#install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
library(data.table)
US_data <- read_excel("US_Monthly(gm).xlsx")

US_data$Date <- format(as.Date(US_data$Date), "%Y/%m/%d")

head(US_data)
US_data <- na.omit(US_data)

US_data$d_CPI <-US_data$CPI - lag(US_data$CPI,-1)
US_data$d_CPI <-shift(US_data$d_CPI,n= 1 , fill = NA , type="lag" )
US_data$oil_yy <- US_data$`Crude oil`- lag(US_data$`Crude oil`,-12)
US_data$oil_yy <-shift(US_data$oil_yy,n= 12 , fill = NA , type="lag" )
US_data <- na.omit(US_data)

#OLS 


reg1<- lm(d_CPI  ~ Output_gap  + lag(oil_yy,3L)+ lag(NEER, 3L) + lag(World_cpi, 3L) + lag(US_Michigan_5y,3L) ,  data = US_data )
summary(reg1)

reg2<- lm(d_CPI  ~ Output_gap  + oil_yy + NEER + World_cpi + US_Michigan_5y ,  data = US_data )
summary(reg2)

reg3<- lm(CPI  ~ Output_gap  + oil_yy + NEER + World_cpi + US_Michigan_5y ,  data = US_data )
summary(reg3)

stargazer(reg1,reg2, reg3, type = "html" , dep.var.labels=c("CPI") , title="Regression Results", digits=1,out="US_OLS_gm2.html")

mse <- function(reg3) 
  mean(reg3$residuals^2)
mse(reg1)
###

install.packages("tseries")
library(ggplot2)
library(tidyverse)
library(vars)
library(stargazer)
library(forecast)
library(tseries)
library(sandwich)
library(lmtest)
##VAR for the US

data<-read_excel("US_Monthly(gm).xlsx")
data$Date <- format(as.Date(data$Date), "%d/%m/%Y")
data<-as.data.frame(data)

View(data)


nrow(data)
View(data)
data<- drop_na(data)
nrow(data)
train_us <- data[1:129,]
test_us <- data[130:242,]
sum(is.na(test_us))




#The first step is to choose an appropriate number of lags for the model. Since Ivanov and Kilian (2005) sugests that Akaike's Information Criterion (AIC) tends to produce the most accurate structural and semi-structural impulse response, I will be using this criterion.



CPI <- train_us[, 2]
#US_data<-drop_na(US_data)
predictors <- length(data)
data.var.order <- data[, 2:predictors] #Column 1 is a dates array
var.order <- VARselect(data.var.order, type = "const", lag.max = 12)[["selection"]]
var.order
var.order <- as.numeric(var.order[1]) #var.order[1] = AIC
data.var <- US_data[1:(nrow(train_us) + var.order), 2:predictors]

sum(is.na(data.var))
sum(is.na(test_us))
test_us
m.var <- VAR(data.var, p = var.order) 
m.var
checkresiduals(m.var$varresult$CPI)


# # Assuming `var_model` is the estimated VAR(11) model object
# nw_cov <- vcovHC(m.var, method = "HAC", type = "HC0", sandwich = FALSE)
# nw_test <- coeftest(m.var, vcov. = nw_cov)
# nw_test
# NeweyWest


#ADF Test

adf.test(diff(train_us[,"CPI"]))
adf.test(diff(train_us[,"Crude oil"]))
adf.test(diff(train_us[,"Output_gap"]))
adf.test(diff(train_us[,"NEER"]))
adf.test(diff(train_us[,"World_cpi"]))
adf.test(diff(train_us[,"US_Michigan_5y"]))




plotdate <- as.Date(data$Date[1:nrow(train_us)])
m.var.resid <- as.data.frame(m.var$varresult$CPI$residuals)
colnames(m.var.resid) <- 'x'
resid.acf <- acf(resid(m.var))
resid.plot <- ggplot(data=m.var.resid, aes(x=plotdate, y=x)) + 
  geom_line() + labs(x = 'Residuals', y = 'Date', title ='US inflation residuals from VAR(11) model') + theme_bw()
resid.plot

acf.plot <- autoplot(resid.acf, main = 'US VAR(11) residuals autocorrelation function') + theme_bw()
acf.plot
resid.dist.plot <- ggplot(data=m.var.resid, aes(x)) +
  geom_histogram(aes(y=..density.., fill = ..count..)) + 
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(as.ts(m.var.resid)), 
                          sd=sd(as.ts(m.var.resid)))) +
  labs(x = 'Residuals', y = 'Density', title = 'US VAR(11) residuals distribution') + theme_bw()
resid.dist.plot

mean(as.ts(m.var.resid))

norm.test <- jarque.bera.test(as.ts(m.var.resid))
norm.test
fcast.var <- forecast(m.var$datamat$CPI, h = 12)
autoplot(as.ts(CPI), series = 'Data', 
         main = 'US inflation VAR(11) forecast') +
  autolayer(fitted(fcast.var), series = 'Fitted') +
  autolayer(fcast.var, series = 'Forecast') +
  xlab('Time Period') + ylab('Inflation') + theme_bw()

pred.var.us <- fcast.var$mean
nrow(fcast.var$mean)
error.var.us <- (fcast.var$mean[1:12])-(test_us[c(115:126),2]) #Inflation is in column 2
autoplot(error.var.us, main = 'US inflation VAR(11) forecast errors', color = 'darkred') + 
  geom_line(y= c(0,0,0,0,0,0,0,0,0,0,0,0)) +
  xlab('Time Period') + ylab('Prediction error') + theme_bw()
sum(error.var.us)/12
################

CPI <- data[, 2]
predictors <- length(data)
data.var.order <- data[, 2:predictors] #Column 1 is a dates array
var.order <- VARselect(data.var.order, type = "const", lag.max = 12)[["selection"]]
var.order <- as.numeric(var.order[1]) #var.order[11] = AIC
data.var <- data[(1 ) :(nrow(train_us)), 2:predictors]
m.var <- VAR(data.var, p = var.order) 
m.var
fcast.var <- forecast(m.var$datamat$CPI, h = 12)
var.fitted <- m.var$varresult$CPI$fitted.values
var.fitted <- as.ts(var.fitted)
CPI <- data[(var.order+1):nrow(data), 2]
autoplot(var.fitted, series = 'Fitted', 
         main = 'US inflation VAR(11) forecast') +
  autolayer(fcast.var, series = 'Forecast') +
  autolayer(as.ts(CPI), series = 'Data') +
  xlab('Time Period') + ylab('Inflation') + theme_bw()
autoplot(var.fitted , color = 'darkred', series = 'fitted', 
         main = 'US VAR(11) inflation forecast') +
  autolayer(fcast.var, color = 'steelblue', series = 'forecast') +
  autolayer(as.ts(CPI), color = 'black' , series = 'observed') +
  xlab('Time Period') + ylab('') +
  theme_bw()
nrow(test_us)
fcast.var$mean <- as.list(fcast.var$mean, ncol=1)
fcast.error <- (fcast.var$mean)-(test_us[c(103:114),2])
autoplot(fcast.error, main = 'US inflation VAR(11) forecast errors', color = 'darkred') + 
  geom_line(y= c(0,0,0,0,0,0,0,0,0,0,0,0)) +
  xlab('Time Period') + ylab('Prediction error') + theme_bw()
pred.var.us <- fcast.var$mean
error.var.us <- fcast.var$mean - test_us[115:126,2]
sum(error.var.us)/12
mse<- sqrt(sum(error.var.us^2/12))
mse



