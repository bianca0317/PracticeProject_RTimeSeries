#load library for extracting
library(quantmod)
library(xts)
library(tseries)
library(lmtest)
library(forecast)
library(moments)


#extract from yahoo finance
getSymbols(Symbols="PSEI.PS",src="yahoo",from="1972-01-31",to=Sys.Date(), periodicity="monthly")
#output is xts object
class(PSEI.PS)

#check output, gets the start of every month
head(PSEI.PS)
tail(PSEI.PS)
#use only close
psei=PSEI.PS[,"PSEI.PS.Close"]
plot(psei)

#cut data to get only training portion, the rest is for comparing forecasts later on
psei_train=psei["1987/2019-05"]
first(psei_train)
#remove na
psei_train=na.omit(psei_train)
#get number of working data points
length(psei_train)

#check if stationary
adf.test(psei_train,k=0,alternative="stationary")

#detrend
psei_train_ret=diff(log(psei_train))
psei_train_ret=na.omit(psei_train_ret)

#check if stationary after one differencing  
plot(psei_train_ret)
adf.test(psei_train_ret,k=0,alternative="stationary")
#results indicate stationary, i.e. reject the null hypothesis that it is non-stationary
#hence, difference order = 1

#use auto.arima() first before manual process
fit0=auto.arima(log(psei_train))
fit0
coeftest(fit0)
acf(fit0$residuals)
Box.test(fit0$residuals, lag=20, type="Ljung-Box")


#check ACF for MA level
acf(psei_train_ret)
#cuts off after lag=1

#check PACF for AR level
pacf(psei_train_ret)

#loops possible models
p=0:1 #AR levels to test
q=0:1 #MA levels to test
aic=matrix(nrow=2,ncol=2)
for (i in p) {
  for (j in q) {
    aic[i+1,j+1]= arima(log(psei_train), order=c(i, 1, j))$aic
  }
}
aic
#find model with lowest AIC
min(aic)

#closer look into "best" model
fit <- arima(log(psei_train), order=c(1, 1, 0))
coeftest(fit)
#and acf of residuals is 0 for non-zero lag
acf(fit$residuals)
Box.test(fit$residuals,lag=20)

#predict using model with lowest AIC
pred <- predict(fit, n.ahead = 20)

#convert forecasts to prices
fore=forecast(fit,h=20)
point=exp(fore$mean)
lower=exp(fore$lower)
upper=exp(fore$upper)
forecast_prices=cbind(point,lower,upper)

#plots

ts.plot(as.ts(psei_train), forecast_prices, col=c("black","blue","purple","orange","purple","orange"), lty=c(1,3,3,3,3,3))

#get actual
psei_test=psei["2019-05/"]
psei_test
length(psei_test)

ts.plot(as.ts(psei_train), as.ts(psei_test,start=390), forecast_prices, col=c("black","red","blue","purple","orange","purple","orange"), lty=c(1,1,3,3,3,3,3))


