# RELIANCE.NS$Date=as.Date(RELIANCE.NS$Date, format = "%d-%m-%Y")
# ts=ts(RELIANCE.NS,frequency=365)
# plot.ts(ts)
# composition.ts=decompose(ts)
# decomposed.ts=ts-composition.ts$seasonal
# plot(decomposed.ts)

#installing packages
install.packages("dynlm")
library(dynlm)
install.packages("forecast")
library(forecast)
install.packages("aTSA")
library(aTSA)
library(forecast)
library(urca)
library(readr)
install.packages("zoom")
library(zoom)
library(ggplot2)
install.packages("ggforce")
library(ggforce)

#importing data
RELIANCE_NS <- read_csv("C:/Users/pddes/Desktop/ae 3/RELIANCE.NS.csv")
View(RELIANCE_NS)
adj.closing <- RELIANCE_NS$`Adj Close`
plot(adj.closing) #Looks like multiplicative model
ln_adj.closing = log(adj.closing) #Log transformation
plot (ln_adj.closing)

#creating time series objects
adj.closing_ts <- ts(adj.closing, frequency=365)
ln_adj.closing_ts <- ts(ln_adj.closing, frequency=365)

#using auto.arima() function to determine p,d,q values
auto.arima (adj.closing_ts)
auto.arima (ln_adj.closing_ts) #Better Model

# diff_series= diff(ln_adj.closing, k=1)
# L1 = lag(diff_series,1)
# reg1=dynlm(diff_series~L1,data=ln_adj.closing_ts)
# a=glance(reg1)[c("r.squared","statistic","AIC","BIC")]
# diff_series2= diff(adj.closing,5)
# l1= lag(diff_series2,1)
# l2= lag(diff_series2,2)
# l3= lag(diff_series2,3)
# l4= lag(diff_series2,4)
# l5= lag(diff_series2,5)
# reg2=dynlm(diff_series2~l1+l2+l3+l4+l5,adj.closing_ts)
# (b=glance(reg2)[c("r.squared","statistic","AIC","BIC")])
# judge=data.frame(rbind(a,b))
# names(judge)=c("with log","original data")
# judge #log model has lower BIC value

# bgtest(reg1,order=1,type="F",fill=0) #failed to reject, so no serial corr in log model

#using arima() function to fit an arima model
reg_arima=arima(ln_adj.closing_ts,order=c(1,1,0))
(fcst=forecast(reg_arima,h=5,level=0.05))

#deseasonalizing the data
ln_adj.closing_ts2 = na.locf(ln_adj.closing_ts)
dc = decompose(ln_adj.closing_ts2)
dc_ln = ln_adj.closing_ts2 - dc$seasonal
plot.ts(dc_ln)
plot.ts(ln_adj.closing_ts2)

#checking arima model for deseasonalized data
auto.arima(dc_ln) 
#suggests an ARIMA(0,1,1) model with drift
reg_ma= Arima(dc_ln,order=c(0,1,1), include.drift=TRUE) #using Arima() function with the include.drift=TRUE parameter
(fcst_ma= forecast(reg_ma,h=5,level=99.5)) 
#forecasting values for 5 days ahead

#Ljung-Box test to test for serial correlation in the forecast residuals
Box.test(fcst_ma,type="Ljung-Box",lag=1) 
#failed to reject null hypothesis of no serial correlation

#stationary test
dc_ln_d1=diff(dc_ln) 
#differencing the series once, as suggested by auto.arima function
stationary.test(dc_ln_d1) 
#successful

#making plots
plot(Acf(fcst_ma$residuals))
plot.ts(ln_adj.closing_ts2)
plot.ts(dc_ln)
plot.ts(dc)

#making graphs for the forecasted values
autoplot(fcst_ma)+autolayer(dc_ln) + facet_zoom(xlim=c(19.1,19.175),ylim=c(7.7,8.1))
plot(fcst_ma,xlim=c(19.1,19.175),ylim=c(7.7,8.1))