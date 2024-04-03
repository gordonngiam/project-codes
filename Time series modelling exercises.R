global=scan("globtemp.dat")
pdf("globalplot.pdf")
plot.ts(global)
dev.off()

globaldiff=diff(global)
pdf("globaldiffplot.pdf")
plot.ts(globaldiff)
dev.off()

pdf("globalacf.pdf")
acf(global)
dev.off()

pdf("globaldiffacf.pdf")
acf(globaldiff)
dev.off()

add=function(a,b){
  result=a+b
  result
}
add(2,3)

set.seed(100)
randomwalk=function(sigsq,T){
  x=rep(0,T)
  w=rnorm(T,sd=sqrt(sigsq))
  for(i in 2:T){
    x[i]=x[i-1]+w[i]
  }
  x
}
randomwalk(1,100)

arsim=function(phis,sigsq,T){
  p=length(phis) #find the no. of lags in AR
  noise=rnorm(T+p,sd=sqrt(sigsq)) #generate the white noise plus a few to get started
  x=c(noise[1:p],rep(0,T)) #put the intitial noise terms in and set the rest to zero
  for(i in (p+1):(T+p)){  #this loop generates the AR series with the recursive formula
    x[i]=phis%*%x[i-(1:p)] + noise[i]
  }
  x=x[(p+1):(T+p)] #delete initial starting points
  x
}

x1=arsim(c(0.5),1,200)
plot.ts(x1)
acf(x1)
x2=arsim(c(-0.5),1,200)
plot.ts(x2)
acf(x2)

#HOMEWORK
#Q1
masim=function(thetas,sigsq,T){
  q=length(thetas)
  noise=rnorm(T+q,sd=sqrt(sigsq))
  x=c(noise[1:q],rep(0,T))
  for (i in (q+1):(T+q)){
    x[i]=thetas%*%noise[i-(1:q)]+noise[i]
  }
  x=x[(q+1):(T+q)]
  x
}
#Q2
model=masim(c(0.5,2),1,1000)
acf(model)
#It is consistent with the model generated since q=2 so only the first 2 values in the ACF plot after the first are significant
model1=masim(c(0.5,2),1,200)
acf(model1)
#The autocorrelations around the dotted blue lines are now more significant in the values after 2
par(mfrow=c(1,2))
acf(model)
acf(model1)
#The dotted blue lines are also further away from the axis due to lesser observations.

#Q3
arima(model1)




data=read.table("cow.dat",col.names="data")
ts.plot(data)
#variation of the data is constant. However, the trend seems to be non constant and could be non stationary
#Assuming stationarity,
#ACF plot
cov=acf(data, main="ACF plot")
#ACF cuts off after Lag 7 and could be MA(7) model
#PACF plot
pacf(data,main="PACF plot")
#PACF cuts off after lag 2 and could be AR(2) model

cov=acf(data)
cov
pcov=pacf(data)
pcov
testvalue=1.96/sqrt(74)
#values agree with the test values

#Considering non stationary case:variation in data seems to be constant, however trend seems going downwards.So we apply one time difference operator to data

datadiff=diff(ts(data),differences=1)
ts.plot(datadiff) 
#looks like constant variance. Now consider ACF and PACF plots
acf(datadiff, main="ACF plot")
pacf(datadiff,main="PACF plot")
#ACF cuts off after lag 5 and the PACF cuts off after lag 6, so can be either MA(5) or AR(6) model

#Assuming stationarity, data can be either MA(7) or AR(2) model.Assuming non-stationarity, data can. be either ARIMA(0,1,5) or ARIMA(6,1,0)

#Trying MA(7) model
ma7=arima(data,order=c(0,0,7))
ma7$coef
ma7#The intercept is 53.655, mean is not 0,AIC=538

#Trying AR(2) model
ar2=arima(data,order=c(2,0,0))
ar2$coef
ar2 #The intercept is 53.86, unlikely for 0 mean, #AIC=537
#AIC for AR(2) model is lower

#Box-Pierce and Ljung-Box test
Box.test(ma7$residuals,lag=7) #p value = 0.9262
Box.test(ar2$residuals,lag=2) #p value = 0.4841
#Both p values are high, so do not reject H0: independent residules

#taking the data to be non-stationary
#Testing ARIMA(0,1,5)
arima015=arima(data,order=c(0,1,5))
arima015$coef
arima015
#Testing ARIMA(6,1,0)
arima610=arima(data,order=c(6,1,0))
arima610$coef
arima610
#Box-Pierce and Ljung-Box Test
Box.test(arima015$residuals,lag=5) #p value is 0.7077
Box.test(arima610$residuals, lag=6) #p value is 0.977
#Both have high p values so do not reject H0: independent residuals

AIC(arima015) #528
AIC(arima610) #530

#If assume the data is stationary, choose MA(7) model. X_t=0.204X_t-1 + 0.349X_t-2 + 0.153X_t-3 + 0.436X_t-4 + 0.061X_t-5 + 0.041X_t-6 + 0.380X_t-7 + 54 
#If assume the data is non-stationary, choose ARIMA(0,1,5) model. X_t=X_t-1 + Zt - 0.752 Z_t-1 + 0.190Z_t-2 + 0.029Z_t-3 - 0.111 Z_t-4 - 0.196 Z_t-5
#ARIMA(0,1,5) has the lowest AIC out of all 4 candidate models, so it is preffered out of the models
