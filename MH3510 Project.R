library(dplyr)
library(ggplot2)

#Part 1: Setting up the data frame and setting categorical variables to numerical.
df=read.csv("aadt.txt",header=F,sep="")[,1:5]
colnames(df)=c("Y","X1","X2","X3","X4")
str(df)
df$X4=ifelse(df$X4==2,0,df$X4)
df$X4=as.numeric(df$X4)


#Part 2: Graphic display of Model
plot(df)


#Part 3: Setting up MLR model 
mlr=lm(Y~X1+X2+X3+X4,df)
names(mlr)
mlrs=summary(mlr)
names(mlrs)


#Part 4: Adequacy checking for fitted model
summary(mlr) 
#We note that X3:Width of road section, is not a significant predictor for aadt. With a p value of 0.421>0.05
qqnorm(residuals(mlr),ylab="Residuals")
qqline(residuals(mlr))
par(mfrow=c(1,6))
plot(residuals(mlr),ylab='Residuals',xlab='Time')
plot(residuals(mlr),fitted(mlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(mlr),df$X1,ylab="Residuals",xlab="X1")
plot(residuals(mlr),df$X2,ylab="Residuals",xlab="X2")
plot(residuals(mlr),df$X3,ylab="Residuals",xlab="X3")
plot(residuals(mlr),df$X4,ylab="Residuals",xlab="X4")
par(mfrow=c(1,1))
#Note that the residual plot against fitted values has a Y^2=X shape. 
#Thus let us make the change of variables of Y to Sqrt(Y)
#Define this new model to be newmlr
newmlr=lm(sqrt(Y)~X1+X2+X3+X4,df)
par(mfrow=c(1,6))
plot(residuals(newmlr),ylab='Residuals',xlab='Time')
plot(residuals(newmlr),fitted(newmlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(newmlr),df$X1,ylab="Residuals",xlab="X1")
plot(residuals(newmlr),df$X2,ylab="Residuals",xlab="X2")
plot(residuals(newmlr),df$X3,ylab="Residuals",xlab="X3")
plot(residuals(newmlr),df$X4,ylab="Residuals",xlab="X4")
par(mfrow=c(1,1))
#Durbin-Watson Test
library(lmtest)
dwtest(sqrt(Y)~X1+X2+X3+X4,data=df)
#From the Durbin-Watson Test, DW=1.58, p value=0.006<0.05
#Thus we reject H0 in favour of H1
#Thus it is likely that successive residuals are positively serially correlated


#Part 5: F-test for reduced model and full model
#Since above we found that X3 is not a significant model.
#We shall consider the reduced model without X3

#Part 5.1: Testing if coefficient of Beta 3 is 0
#H0: Beta3=0
reducedmlr=lm(Y~X1+X2+X4,df)
summary(reducedmlr)
anova(reducedmlr,mlr)
#p is too high, so cannot reject H0:Beta3=0
#Thus we shall use the reduced model
par(mfrow=c(1,6))
plot(residuals(reducedmlr),ylab='Residuals',xlab='Time')
plot(residuals(reducedmlr),fitted(reducedmlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(reducedmlr),df$X1,ylab="Residuals",xlab="X1")
plot(residuals(reducedmlr),df$X2,ylab="Residuals",xlab="X2")
plot(residuals(reducedmlr),df$X4,ylab="Residuals",xlab="X4")
par(mfrow=c(1,1))
plot(reducedmlr)
#Now we shall apply the same change of variables to reducedmlr
#Using sqrt(Y) instead of Y, call this model newreducedmlr
newreducedmlr=lm(sqrt(Y)~X1+X2+X4,df)
par(mfrow=c(1,6))
plot(residuals(newreducedmlr),ylab='Residuals',xlab='Time')
plot(residuals(newreducedmlr),fitted(newreducedmlr),ylab='Residuals',xlab='Fitted values')
plot(residuals(newreducedmlr),df$X1,ylab="Residuals",xlab="X1")
plot(residuals(newreducedmlr),df$X2,ylab="Residuals",xlab="X2")
plot(residuals(newreducedmlr),df$X4,ylab="Residuals",xlab="X4")
par(mfrow=c(1,1))
plot(newreducedmlr)
summary(newreducedmlr)
anova(newreducedmlr,newmlr)
#p value is 0.6023>0.05
#We do not reject H0:Beta3=0 for the new reduced model
#Thus we can continue using the new reduced model

#Part 5.2: Testing for complicated relationships
#Test if H0:2*Beta2=Beta4
newreducedmlr2=lm(sqrt(Y)~I(2*X4+X2)+X1,df)
summary(newreducedmlr2)
anova(newreducedmlr2,newreducedmlr)
#p=0.5621>0.05, thus we cannot reject H0:Beta2*2=Beta4 at the 5% Significance level


#Conclusion
summary(newreducedmlr)
newreducedmlr$coeff
#Intercept = -29.60
#Beta1 = 9.58 x 10^-5
#Beta2 = 32.86
#Beta4 = 57.22
#Final model is the following: sqrt(Y)=-29.40+(9.58x10^-5)*X1+32.86*X2+57.22*X4

#Prediction of Y using newreduced model: lm(sqrt(Y)~X1+X2+X4,df)
#Sqrt(Y')=Intercept+Beta1*50000+Beta2*32.86+Beta4*0 (Note that in our model, we changed 2 to 0)
#Y'= 5442.0129
#Thus the predicted Y for the given predictor variables is 5442.0129