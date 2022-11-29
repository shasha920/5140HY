install.packages("car")
library(car)

#Penalize R for using scietific way to display number
options(scipen=999)

#Example 10-1
butler.df<-read.csv("Data_10_Butler.csv")
bulter.lm<-lm(Time~Miles+Deliveries,data=bulter.df)
bulter.lm.summary<-summary(bulter.lm)
print(bulter.lm.summary)
bulter.df.new<-data.frame(Miles=c(85,65),Deliveries=c(3,5))
predict(bulter.lm,newdata = bulter.df.new)
predict(bulter.lm,newdata=bulter.df.new,interval="confidence",level=0.95)
predict(bulter.lm,newdata = bulter.df.new,interval="prediction",level=0.95)
vif(bulter.lm)
#Residual Analysis
butler.dv.est<-butler.lm$fitted.values
butler.res.std<-rstudent(butler.lm)
lb<-min(-3,-max(abs(butler.res.std)))
ub<-max(3,max(abs(butler.res.std)))
plot(butler.res.std~butler.dv.est,pch=19,xlab="Fitted Value of DV",
     ylab="Standardized Residual",main="Residual Analysis",ylim=c(lb,ub))
abline(h=-1.5,lty=3)
abline(h=1.5,lty=3)

#Example 10-2
johnson.df<-read.csv("Data_10_Johnson.csv")
class(johnson.df$Type.of.Repair)
levels(johnson.df$Type.of.Repari)
levels.order<-c("mechanical","electrical")
johnson.df$Type.of.Repair<-factor(johnson.df$Type.of.Repair,levels.order)
levels(johnson.df$Type.of.Repair)
johnson.lm<-lm(Repair.Time~Months.Since.Last.Service+Type.of.Repair,
               data=johnson.df)
johnson.lm.summary<-summary(johnson.lm)
print(johnson.lm.summary)
vif(johnson.lm)
#Residual Analysis
johnson.dv.est<-johnson.lm$fitted.values
johnson.res.std<-rstudent(johnson.lm)
lb<-min(-3,-max(abs(johnson.res.std)))
ub<-max(3,max(abs(johnson.res.std)))
plot(johnson.res.std~johnson.dv.est,pch=19,xlab="Fitted Value of DV",
     ylab="Standardized Residual",main="Residual Analysis",ylim=c(lb,ub))
abline(h=-1.5,lty=3)
abline(h=1.5,lty=3)