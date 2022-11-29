#Example 9-1
options(scipen=999)
armands.df<-read.csv("Data_09_Armand's.csv")
#Dependent variable(y): Sales(in $000s)
#Independent variable(x): Population(in 000s)
#Simple linear regression model:
#y=beta.0+beta.1*x+epsilon
#Simple linear regression equation:
#E(y)=beta.0+beta.1*x
#Estimated simple linear regression equation:
#y.hat=b0+b1*x
#Equivalently
#Sales=b0+b1*Population
armands.lm<-lm(Sales~Population,data=armands.df[c(2,3)])
#Summarize the model:
armands.lm.summary<-summary(armands.lm)
armands.lm.summary
#Predict new record:
x.star<-16
armands.df.new<-data.frame(Population=x.star)
print(paste("Prediction for population= ",x.star,":sales= ",
            predict(armands.lm,armands.df.new),sep = ""))
#R squared and sample correlation coefficient:
print(paste("Coeficient of determination (r^2):",
            armands.lm.summary$r.squared))
print(paste("Sample correlation coefficient:",
            sign(armands.lm.summary$coefficients["Population","Estimate"])
            *sqrt(armands.lm.summary$r.squared)))
#t test and F test:
print(armands.lm.summary)
#Confidence interval and prediction interval:
x.star<-30
armands.df.new<-data.frame(Population=x.star)
predict(armands.lm,armands.df.new)
print(paste("Confidence interval for population= ",x.star,":",sep = ""))
predict(armands.lm,armands.df.new,interval = "confidence",level=0.95)
print(paste("Prediction interval for population= ",x.star,":",sep=""))
predict(armands.lm,armands.df.new,interval = "prediction",level=0.95)
#Visualize residuals against IV and DV:
plot(armands.lm.summary$residuals~armands.df$Population,pch=19,
     xlab="Population",ylab="Residual",main="Residual Analysis")
plot(armands.lm.summary$residuals~armands.df$Sales,pch=19,
     xlab="Sales",ylab="Residual",main="Residual Analysis")


#Example 9-2
wshouses.df<-read.csv("Data_09_WSHouses.csv")
names(wshouses.df)<-c("size","selling.price")
wshouses.lm<-lm(selling.price~size,data=wshouses.df)
wshouses.lm.summary<-summary(wshouses.lm)
print(wshouses.lm.summary)
#Visualize residuals against IV and DV:
plot(wshouses.lm.summary$residuals~wshouses.df$size,pch=19,
     xlab="Size",ylab="Residual",main="Residual Analysis")
plot(wshouses.lm.summary$residuals~wshouses.df$selling.price,pch=19,
     xlab="Selling Price",ylab="Residual",main="Residual Analysis")
#Visualize standardized residuals agianst IV ans DV:
wshouses.lm.residuals.std<-rstudent(wshouses.lm)
plot(wshouses.lm.residuals.std~wshouses.df$size,pch=19,
     xlab="Size",ylab="Standardized Residual",main="Residual Analysis")
plot(wshouses.lm.residuals.std~wshouses.df$selling.price,pch=19,
     xlab="Selling Price",ylab="Standardized Residual",main="Residual Analysis")