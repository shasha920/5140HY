#install.pacages("e1071")
library(e1071)

#Example 2-1
softdrink.df <- read.csv("Data_02_SoftDrink.csv")
#Frequency distribution
softdrink.freq<-data.frame(table(softdrink.df))
names(softdrink.freq)[1]<-"softDrink"
softdrink.freq
#Relative and percent frequency distribution
softdrink.freq$Rel.Freq<-softdrink.freq$Freq/sum(softdrink.freq$Freq)
softdrink.freq$Pct.Freq<-100.00*softdrink.freq$Rel.Freq
softdrink.freq
#Bar chart
barplot(softdrink.freq$Freq,names.arg = softdrink.freq$softDrink,
        xlab="Soft Drink",ylab="Frequency",main="Bar Chart of Soft Drink Purchases")
pie(softdrink.freq$Freq,main="Pie Chart of Soft Driink Purchases",
    labels = paste(softdrink.freq$softDrink,"(",softdrink.freq$Pct.Freq,"%)",sep=""))

#Example 2-2
audit.df<-read.csv("Data_02_Audit.csv")
#Frequency distribution
#Appropriate number of classes is
#determined by either Sturges' rule,
#Scott's rule, or Freedman-Diaconis's rule
audit.time.bin.num<-nclass.Sturges(audit.df$Audit.Time)
audit.time.bin.num<-nclass.scott(audit.df$Audit.Time)
audit.time.bin.num<-nclass.FD(audit.df$Audit.Time)
audit.df$Audit.Time.Bin<-cut(audit.df$Audit.Time,audit.time.bin.num)
audit.freq<-data.frame(table(audit.df$Audit.Time.Bin))
names(audit.freq)[1]<-"Audit.Time.Bin"
audit.freq
#Relative and percent frequency distribution
audit.freq$Rel.Freq<-audit.freq$Freq/sum(audit.freq$Freq)
audit.freq$Pct.Freq<-100.00*audit.freq$Rel.Freq
audit.freq
#Cumulative(relative and percent) frequency distribution
audit.freq$Cum.Freq<-cumsum(audit.freq$Freq)
audit.freq$Cum.Rel.Freq<-cumsum(audit.freq$Rel.Freq)
audit.freq$Cum.Pct.Freq<-cumsum(audit.freq$Pct.Freq)
audit.freq
#Histofram
#Appropriate number of classes is 
#determined by either Sturges' rule,
#Scott's rule, or Freedman-Diaconis's rule
#although the breakpoints are adjusted
#to be "pretty"
hist(audit.df$Audit.Time,breaks="FD",xlab="Audit Time",main="Histogram of Audit Time")


#Example 2-3
restaurant.df<-read.csv("Data_02_Restaurant.csv")
names(restaurant.df)[3]<-"Meal.Price"
#Crosstabulation
#Cut meal price into 5 equal width bins
price.bin.num<-5
restaurant.df$Meal.Price.Bin<-cut(restaurant.df$Meal.Price,price.bin.num)
#Convert quality rating from string to factor with desired level order
#Good<Very Good<Excellent
#the default order was by ascending lexical order
restaurant.df$Quality.Rating<-factor(restaurant.df$Quality.Rating,levels=c("Good","Very Good","Excellent"))
#Crosstabulation:frequency distribution
restaurant.freq<-table(restaurant.df$Meal.Price.Bin,restaurant.df$Quality.Rating)
restaurant.freq
#Crosstabulation:column percent
restaurant.col.pct.freq<-apply(restaurant.freq,2,function(x){100.00*x/sum(x)})
restaurant.col.pct.freq
#Crosstabulation:row percent
restaurant.row.pct.freq<-apply(restaurant.freq,1,function(x){100.00*x/sum(x)})
restaurant.row.pct.freq
#Use function "t" to transpose a matrix
t(restaurant.row.pct.freq)
#Crosstabulation:column cumulative percent
restaurant.col.cum.pct.freq<-apply(restaurant.freq,2,function(x){100.00*cumsum(x)/sum(x)})
restaurant.col.cum.pct.freq
#Crosstabulation: row cumulative percent
restaurant.row.cum.pct.freq<-apply(restaurant.freq,1,function(x){100.00*cumsum(x)/sum(x)})
restaurant.row.cum.pct.freq
#Use function "t" to transpose a matrix
t(restaurant.row.cum.pct.freq)

#Example 2-4
stereo.df<-read.csv("Data_02_Stereo.csv")
names(stereo.df)[2]<-"Num.Commercials"
#Scatter chart and trend line
plot(stereo.df$Sales.Volume ~ stereo.df$Num.Commercials,pch=19,
     xlab="Number of Commercials",ylab="Sales Volume",main="Sales Volume vs. Number of Commercials")
#Use simple linear regression to generate trend line
abline(lm(stereo.df$Sales.Volume~stereo.df$Num.Commercials))


#Example 2-5
start.salary.df<-read_csv("Data_02_2012StartSalary.csv")
names(start.salary.df)[2]<-"Monthly.Starting.Salary"
#Measures of location
print(paste("80th percentile:",quantile(start.salary.df$Monthly.Starting.Salary,0.8,type=6)))
print(paste("First quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Second quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.5,type=6)))
print(paste("Third quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)))
#Measure of variablity
print(paste("Range:",max(start.salary.df$Monthly.Starting.Salary)-min(start.salary.df$Monthly.Starting.Salary)))
print(paste("IQR:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)-
              quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Variance:",var(start.salary.df$Monthly.Starting.Salary)))
print(paste("Standard deviation:",sd(start.salary.df$Monthly.Starting.Salary)))
print(paste("Coefficient of variation:",sd(start.salary.df$Monthly.Starting.Salary)/
              mean(start.salary.df$Monthly.Starting.Salary)))
#Measure of distribution shape
#Package e1071 is required for computing skewness
#set type=2 to compute skewness using the formula in book
print(paste("Skewness:",skewness(start.salary.df$Monthly.Starting.Salary,type=2)))
#Five number summary
print(paste("Smallest value:",min(start.salary.df$Monthly.Starting.Salary)))
print(paste("First quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.25,type=6)))
print(paste("Median:",quantile(start.salary.df$Monthly.Starting.Salary,0.5,type=6)))
print(paste("Third quartile:",quantile(start.salary.df$Monthly.Starting.Salary,0.75,type=6)))
print(paste("Largest value:",max(start.salary.df$Monthly.Starting.Salary)))

#Example 2-6
major.salary.df<-read.csv("Data_02_2012MajorSalary.csv")
names(major.salary.df)[2]<-"Monthly.Starting.Salary"
#Box plot
boxplot(major.salary.df$Monthly.Starting.Salary~major.salary.df$Major,
        xlab="Major",ylab="Monthly Starting Salary",
        main="Box Plot of Monthly Starting Salary by Major")


#Example 2-7
stereo.df<-read.csv("Data_02_Stereo.csv")
names(stereo.df)[2]<-"Num.Commercials"
#Sample covariance and correlation coefficient
print(paste("Covariance:",cov(stereo.df$Num.Commercials,stereo.df$Sales.Volume)))
print(paste("Correlation coefficient:",cor(stereo.df$Num.Commercials,stereo.df$Sales.Volume)))