#Example 5-1 Population Mean:sigma Known
alpha <-1-0.9
print(paste("z_alpha/2 when 1-alpha=0.9.",qnorm(1-alpha/2,mean=0,sd=1,lower.tail = TRUE)))
#Another way to implement the same thing.
print(paste("z_alpha/2 when 1-alpha=0.9:",qnorm(alpha/2,mean = 0,sd=1,lower.tail = FALSE)))
alpha<-1-0.95
print(paste("z_alpha/2 when 1-alpha=0.95.",qnorm(1-alpha/2,mean=0,sd=1,lower.tail = TRUE)))
alpha<-1-0.99
print(paste("z_alpha/2 when 1-alpha=0.99.",qnorm(1-alpha/2,mean=0,sd=1,lower.tail=TRUE)))


#Example 5-2
lloyd.df<-read_csv("Data_05_Lloyd's.csv")
#Compute interval estimate when 1-alpha=0.95
n<-dim(lloyd.df)[1]
x.bar<-mean(lloyd.df$`Amount Spent`)
sigma<-20
alpha<-1-0.95
z<-qnorm(1-alpha/2,mean=0,sd=1,lower.tail = TRUE)
#Assume large enough population if not specified
x.bar.se<-sigma/sqrt(n)
print(paste("Interval estimate lower bound:",x.bar-z*x.bar.se))
print(paste("Interval estimate upper bound:",x.bar+z*x.bar.se))



#Example 5-3 sigma Unknown
alpha<-1-0.9
n<-65
print(paste("t_alpha/2 when 1-alpha =0.9 and n=65:",
            qt(1-alpha/2,df=n-1,lower.tail = TRUE)))
alpha<-1-0.95
n<-95
print(paste("t_alpha/2 when 1-alpha=0.95 and n=95:",
            qt(1-alpha/2,df=n-1,lower.tail =TRUE)))
alpha<-1-0.99
n<-95
print(paste("t_alpha/2 when 1-alpha=0.99 and n=95:",
            qt(1-alpha/2,df=n-1,lower.tail = TRUE)))



#Example 5-4
nb.df<-read_csv("Data_05_NewBalance.csv")
#Compute interval estimate when 1-alpha=0.95
n<-dim(nb.df)[1]
x.bar<-mean(nb.df$NewBalance)
s<-sd(nb.df$NewBalance)
alpha<-1-0.95
t<-qt(1-alpha/2,df=n-1,lower.tail = TRUE)
#Assume large enough population if not specified
x.bar.se<-s/sqrt(n)
print(paste("Interval estimate lower bound:",x.bar-t*x.bar.se))
print(paste("Interval estimate upper bound:",x.bar+t*x.bar.se))


#Example 5-5
scheer.df<-read.csv("Data_05_Scheer.csv")
#Compute Interval estimate when 1-alpha=0.95
n<-dim(scheer.df)[1]
x.bar<-mean(scheer.df$Days)
s<-sd(scheer.df$Days)
alpha<-1-0.95
t<-qt(1-alpha/2,df=n-1,lower.tail = TRUE)
#Assume large enough population if not specified
x.bar.se<-s/sqrt(n)
print(paste("Interval estimate lower bound:",x.bar-t*x.bar.se))
print(paste("Interval estimate upper bound:",x.bar+t*x.bar.se))


#Example 5-6
#Compute mininum sample size given desired margin of error
sigma<-4500
alpha<-1-0.95
E<-500
z<-qnorm(1-alpha/2,mean=0,sd=1,lower.tail = TRUE)
print(paste("The minimum sample size should be:",(z^2)*(sigma^2)/(E^2)))



#Example 5-7 Population Proportion
tt.df<-read_csv("Data_05_TeeTimes.csv")
#Compute interval estimate when 1-alpha=0.95
n<-dim(tt.df)[1]
p.bar<-sum(tt.df$Response=="Yes")/n
alpha<-1-0.95
z<-qnorm(1-alpha/2,mean = 0,sd=1,lower.tail = TRUE)
p.bar.se<-sqrt(p.bar*(1-p.bar)/n)
print(paste("Interval estimate lower bound:",p.bar-z*p.bar.se))
print(paste("Interval estimate upper bound:",p.bar+z*p.bar.se))
#Compute minimum sample size given desired margin of error
p.star<-0.5
alpha<-1-0.95
E<-0.025
z<-qnorm(1-alpha/2,mean=0,sd=1,lower.tail = TRUE)
print(paste("The minimum sample size should be:",z^2*p.star*(1-p.star)/E^2))