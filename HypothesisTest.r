#Example 6-1 sigma Known
coffee.df <- read.csv("Data_06_Coffee.csv")
n<-dim(coffee.df)[1]
x.bar <- mean(coffee.df$Weight)
mu.zero<-3
#Assume population standard deviation is known
sgima<-0.18
x.bar.se<-sigma/sqrt(n)
#p-value approach:
z<-(x.bar-mu.zero)/x.bar.se
p<-pnorm(z,mean=0,sd=1,lower.tail = TRUE)
alpha<-0.01
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
z<-(x.bar-mu.zero)/x.bar.se
alpha<-0.01
z.alpha<-qnorm(alpha,mean=0,sd=1,lower.tail = TRUE)
print(paste("z=",z,"and z.alpha =",z.alpha))
print(paste("Rejection region: (-inf,", z.alpha, ")",sep="" ))
print(paste("z<=z.alpha is",z<=z.alpha,".",sep=""))
print(paste("H0 is ",ifelse(z<=z.alpha,"","not "),"rejected.",sep="")) 

#Example6-2 sigma Known
golf.df<-read_csv("Data_06_GolfTest.csv")
n<-dim(golf.df)[1]
x.bar<-mean(golf.df$Yards)
mu.zero<-295
#Assume population standard deviation is known
sigma<-12
x.bar.se<-sigma/sqrt(n)
#p-value approach:
z<-(x.bar-mu.zero)/x.bar.se
#Note that p value inclueds both tails!
p<-2*min(pnorm(z,mean=0,sd=1,lower.tail=TRUE),
         pnorm(z,mean=0,sd=1,lower.tail = FALSE))
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
z<-(x.bar-mu.zero)/x.bar.se
alpha<-0.05
#Note that alpha needs to be "distribute" among the two tails!
z.alpha<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
print(paste("z=",z,"and z.alpha =",z.alpha))
print(paste("Rejection rejoin:(-inf,",z.alpha,"] and [",z.alpha,",+inf)",sep=""))
print(paste("z<=-z.alpha is",z<=-z.alpha,",z>=z.alpha is",z>=z.alpha, ".",sep=""))
print(paste("H0 is ",ifelse(z<=-z.alpha | z>=z.alpha,"","not "),"rejected.",sep=""))


#Example 6-3 sigma Unknown
rating.df<-read.csv("Data_06_AirRating.csv")
n<-dim(rating.df)[1]
x.bar<-mean(rating.df$Rating)
mu.zero<-7
s<-sd(rating.df$Rating)
x.bar.se<-s/sqrt(n)
#p-value approach:
t<-(x.bar-mu.zero)/x.bar.se
p<-pt(t,df=n-1,lower.tail = FALSE)
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
t<-(x.bar-mu.zero)/x.bar.se
alpha<-0.05
t.alpha<-qt(alpha,df=n-1,lower.tail = FALSE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region:[",t.alpha,",+inf)",sep=""))
print(paste("t>=t.alpha is ",t>=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t>=t.alpha,"","not "),"rejected.",sep=""))


#Example 6-4 sigma Unknown
orders.df<-read.csv("Data_06_Orders.csv")
n<-dim(orders.df)[1]
x.bar<-mean(orders.df$Units)
mu.zero<-40
s<-sd(orders.df$Units)
x.bar.se<-s/sqrt(n)
#p-value approach:
t<-(x.bar-mu.zero)/x.bar.se
#Note that p value includes both tails!
p<-2*min(pt(t,df=n-1,lower.tail = TRUE),
         pt(t,df=n-1,lower.tail = FALSE))
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=))
#CV approach:
t<-(x.bar-mu.zero)/x.bar.se
alpha<-0.05
#Note that alpha needs to be "distributed" among the two tails!
t.alpha<-qt(alpha/2,df=n-1,lower.tail = FALSE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region: (-inf,",-t.alpha,"] and [",t.alpha,",+inf)",sep=""))
print(paste("t<=-t.alpha is ",t<=-t.alpha,",t>=t.alpha is ",t>=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t<=-t.alpha | t>=t.alpha,"","not "),"rejected.",sep="" ))



#Example 6-5 Population Proportion
wgolf.df<-read.csv("Data_06_WomenGolf.csv")
n<-dim(wgolf.df)[1]
p.bar<-sum(wgolf.df$Golfer=="Female")/n
p.zero<-0.20
p.bar.se<-sqrt(p.zero*(1-p.zero)/n)
#p-value
z<-(p.bar-p.zero)/p.bar.se
p<-pnorm(z,mean=0,sd=1,lower.tail = FALSE)
alpha<-0.05
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach
z<-(p.bar-p.zero)/p.bar.se
alpha<-0.05
z.alpha<-qnorm(alpha,mean=0,sd=1,lower.tail=FALSE)
print(paste("z=",z,"and z.alpha=",z.alpha))
print(paste("Rejection rejion:[",z.alpha,",+inf)",sep=""))
print(paste("z>=z.alpha is ",z>=z.alpha,".",sp=""))
print(paste("H0 is ",ifelse(z>=z.alpha,"","not "),"rejected.",sep=""))