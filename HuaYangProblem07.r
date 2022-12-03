#question 1
#sigma unknown
n1<-40
n2<-50
x1.bar<-56100
x2.bar<-59400
s1<-6000
s2<-7000
x1.bar.se<-s1/sqrt(n1)
x2.bar.se<-s2/sqrt(n2)
df<-(x1.bar.se^2+x2.bar.se^2)^2/
  ((x1.bar.se^4)/(n1-1)+(x2.bar.se^4)/(n2-1))
#p-value
print("Null hypothesis (H0): The nursing salaries in Tampa same or more than in Dallas.")
print("Alternative hypothesis (Ha): The nursing salaries in Tampa lower than in Dallas.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
p<-pt(t,df=df,lower.tail = TRUE)
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,""," not")," rejected.",sep=""))
#Critical value approach
print("Null hypothesis (H0): The nursing salaries in Tampa same or more than in Dallas.")
print("Alternative hypothesis (Ha): The nursing salaries in Tampa lower than in Dallas.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
t.alpha<-qt(alpha,df=df,lower.tail = TRUE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region: (-inf, ",t.alpha,"]",sep=""))
print(paste("t<=t.alpha is ",t<=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t<=t.alpha,"","not ")," rejected.",sep=""))


#question 2
#Difference between two population proportions
n1<-1000
n2<-900
p1.bar<-410/n1
p2.bar<-315/n2
p.bar<-(n1*p1.bar+n2*p2.bar)/(n1+n2)
p.bar.se<-sqrt(p.bar*(1-p.bar)*(1/n1+1/n2))
#p-value
print("Null hypothesis (H0): there is no difference between the proportions of financially fit adults from 2010 and 2012.")
print("Alternative hypothesis (Ha): there is significant difference between the proportions of financially fit adults from 2010 and 2012.")
z<-(p1.bar-p2.bar)/p.bar.se
alpha<-0.05
p<-2*min(pnorm(z,mean=0,sd=1,lower.tail = TRUE),
         pnorm(z,mean=0,sd=1,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null hypothesis (H0): there is no difference between the proportions of financially fit adults from 2010 and 2012.")
print("Alternative hypothesis (Ha): there is significant difference between the proportions of financially fit adults from 2010 and 2012.")
z<-(p1.bar-p2.bar)/p.bar.se
alpha<-0.05
z.alpha<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
print(paste("z=",z,"and z.alpha=",z.alpha))
print(paste("Rejection region: (-inf, ",-z.alpha,"] and [",z.alpha,", +inf)",sep=""))
print(paste("z<=-z.alpha is ",z<=-z.alpha,",z>=z.alpha is ",z>=z.alpha,".",sep=""))
print(paste("H0 is ",ifelse(z<=-z.alpha | z>=z.alpha,"","not ")," rejected.",sep=""))