#Example 7-1
scores.df<-read.csv("Data_07_ExamScores.csv")
#Use is.na function to ruole out NAs, which are used
#to "patch"the shorter solumns so that all columns
#are of equal length!
n1<-sum(!is.na(scores.df$Center.A))
n2<-sum(!is.na(scores.df$Center.B))
#Remember to set na.rm so that the mean is not NA
x1.bar<-mean(scores.df$Center.A,na.rm = TRUE)
x2.bar<-mean(scores.df$Center.B,na.rm=TRUE)
sigma1<-10
sigma2<-10
x1.bar.se<-sigma1/sqrt(n1)
x2.bar.se<-sigma2/sqrt(n2)
#p-value approach:
print("Null hypothesis (H0): There is no difference between the average exam scores of students from center A and center v.")
print("Alternative hypothesis (Ha): There is significant difference between the average exam scores of students from center A and center B.")
z<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
p<-2*min(pnorm(z,mean=0,sd=1,lower.tail = TRUE),
         pnorm(z,mean=0,sd=1,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep = ""))
print(paste("H0 is",ifelse(p<=alpha,""," not")," rejected.",sep = ""))
#Critical value approach:
print("Null hypothesis (H0): There is no difference between the average exam scores of students from center A and center B.")
print("Alternative hypothesis (Ha): There is significant difference between the average exam scores of students from center A and center B.")
z<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
z.alpha<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
print(paste("z=",z,"and z.alpha=",z.alpha))
print(paste("Rejection region: (-inf, ",-z.alpha,"] and [",z.alpha,", +inf)",sep=""))
print(paste("z <= -z.alpha is ",z<=-z.alpha, ", z>=z.alpha is ",z>=z.alpha,".",sep=""))
print(paste("H0 is ",ifelse(z<=-z.alpha | z>=z.alpha, "","not ")," rejected.",sep=""))

#Example 7-2
check.df<-read.csv("Data_07_CheckAcct.csv")
n1<-sum(!is.na(check.df$Cherry.Grove))
n2<-sum(!is.na(check.df$Beechmont))
x1.bar<-mean(check.df$Cherry.Grove,na.rm=TRUE)
x2.bar<-mean(check.df$Beechmont,na.rm = TRUE)
s1<-sd(check.df$Cherry.Grove,na.rm = TRUE)
s2<-sd(check.df$Beechmont,na.rm=TRUE)
x1.bar.se<-s1/sqrt(n1)
x2.bar.se<-s2/sqrt(n2)
df<-(x1.bar.se^2+x2.bar.se^2)^2/
  ((x1.bar.se^4)/(n1-1)+(x2.bar.se^4)/(n2-1))
#p-value
print("Null hypothesis (H0): There is no difference between the average checking account balances from the two branches.")
print("Alternative hypothesis (Ha): There is significant difference between the average checking account balaces from the two branches.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
p<-2*min(pt(t,df=df,lower.tail = TRUE),
         pt(t,df=df,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null hypothesis (H0): There is no difference between the average checking account balances from the two branches.")
print("alternative hypothesis (Ha): There is significant difference between the average checking account balances from the two branches.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
t.alpha<-qt(alpha/2,df=df,lower.tail = FALSE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region: (-inf, ",-t.alpha,"] and [",t.alpha,", +inf)",sep=""))
print(paste("t<=-t.alpha is ",t<=-t.alpha,", t>=t.alpha is ",t>=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t<=-t.alpha | t>=t.alpha,"","not ")," rejected.",sep=""))


#Example 7-3
software.df<-read.csv("Data_07_softwareTest.csv")
n1<-sum(!is.na(software.df$Current))
n2<-sum(!is.na(software.df$New))
x1.bar<-mean(software.df$Current,na.rm=TRUE)
x2.bar<-mean(software.df$New,na.rm = TRUE)
s1<-sd(software.df$Current,na.rm = TRUE)
s2<-sd(software.df$New,na.rm = TRUE)
x1.bar.se<-s1/sqrt(n1)
x2.bar.se<-s2/sqrt(n2)
df<-(x1.bar.se^2+x2.bar.se^2)^2/
  ((x1.bar.se^4)/(n1-1)+(x2.bar.se^4)/(n2-1))
#p-value
print("Null hypothesis (H0): The new software take the same or more time to complete a project compared to the current technology.")
print("Alternative hypothesis (Ha): The new software take less time to complete a project compared to the current techonology.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-0.05
p<-pt(t,df=df,lower.tail = FALSE)
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null hypothesis (H0): The new software take the same or more time to complete a project compared to the current technology.")
print("Alternative hypothesis (Ha): The new software take less time to complete a project compared to the current technology.")
t<-(x1.bar-x2.bar)/sqrt(x1.bar.se^2+x2.bar.se^2)
alpha<-qt(alpha,df=df,lower.tail = FALSE)
print(paste("t=",t,"and t.alpha=",t.alpha))
print(paste("Rejection region: [",t.alpha,",+ inf)",sep=""))
print(paste("t>=t.alpha is ",t>=t.alpha,".",sep=""))
print(paste("H0 is ",ifelse(t>=t.alpha,"","not ")," rejected.",sep=""))


#Example 7-4
tax.df<-read.csv("Data_07_TaxPrep.csv")
tax.df<-data.frame(apply(tax.df,c(1,2),function(x){ifelse(x=="",NA,x)}))
n1<-sum(!is.na(tax.df$Office.1))
n2<-sum(!is.na(tax.df$Office.2))
p1.bar<-sum(tax.df$Office.1=="Yes",na.rm=TRUE)/n1
p2.bar<-sum(tax.df$Office.2=="Yes",na.rm = TRUE)/n2
p.bar<-(n1*p1.bar+n2*p2.bar)/(n1+n2)
p.bar.se<-sqrt(p.bar*(1-p.bar)*(1/n1+1/n2))
#p-value approach:
print("Null hypothesis (H0): The error rate of tax returns prepared by the two regional offices are the same.")
print("Alternative hypothesis (Ha): The error rate of tax returns prepared by the two regional offices are different.")
z<-(p1.bar-p2.bar)/p.bar.se
alpha<-0.05
p<-2*min(pnorm(z,mean=0,sd=1,lower.tail = TRUE),
         pnorm(z,mean=0,sd=1,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null hypothesis (H0): The error rate of tax return prepared by the two regional offices are the same.")
print("Alternative hypothesis (Ha): The error rate of tax returns prepared by the two regional offices are different.")
z<-(p1.bar-p2.bar)/p.bar.se
alpha<-0.05
z.alpha<-qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
print(paste("z=",z,"and z.alpha=",z.alpha))
print(paste("Rejection region: (-inf, ",-z.alpha,"] and [",z.alpha,", +inf)",sep=""))
print(paste("z<=-z.alpha is ",z<=-z.alpha,",z>=z.alpha is ",z>=z.alpha,".",sep=""))
print(paste("H0 is ",ifelse(z<=-z.alpha | z>=z.alpha,"","not ")," rejected.",sep=""))