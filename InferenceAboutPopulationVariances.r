#Example 8-1 Population Variance Interval Estimation
n<-20
s<-sqrt(0.0025)
alpha<-1-0.95
df<-n-1
chisq.lt<-qchisq(alpha/2,df=df,lower.tail = TRUE)
chisq.rt<-qchisq(alpha/2,df=df,lower.tail = FALSE)
lb<-(n-1)*s^2/chisq.rt
ub<-(n-1)*s^2/chisq.lt
print(paste("Interval estimate:[",lb,",",ub,"]",sep=""))

#Example 8-2 Upper tail
bustimes.df<-read.csv("Data_08_BusTimes.csv")
n<-length(bustimes.df$Times)
df<-n-1
s<-sd(bustimes.df$Times)
var.zero<-4
alpha<-0.05
#P-value approach:
print(paste("Null hypothesis(H0): The variance of bus arrival times is no more than ",var.zero,".",sep=""))
print(paste("Alternative hypothesis (Ha): The variance of bus arrival times is greater than ",var.zero,",",sep=""))
chisq<-(n-1)*s^2/var.zero
p<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,",","not "),"rejected",sep=""))
#CV approach:
print(paste("Null hypothesis (H0): The variance of bus arrival times is no more than ",var.zero,".",sep=""))
print(paste("Alternative hypothesis (Ha): The variance of bus arrival times is greater than ",var.zero,".",sep=""))
chisq<-(n-1)*s^2/var.zero
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region: [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha.rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not "),"rejected.",sep=""))


#Example 8-3 two tail
n<-30
df<-n-1
var.zero<-100
s<-sqrt(162)
apha<-0.05
#P-value approach:
print("Null hypothesis (H0): There is no difference in the variance of test scores between two versions of test.")
print("Alternative hypothesis (Ha): There is significant difference in the variance of test scores between two versions of the test.")
chisq<-(n-1)*s^2/var.zero
p<-2*min(pchisq(chisq,df=df,lower.tail = TRUE),
         pchisq(chisq,df=df,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
print("Null hypothesis (H0): There is no difference in the varicance of test scores between two versions of test.")
print("Alternative hypothesis (Ha): There is significant difference in the variance of test scores between two versions of the test.")
chisq<-(n-1)*s^2/var.zero
chisq.alpha.lt<-qchisq(alpha/2,df=df,lower.tail = TRUE)
chisq.alpha.rt<-qchisq(alpha/2,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.lt=",chisq.alpha.lt,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region:(0, ",chisq.alpha.lt,"] and [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq<=chisq.alpha.lt is ",chisq<=chisq.alpha.lt,",chisq>=f.alpha.rt is ",chisq>=chisq.alpha.rt,sep=""))
print(paste("H0 is ",ifelse(chisq<=chisq.alpha.lt | chisq>=chisq.alpha.rt,"","not "),"rejected.",sep=""))


#Example 8-4 Two Population Variances two tail
schoolbus.df<-read_csv("Data_08_SchoolBus.csv")
print(paste("Sample stdev of Milbank:",sd(schoolbus.df$Milbank,na.rm = TRUE)))
print(paste("Sample stdev of Gulf Park:",sd(schoolbus.df$`Gulf Park`,na.rm = TRUE)))
n1<-sum(!is.na(schoolbus.df$Milbank))
n2<-sum(!is.na(schoolbus.df$`Gulf Park`))
df1<-n1-1
df2<-n2-1
s1<-sd(schoolbus.df$Milbank,na.rm=TRUE)
s2<-sd(schoolbus.df$`Gulf Park`,na.rm = TRUE)
#Verify that we have assigned the population with larger
#samle variance as population 1
print(paste("Arrival time variance of Milbank is:",s1^2))
print(paste("Arrival time variance of Gulf Park is:",s2^2))
alpha<-0.1
#P-value approach:
print("Null hypothesis (H0): There is no difference in the variance of arrival times between the two companies.")
print("Alternative hypothesis (Ha): There is significant difference in the variance of arrival times between the two companies.")
f<-s1^2/s2^2
p<-2*min(pf(f,df1=df1,df2=df2,lower.tail = TRUE),
         pf(f,df1=df1,df2=df2,lower.tail = FALSE))
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
print("Null hypothesis (H0): There is no difference in the variance of arrival times between the two companies.")
print("Alternative hypothesis (Ha): There is significant difference in the variance of arrical times between the two companies.")
f<-s1^2/s2^2
f.alpha.lt<-qf(alpha/2,df1=df1,df2=df2,lower.tail = TRUE)
f.alpha.rt<-qf(alpha/2,df1=df1,df2=df2,lower.tail = FALSE)
print(paste("f=",f,"and f.alpha,lt=",f.alpha.lt,"] and [",f.alpha.rt,", +inf)",sep=""))
print(paste("f<=f.alpha.lt is ",f<=f.alpha.lt,",f>=f.alpha.rt is ",f>=f.alpha.rt,sep=""))
print(paste("H0 is ",ifelse(f<=f.alpha.lt | f>=f.alpha.rt,"","not "),"rejected.",sep=""))


#Example 8-5 Upper tail
#Women popultaion is denoted by population 1
#Note that the hypoteses are already framed
#such that the test is an upper one-tailed test
n1<-41
n2<-31
df<-n1-1
df<-n2-1
s1<-sqrt(120)
s2<-sqrt(80)
alpha<-0.05
#P-value approach:
print("Null hypothesis (H0): Women' variance in attitudes about current political issues is no greater than mens'.")
print("Alternative hypothesis (Ha): Womens' variance in attitudes about current political issues is greater than mens'.")
f<-s1^2/s2^2
p<-pf(f,df1=df1,df2=df2,lower.tail = FALSE)
print(paste("p=",p,"and alpha=",alpha))
print(paste("p<=alpha is ",p<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p<=alpha,"","not "),"rejected.",sep=""))
#CV approach:
print("Null hypothesis (H0):Womens' variance in attitudes about current political issues is no greater than mens'.")
print("Alternative hypothesis (Ha):Womens' variance in attitudes about current political issues is greater than mens'.")
f<-s1^2/s2^2
f.alpha.rt<-qf(alpha,df1=df1,df2=df2,lower.tail = FALSE)
print(paste("f=",f,"and f.alpha.rt=",f.alpha.rt))
print(paste("Rejection region:[",f.alpha.rt,", +inf)",sep=""))
print(paste("f>=f.alpha.rt is ",f>=f.alpha.rt,sep=""))
print(paste("H0 is ",ifelse(f>=f.alpha.rt,"","not "),"rejected.",sep=""))