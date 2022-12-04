#Problem 1
#Test of Independence
#Data-Ambulance.csv
ambulance.df<-read.csv("Data-Ambulance.csv")
table(ambulance.df$County)
ambulance.df$County<-factor(ambulance.df$County,levels = c("Urban","Rural"))
table(ambulance.df$Day.of.Week)
ambulance.df$Day.of.Week<-factor(ambulance.df$Day.of.Week,
                                 levels = c("Monday","Tuesday","Wednesday",
                                            "Thursday","Friday","Saturday","Sunday"))
#Compute the observed frequency of preference toward
#different the day of the week by different county
ambulance.table.f<-table(ambulance.df[c("Day.of.Week","County")])
ambulance.table.f
#Compute the sample size
n<-margin.table(ambulance.table.f)
#Compute the proportion of preference toward
#different the day of the week regardless of county
p.i<-margin.table(ambulance.table.f,1)/n
#Compute the proportion of county regardless of 
#preference towards different the day of the week
p.j<-margin.table(ambulance.table.f,2)/n
#Compute the expected frequency of preference toward
#different the day of the week by different county
ambulance.table.e<-p.i %o% p.j * n
ambulance.table.e
ambulance.table.test<-(ambulance.table.f - ambulance.table.e) ^ 2 / ambulance.table.e
chisq<-margin.table(ambulance.table.test)
df<-(dim(ambulance.table.f)[1]-1)*(dim(ambulance.table.f)[2]-1)
alpha<-0.05
#p-value approach:
print("Null Hypothesis (H0): Preferences toward different the day of the week is independent of county.")
print("Alternative Hypothesis (Ha): Preferences toward different the day of the week is not independent of county.")
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p.value=",p.value,"and alpha=",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",sep=""))
#Critical value approach
print("Null Hypothesis (H0): Preferences toward different the day of the week is independent of county.")
print("Alternative Hypothesis (Ha): Preferences toward different the day of the week is not independent of county.")
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region :[",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha.rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))


#Problem 2
#Goodness of Fit Test
#Data-Grades.csv
grades.df<-read.csv("Data-Grades.csv")
n<-length(grades.df$Grade)
x.bar<-mean(grades.df$Grade)
s<-sd(grades.df$Grade)
print(paste("Sample mean=",x.bar))
print(paste("Sample standard deviation=",s))
#Determine the number of intervals
k<-floor(n/5)
#Determine the break points for bincode function
breaks<-x.bar+c(-Inf,qnorm(seq(1,k)/k))*s
#Create observed frequency vector
#The observed frequencies is different from the ones in the book!
grades.f<-table(.bincode(grades.df$Grade,breaks,include.lowest = TRUE))
grades.f
#Create expected frequency vector
#Note that grades.f/grades.f is used to create a
#table with all is with the same column names as grades.f
grades.e<-(n/k)*grades.f / grades.f
grades.e
grades.test<-(grades.f - grades.e) ^ 2 / grades.e
chisq<-sum(grades.test)
df<-k-2-1
alpha<-0.05
#p-value approach:
print(paste("Null hypothesis (H0): The test grades follows a normal distribution with mean ",
            x.bar," and standard deviation ",s,":",sep=""))
print(paste("Alternative hypothesis (Ha): The test grades does not follow a normal distribution with mean ",
            x.bar," and standard deviation ",s,".",sep=""))
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p.value=",p.value,"and alpha",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print(paste("Null hypothesis (H0): The test grades follows a normal distribution with mean ",
            x.bar," and standard deviation ",s,":",sep=""))
print(paste("Alternative hypothesis (Ha): The test grades does not follow a normal distribution with mean ",
            x.bar," and standard deviation ",s,".",sep=""))
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha,rt=",chisq.alpha.rt))
print(paste("Rejection region: [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha.rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))



#Problem 3
#Testing the Equality of Population Proportions for Three or More Populations
#Data-Flights.csv
flights.df<-read.csv("Data-Flights.csv")
table(flights.df$Status)
flights.df$Status<-factor(flights.df$Status,levels = c("delayed","ontime"))
flights.df
#Compute the observed frequency of flight
#carriers who delayed
flights.table.f<-table(flights.df[c("Carrier","Status")])
flights.table.f
#Compute the sample size
n<-margin.table(flights.table.f)
#Compute the proportion of different carriers
#regardless of whether they delayed or not
p.i<-margin.table(flights.table.f,1)/n
#Compute the proportion of carriers delayed
#regardless of which carriers
p.j<-margin.table(flights.table.f,2)/n
#Apply the above proportion to compute the expected
#frequency of carriers who delayed
#%o% stands for outer product (of two vectors):
#x1<-c(1,2,3)
#x2<-c(4,5,6)
#t(t(x1))
#t(x2)
#x1 %o% x2
flights.table.e<-p.i %o% p.j
flights.table.e<-flights.table.e*n
flights.table.e
flights.table.test<-(flights.table.f - flights.table.e) ^ 2 / flights.table.e
flights.table.test
chisq<-margin.table(flights.table.test)
df<-dim(flights.table.f)[1]-1
alpha<-0.05
#p-value approach:
print("Null Hypothesis (H0): The proportion of delayed flights is the same across all carriers.")
print("Alternative Hypothesis (Ha): The proportion of delayed flights is not equal across all carriers.")
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p.value=",p.value,"and alpha=",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",spe=""))
#Critical value approach:
print("Null Hypothesis (H0): The proportion of delayed flights is the same across all carriers.")
print("Alternative Hypothesis (Ha): The proportion of delayed flights is not equal across all carriers.")
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region: [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha.rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))
#Marascuilo procedure:
alpha<-0.05
#Loop through all possible combination
#without repetition
for(i1 in 1:(dim(flights.table.f)[1]-1)){
  for(i2 in (i1+1):dim(flights.table.f)[1]){
    model.1<-rownames(flights.table.f)[i1]
    model.2<-rownames(flights.table.f)[i2]
    flights.table.f.1<-flights.table.f[i1,]
    flights.table.f.2<-flights.table.f[i2,]
    n.1<-sum(flights.table.f.1)
    n.2<-sum(flights.table.f.2)
    p.1.bar<-flights.table.f.1[c("delayed")]/n.1
    p.2.bar<-flights.table.f.2[c("delayed")]/n.2
    print(paste("Null hypothesis (H0): The proportion of delayed flights is the same between ",
                model.1," and ",model.2,".",sep=""))
    print(paste("Alternative hypothesis (Ha): The proportion of delayed flights is not equal between ",
                model.1," and ",model.2,".",sep=""))
    test.statistic<-abs(p.1.bar - p.2.bar)
    chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
    critical.value<-sqrt(chisq.alpha.rt)*sqrt(p.1.bar * (1 - p.1.bar) / n.1 + p.2.bar * (1 - p.2.bar) / n.2)
    print(paste("Test statistic = ",test.statistic,",critical value=",critical.value,".",sep=""))
    print(paste("Rejection region:[",critical.value,",+inf)",sep=""))
    print(paste("Test statistic >=critical value is ",test.statistic>=critical.value,".",sep=""))
    print(paste("H0 is ",ifelse(test.statistic>=critical.value,"","not ")," rejected.",sep=""))
    print("=====================================")
  }
}