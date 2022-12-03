#Example 11-1 Testing the Equality of Population Proportions for Three or More Populations
auto.df<-read.csv("Data_11_AutoLoyalty.csv")
auto.df$Likely.Purchase<-factor(auto.df$Likely.Purchase,levels = c("Yes","No"))
auto.df
#Compute the observed frequency of car
#owners who repurchase the same model
auto.table.f<-table(auto.df[c("Population","Likely.Purchase")])
auto.table.f
#Compute the sample size
n<-margin.table(auto.table.f)
#Compute the proportion of car owners of different models
#regardless of whether they repurchase the same model or not
p.i<-margin.table(auto.table.f,1)/n
#Compute the proportion of car owners repurchasing
#the same model regardless of which model
p.j<-margin.table(auto.table.f,2)/n
#Apply the above proportion to compute the expected
#frequency of car owners who repurchase the same model
#%o% stands for outer product (of two vectors):
#x1<-c(1,2,3)
#x2<-c(4,5,6)
#t(t(x1))
#t(x2)
#x1 %o% x2
auto.table.e<-p.i %o% p.j
auto.table.e<-auto.table.e*n
auto.table.e
auto.table.test<-(auto.table.f - auto.table.e)^2/auto.table.e
auto.table.test
chisq<-margin.table(auto.table.test)
df<-dim(auto.table.f)[1]-1
alpha<-0.05
#p-value approach:
print("Null Hypothesis (H0): The proportions of auto owners who repurchase the same model are the same across all models.")
print("Alternative Hypothesis (Ha): The proportions of auto owners who repurchase the same model are not equal across all models.")
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("P.value=",p.value,"and alpha=",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null Hypothesis (H0): The proportions of auto owners who repurchase the same model are the same across all models.")
print("Alternative Hypothesis (Ha): The proportions of auto owners who repurchase the same model are not equal across all models.")
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region: [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha,rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))
#Marascuilo procedure:
alpha<-0.05
#Loop through all possible combination
#without repetition
for(i1 in 1:(dim(auto.table.f)[1]-1)){
  for(i2 in (i1+1):dim(auto.table.f)[1]){
    model.1<-rownames(auto.table.f)[i1]
    model.2<-rownames(auto.table.f)[i2]
    auto.table.f.1<-auto.table.f[i1,]
    auto.table.f.2<-auto.table.f[i2,]
    n.1<-sum(auto.table.f.1)
    n.2<-sum(auto.table.f.2)
    p.1.bar<-auto.table.f.1[c("Yes")]/n.1
    p.2.bar<-auto.table.f.2[c("Yes")]/n.2
    print(paste("Null hypothesis (H0): The proportions of auto owners who repurchase the same model are the same between ",
                model.1," and ",model.2,".",sep=""))
    print(paste("Alternative hypothesis (Ha): The proportions of auto owners who repurchase the same model are not the same between ",
                model.1," and ",model.2,".",sep=""))
    test.statistic<-abs(p.1.bar-p.2.bar)
    chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
    critical.value<-sqrt(chisq.alpha.rt)*sqrt(p.1.bar*(1-p.1.bar)/n.1+p.2.bar*(1-p.2.bar)/n.2)
    print(paste("Test statistic = ",test.statistic,",critical value=",critical.value,".",sep=""))
    print(paste("Rejection region:[",critical.value,",+inf)",sep=""))
    print(paste("Test statistic >= critical value is ",test.statistic>=critical.value,".",sep=""))
    print(paste("H0 is ",ifelse(test.statistic>=critical.value,"","not ")," rejected.",sep=""))
    print("=====================================")
  }
}


#Example 11-2 Test of Independence
beer.df<-read.csv("Data_11_BeerPreference.csv")
beer.df$Gender<-factor(beer.df$Gender,levels=c("Male","Female"))
beer.df$Preference<-factor(beer.df$Preference,
                            levels=c("Light","Regular","Dark"))
#Compute the observed frequency of preference toward
#different types of beer by different gender
beer.table.f<-table(beer.df[c("Preference","Gender")])
beer.table.f
#Compute the sample size
n<-margin.table(beer.table.f)
#Compute the proportion of preference toward
#different types of beer regardless of gender
p.i<-margin.table(beer.table.f,1)/n
#Compute the proportion of gender regardless of
#preference towards different types of beer
p.j<-margin.table(beer.table.f,2)/n
#Compute the expected frequency of preference toward
#different types of beer by different gender
beer.table.e<-p.i %o% p.j*n
beer.table.e
beer.table.test<-(beer.table.f - beer.table.e)^2/beer.table.e
chisq<-margin.table(beer.table.test)
df<-(dim(beer.table.f)[1]-1)*(dim(beer.table.f)[2]-1)
alpha<-0.05
#p-value approach:
print("Null Hypothesis (H0): Preferences toward different types of beer is independent of gender.")
print("Alternative Hypothesis (Ha): Preferences toward different types of beer is not independent of gender.")
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p.value=",p.value,"and alpha=",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print("Null Hypothesis (H0): Preferences toward different types of beer is independent of gender.")
print("Alternative Hypothesis (Ha): Preferences toward different types of beer is not independent of gender.")
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha.rt=",chisq.alpha.rt))
print(paste("Rejection region:[",chisq.alpha.rt,",+inf)",sep=""))
print(paste("chisq>=chisq.alpha.rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))



#Example 11-3 Goodness of Fit Test
chemline.df<-read.csv("Data_11_Chemline.csv")
n<-length(chemline.df$Score)
x.bar<-mean(chemline.df$Score)
s<-sd(chemline.df$Score)
print(paste("Sample mean=",x.bar))
print(paste("Sample standard deviation=",s))
#Determine the number of intervals
k<-floor(n/5)
#Determine the break points for bincode function
breaks<-x.bar+c(-Inf,qnorm(seq(1,k)/k))*s
#Create observed frequency vector
#The observed frequencies is different from the ones in the book!
chemline.f<-table(.bincode(chemline.df$Score,breaks,include.lowest = TRUE))
chemline.f
#Create expected frequency vector
#Note that chemline.f/chemline.f is used to create a
#table with all 1s with the same column names as chemline.f
chemline.e<-(n/k)*chemline.f / chemline.f
chemline.e
chemline.test<-(chemline.f - chemline.e)^2/chemline.e
chisq<-sum(chemline.test)
df<-k-2-1
alpha<-0.05
#p-value approach:
print(paste("Null hypothesis (H0): The test score of new hires follows a normal distribution with mean ",
            x.bar," and standard deviation ",s,":",sep=""))
print(paste("Alternative hypothesis (Ha): The test score of new hires does not follow a normal distribution with mean ",
            x.bar," and standard deviation ",s,".",sep=""))
p.value<-pchisq(chisq,df=df,lower.tail = FALSE)
print(paste("p.value=",p.value,"and alpha=",alpha))
print(paste("p.value<=alpha is ",p.value<=alpha,".",sep=""))
print(paste("H0 is ",ifelse(p.value<=alpha,""," not")," rejected.",sep=""))
#Critical value approach:
print(paste("Null hypothesis (H0): The test score of new hires follows a normal distribution with mean ",
            x.bar," and standardd deviation ",s,":",sep=""))
print(paste("Alternative hypothesis (Ha): The test score of new hires does not follow a normal distribution with mean ",
            x.bar," and standard deviation ",s,".",sep=""))
chisq.alpha.rt<-qchisq(alpha,df=df,lower.tail = FALSE)
print(paste("chisq=",chisq,"and chisq.alpha,rt=",chisq.alpha.rt))
print(paste("Rejection region: [",chisq.alpha.rt,", +inf)",sep=""))
print(paste("chisq>=chisq.alpha,rt is ",chisq>=chisq.alpha.rt,".",sep=""))
print(paste("H0 is ",ifelse(chisq>=chisq.alpha.rt,"","not ")," rejected.",sep=""))