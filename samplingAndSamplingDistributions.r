#Example 4-1
eai.df<-read.csv("Data_04_EAI.csv")
#Compute population parameters
mu<-mean(eai.df$Annual.Salary)
print(paste("Population mean of salary:",mu))
#Note that to compute population standard deviation, you will have to 
#correct the sample standard deviation(computed using the sd function)
#with a factor sqrt((N-1)/N)
N<-dim(eai.df)[1]
sigma<-sqrt((N-1)/N)*sd(eai.df$Annual.Salary)
print(paste("Population standard deviation of salary:",sigma))
p<-sum(eai.df$Training.Program=="Yes")/N
print(paste("Population proportion of managers who took training program:",p))
#Compute sample estimates:
#Obtain a sample of 30 elements, without replacement
n<-30
set.seed(1000)
sample.manager<-sample(eai.df$Manager,n,replace=FALSE)
#If there is not a column for ID(for each row)
#use the "row.names"function
#it gives you the row number
#which you can use as ID
eai.df.sample<-eai.df[match(sample.manager,eai.df$Manager),]
x.bar<-mean(eai.df.sample$Annual.Salary)
print(paste("Sample mean of salary:",x.bar))
s<-sd(eai.df.sample$Annual.Salary)
print(paste("Sample standard deviation of salary:",s))
p.bar<-sum(eai.df.sample$Training.Program=="Yes")/n
print(paste("Sample proportion of managers who took training program:",p.bar))
#Compute sample mean standard error:
if(n/N<0.05){
  x.bar.se<-sigma/sqrt(n)
}else{
  x.bar.se<-sqrt((N-n)/(N-1))*sigma/sqrt(n)
}
print(paste("Sample mean standard error:",x.bar.se))
#Compute sample proportion standard error:
if(n/N<0.05){
  p.bar.se<-sqrt(p*(1-p)/n)
}else{
  p.bar.se<-sqrt((N-n)/(N-1))*sqrt(p*(1-p)/n)
}
print(paste("Sample proportion standard error:",p.bar.se))