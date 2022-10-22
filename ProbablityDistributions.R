#Compute probability of a random variable of standard normal distribution
#p(z<=1.00)=?
#p(z>=1.50)=?
#p(z<=-1.00)=?
#p(-0.45<=z<=1.25)=?
print(paste("p(z<=1.00)=", pnorm(1,mean=0,sd=1,lower.tail=TRUE)))
print(paste("p(z>=1.50)=",pnorm(1.5,mean=0,sd=1,lower.tail=FALSE)))
print(paste("p(z<=-1.00)=",pnorm(-1,mean=0,sd=1)))
print(paste("p(-0.45<=z<=1.25)=",pnorm(1.25,mean=0,sd=1)-pnorm(-0.45,mean=0,sd=1)))
print(paste("Given p(z>=z0)=0.1,z0=",qnorm(0.1,mean=0,sd=1,lower.tail = FALSE)))

#Example3-1
#Grear Tire Company developed a new tire to be sold throughout the nation. Grear’s managers believe
#that the mileage guarantee offered with the tire will be an important factor in the acceptance of
#the product. Before finalizing the tire mileage guarantee policy, Grear’s managers want probability
#information about 𝑥𝑥 = number of miles the tires will last. After road tests, Grear’s engineering
#group estimated that 𝜇= 36500 miles and 𝜎= 5000 miles. What percentage of the tires can be expected
#to last more than 40000 miles? Grear wants to provide a discount on replacement tires if the original
#tires do not provide the guaranteed mileage. What should the guarantee mileage be if Grear wants no more
#than 10% of the tires to be eligible for the discount guarantee?
print(paste("p(x>=40000)=",pnorm(40000,mean=36500,sd=5000,lower.tail = FALSE)))
print(paste("z=",(40000-36500)/5000))
print(paste("p(z>=0.7)=",pnorm(0.7,mean = 0,sd=1,lower.tail = FALSE)))
print(paste("Give P(x<=x0)=0.1,x0=",qnorm(0.1,mean=36500,sd=5000)))

#Examples3-2
#A company has a history of making errors in 10% of its invoices.
#A sample of 100 invoices has been taken.
#What is the probability that 12 invoices contain errors? 
#What is the probability of having 13 or fewer errors?
print(paste("p(x=12)=",pnorm(12+0.5,mean=100*0.1,sd=sqrt(100*0.1*(1-0.1)))-pnorm(12-0.5,mean=100*0.1,sd=sqrt(100*0.1*(1-0.1)))))
print(paste("p(x<=13)=",pnorm(13+0.5,mean=100*0.1,sd=sqrt(100*0.1*(1-0.1)))))

