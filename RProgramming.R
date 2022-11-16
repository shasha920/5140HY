#R Object
num<-5
num
char<-"abc"
char
char<-"35.2"
char
num<-5.5
num
logic<-TRUE
logic


#Vector
num<-c(3,4.5,2.46)
num
length(num)
num<-seq(1,10,0.1)
num
length(num)
num<-1:10
num
length(num)


#Vector:type coercion
mix<-c(3.2,"abc")
mix
mix<-c(3.2,TRUE,FALSE)
mix
mix<-c(TRUE,'abc')
mix
mix<-c(3.2,'abc',TRUE)
mix


#Factor
?factor
fact<-factor(c(1,2,3,1,3,2,1))
fact
length(fact)
fact<-factor(c(TRUE,FALSE,FALSE,TRUE,TRUE))
fact
length(fact)
fact<-factor(c('yes','no','yes','no','yes','no'))
fact
length(fact)


#Matrix
?matrix
mtrx<-matrix(1:6,nrow=2,ncol=3)
mtrx
length(mtrx)
dim(mtrx)


#Array
?array
arr<-array(1:24,dim=c(2,3,4))
arr
length(arr)
dim(arr)

#List
?list
lst<-list(1:10,c("a","b","c"),TRUE)
lst
length(lst)

#Data frame
?data.frame
?data
data()
mtcars
length(mtcars)
dim(mtcars)

#Missing value
num_na<-c(1,NA,3,4,NA,0/0)
num_na
is.na(num_na)
is.nan(num_na)

#Elementary arithmetic operators
2+3
6/4
2^3
ans<-2+3
ans<-6/4
ans<-2^3


#Arithmetic functions
log(8)
log(8,2)
exp(1)
sqrt(4)
sum(1:5)
sd(1:5)

#Comparison operators
-1>=0
ans<--1>=0
ans
num<-8
num>=5
num==8
num!=9

#Vectorized operations of arithmetic and comparison operators
c(1,2,3)+c(4,5,6)
c(1,2,3)^c(4,5,6)
c(1,2,3)-c(1,2)
c(2,3)^c(1,2,3)
c(1,2,3,5)==c(1,2,3,4)
c(1,2,3,4)>=c(2,3)

#Logical operators
logic1<-TRUE
logic2<-FALSE
logic1&logic2
logic1|logic2
!logic1
!logic2
FALSE&FALSE&TRUE
FALSE&(FALSE|TRUE)
!FALSE&(FALSE|TRUE)

#Vectorized operations of logical operators
num<-1:10
logic1<-num>=2
logic1
logic2<-num<=3
logic2
logic1&logic2
logic1&&logic2
logic1|logic2
logic1||logic2
!logic1
!logic2


#type of function
?typeof
typeof(num)
typeof(logic1)
typeof(char)
typeof(fact)
typeof(lst)
typeof(mtrx)
typeof(arr)
typeof(mtcars)

#attribut function
?attributes
attributes(num)
attributes(logic1)
attributes(char)
attributes(lst)
attributes(fact)
attributes(mtrx)
attributes(arr)
attributes(mtcars)

#class function
?class
class(num)
class(logic1)
class(char)
class(lst)
class(fact)
class(mtrx)
class(arr)
class(mtcars)

#levels function(for factor only)
?levels
levels(fact)

#dim function(dimensions)
?dim
dim(mtrx)
dim(arr)
dim(mtcars)

#names function
names(mtcars)
row.names(mtcars)

colnames(mtcars)
rownames(mtcars)

#subsetting:"[" operator
?"["
num<-1:4
names(num)
names(num)<-c("a","b","c","d")
num[c(1,3)]
num[1:3]
num[c("a","c")]

mtrx
mtrx[1,3]
mtrx[1,2:3]

lst
lst[1]
lst[c(1,2)]
typeof(lst[1])
typeof(lst[c(1,2)])

#subsetting:"[[" operator
?"[["
lst[[1]]
typeof(lst[[1]])
lst[[1]][2]
lst[[c(1,2)]]

#subsetting:"$"operator
?"$"
mtcars$mpg
typeof(mtcars$mpg)
class(mtcars$mpg)

#subsetting:extract a subset using logical indices
num
indices<-num>=2
indices
num[indices]
num[c(FALSE,TRUE,TRUE,TRUE)]
num[num>=2]

#Control structure: if else
#"{"should be in the same row of if and else
#"else" should be in the same row of "}"
?"if"
?"else"
x<-3
if(x>4){
  y<-1
}else{
  y<-0
}
y

x<-5
y<-2
if(x<=3){
  z<-1
}else if(y>4){
  z<-0
}else{
  z<--1
}
z

x<-5
y<-3
if(x<=2|y>4){
  z<-5
}else{
  z<--5
}
z

#Control structure:for
?"for"
for(i in 1:10){
  print(i+2)
}

char<-c("a","b","c","d","e","f","g")
for(i in 3:length(char)){
  print(char[i])
}

#Control structure
for(i in 1:10){
  if(i<=5){
    print(i+2)
  }else{
    print(i-1)
  }
}

#Loop functions
?apply
?airquality
apply(X=airquality,MARGIN=2,FUN=mean)
apply(airquality,2,mean,na.rm=TRUE)
apply(airquality,c(1,2),is.na)
sum(apply(airquality,c(1,2),is.na))


#Implemented using for loop instead of apply
for(i in 1:length(airquality)){
  var.names<-names(airquality)[i]
  var.mean<-mean(airquality[[i]],na.rm=TRUE)
  print(paste("Avg. of",var.names,"=",var.mean,sep=""))
}

?lapply
lapply(airquality,is.na)
?"function"
na.counts<-lapply(X=airquality,FUN=function(x){sum(is.na(x))})
na.counts
typeof(na.counts)

?sapply
sapply(airquality,is.na)
na.counts<-sapply(airquality,function(x){sum(is.na(x))})
na.counts
typeof(na.counts)

?vapply
vapply(airquality,fivenum,c(Min.=0,"1st Qu."=0,Median=0,"3rd Qu."=0,Max.=0))