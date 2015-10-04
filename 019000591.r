#I confirm that the attached is my own work, except where clearly indicated in the text


#uniform to normal
my.rnorm <- function(n, mean=0, sd=1){
  #return a vector of random normal values
  #input n ,number of values to return,no default
  #input mean ,mean of values to return, default o 
  #input sd, sd of values to return,default 1

  if(!is.numeric(n)) stop("invalid arguments",call.=FALSE)
  if(n<=0) stop("invalid arguments",call. = FALSE)
  if(!n%%1==0) stop("invalid arguments",call. = FALSE)
  if(!is.numeric(mean)) stop("invalid arguments",call.=FALSE)
  if(mean<0) stop("invalid arguments",call. = FALSE)
  if(!mean%%1==0) stop("invalid arguments",call. = FALSE)
  if(!is.numeric(sd)) stop("invalid arguments",call.=FALSE)
  if(sd<=0) stop("invalid arguments",call. = FALSE)
  if(!sd%%1==0) stop("invalid arguments",call. = FALSE)
  
  output.myrnorm = NULL
  if (n<0|sd<0){
    stop("invalid arguments")
  } 
#Produce 2 random uniform numbers 
   counter <- 0

repeat{
  r.unif= runif(2,0,1)
    for (i in 1:2){
    r.unif[i]=(r.unif[i])
} 

#2 uniform numbers across [-1,1] square
s.unif=((r.unif[i]*2)-1)
    for (i in 1:2){
    s.unif[i]=c((r.unif[i]*2)-1)
}

#3 If s.unif squared is greater than 1 then repeat until results are less than 1
  w = sum(s.unif^2)

#4 If u1+u2>1 repeat until <=1
  while(w>1){
    r.unif= runif(2,0,1)
      for (i in 1:2){
      s.unif[i]=c((r.unif[i]*2)-1)
  }
      w = sum(s.unif^2)
  }

# 5 - Produce v
  v=sqrt((-2*log(w)/w))

# 6 Define X
  x=c((s.unif[i]*v) * 1 + 0)
  for (i in 1:2){
    x[i]=c((s.unif[i]*v))
}
#Print the results of my.rnorm

   output.myrnorm <- c(output.myrnorm,x)
   if(counter == n-1) break
   counter <- counter+1}          #assistance received with counter
   length(output.myrnorm) <-n
    return(output.myrnorm)
   }

## Normal to Chi-square
my.rchisq = function(n, df=1){
  
  #function that creates a vector of random values from chi-square distribution
  #input n:number of values to return,no default
  #input df:degree of freedom of the distribution,default 1
  
  output.chi=NULL
  if(!is.numeric(n)) stop("invalid arguments",call.=FALSE)
  if(n<=0) stop("invalid arguments",call. = FALSE)
  if(!n%%1==0) stop("invalid arguments",call. = FALSE)
  if(!is.numeric(df)) stop("invalid arguments",call.=FALSE)
  if(df<=0) stop("invalid arguments",call. = FALSE)
  if(!df%%1==0) stop("invalid arguments",call. = FALSE)
  
  counter = 0
  repeat{
    yi= my.rnorm(n,0,1)
    yisq = sum(yi^2)
    output.chi =(c(output.chi,yisq))
    if(counter == n-1) break
    counter <- counter+1}
  length(output.chi) <-n
  return(output.chi)
  }


##Chi to t
my.rt = function(n, df=1){
  #function that generates a vector of random values with an t-distribution
  
  #input n:number of values to return,no default
  #input df:degree of freedom of the distribution,default 1
  
  output.myrt=NULL
  
  if(!is.numeric(n)) stop("invalid arguments",call.=FALSE)
  if(n<=0) stop("invalid arguments",call. = FALSE)
  if(!n%%1==0) stop("invalid arguments",call. = FALSE)
  if(!is.numeric(df)) stop("invalid arguments",call.=FALSE)
  if(df<=0) stop("invalid arguments",call. = FALSE)
  if(!df%%1==0) stop("invalid arguments",call. = FALSE)
  
   counter = 0
      
    rt.numerator = my.rnorm(n)
      rt.denominator = sqrt(my.rchisq(n)/1)
      rt = rt.numerator/rt.denominator
      length(output.myrt) <-n
      output.myrt <- (c(output.myrt,rt))
    return(output.myrt)
        }

###Test Functions######

#Tests for my.rnorm###

## Does it return the correct number of values
x <- my.rnorm(n=10)
pass.test.1 <- (length(x)==10 & is.numeric(x))
pass.test.1

## Sharipo test. High P value suggests normality
shapiro.test(my.rnorm(1000,0,1))

## Test for normality using QQ plot and histogram
par(mfrow=c(1,2)) 
test.norm.1 <- my.rnorm(10000,0,1)
qqnorm(test.norm.1)
qqline(test.norm.1, col="red")
hist(my.rnorm(10000,0,1))


#Tests for my.rchisq#####

## Does it return the correct number of values
y <- my.rchisq(n=10)
pass.test.1 <- (length(x)==10 & is.numeric(x))
pass.test.1

## Test that values are >0 & produces 10 values
neg.chisq <- my.rchisq(n=10)
pass.test.2<- (length(neg.chisq)==10 & neg.chisq>0)
pass.test.2

## Mean is close to degree of freedom, supports chi-square distribution
mean(my.rchisq(10000,1))
 
## Variance close to 2 degrees of freedom, supports chi-square distribution
var((my.rchisq(10000,1)))
 

## approaches normal distribution for large degrees of freedom
hist(my.rchisq(1000,200))


##Tests for t-distribution

## Does it return the correct number of values
z <- my.rt(n=10)
pass.test.3 <- (length(x)==10 & is.numeric(x))
pass.test.3

## Produces 10 values & all >0
neg.rt <- my.rt(n=10)
pass.test.4 <- (length(neg.rt)==10 & neg.rt>0)
pass.test.4


