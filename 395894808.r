
#I confirm that the attached is my own work, except where clearly indicated in the text.

## my.rnorm function

my.rnorm<- function(n, mean=0, sd=1) 
  { 
d=NULL
# check for invalid arguments
if (n<=0 || (n%%1!=0))  
    {stop("invalid arguments")}

# if n is an even number
if(n%%2==0)
{ { 
# n/2 iterations as the function generates pairs of normally distributed variates
for (i in seq(1, n, 2)) 
{ 
y<- runif(2)*2-1   
w<- y[1]^2 + y[2]^2 
while (w>1) 
{ 
y<- runif(2)*2-1 
w<- y[1]^2 + y[2]^2 
} 
v<- sqrt(((-2)*log(w))/w)
#create the vector that my.rnorm returns, inserting two numbers per iteration and keeping the values of the previous iterations 
d[i]<- y[1]* v     
d[i+1]<- y[2]*v 
} 
} 
} 
else
#if n is an odd number
{ 
{ 
# n iterations because the generation of normal distributed numbers doesnt take place in pairs. To be more specific, in this for loop essentially pairs of numbers that follow the uniform distribution are generated, then in their turn they generate by the Marsaglia and Bray's algorithm a pair of normally distributed numbers and basically the sample function reject one value of the pair and keeps the other 
for (i in 1:n)
{ 
y<- runif(2)*2-1 
w<- y[1]^2 + y[2]^2 
while (w>1) 
{ 
  y<- runif(2)*2-1 
  w<- y[1]^2 + y[2]^2 
} 
v<- sqrt(((-2)*log(w))/w) 
k<- sample(1:2,1) 
d[i]<- y[k]* v      #
} } } 
return(d) 
} 



##pass.test function takes the value TRUE if the the output 
##of the function my.rnorm contains n numeric values
pass.test<- function(n)
{
  r<- (length(my.rnorm(n))==n & is.numeric(my.rnorm(n)))
  return(r)
}

##compare the histogram of the number generated 
##by this function with normal distribution histogram and qq-plot.Test skewness and kurtosis for the sample and finally shapiro test for normality. (big samples needed)
histogr.norm<- function(n)
{
x<- my.rnorm(n)
y<- rnorm(n)
rhist<- hist(x,prob=TRUE,col="green",main="Normal Distribution ")
rhist1<- hist(y,prob=TRUE,col="blue",main="Normal Distribution ")
rqq<- qqnorm(x)
rqq1<- qqnorm(y)
sk<-skewness(x)
kur<- kurtosis(x)
test<- shapiro.test(x)
return(list(sk,kur,test))
}




# my.rchisq function returning a vector z of pseudo-random chi-squared distributed deviates
my.rchisq<- function(n,df=1) 
{ 
  if (n<=0 || (n%%1!=0))  
  {stop("invalid arguments")}
k<- my.rnorm(n)
z<- NULL 
for(i in 1:n) 
{ 
z[i]<- k[i]^2 
} 
return(z) 
} 


##pass function takes the value TRUE if the the output 
##of the function my.rchisq contains n numeric values
pass<- function(n)
{
  r<- (length(my.rchisq(n))==n & is.numeric(my.rchisq(n)))
  return(r)
}


##compare the histogram of the  generated numbers
##from this function with chi-squared distribution graph with degree of freedom 1 
histogr<- function(n)
{
  x<- my.rchisq(n)
  y<- rchisq(n,1)
  rhist1<- hist(x,prob=TRUE,col="green",main="Chi-squared Distribution ")
  rhist2<- hist(y,prob=TRUE,col="blue",main="Chi-squared Distribution ")
  return(list(rhist1,rhist2))
}



#my.rt function  returning the vector t of pseudo-random t-distributed deviates
my.rt<- function(n, df=1) 
{ 
if (n<=0 || (n%%1!=0))  
{stop("invalid arguments")}
t<- NULL 
t<- my.rnorm(n)/sqrt(my.rchisq(n)) 
return(t)   
} 


##pass.rt function takes the value TRUE if the the output 
##of the function my.rt contains n numeric values
pass.rt<- function(n)
{
  r<- (length(my.rt(n))==n & is.numeric(my.rt(n)))
  return(r)
}


##compare the histogram of the  generated numbers
##from this function with chi-squared distribution graph with degree of freedom 1 
histogr.rt<- function(n)
{
  x<- my.rt(n)
  y<- rt(n,1)
  rhist1<- hist(x,prob=TRUE,col="green",main="Student's t-Distribution ")
  rhist2<- hist(y,prob=TRUE,col="blue",main="Student's t-Distribution ")
  return(list(rhist1,rhist2))
}


