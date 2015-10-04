# mengqi zhao 110017255 
#I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm<-function(n,mean=0,sd=1){ 
  #purpose: call the function my.rnorm to get normal distributed random variables
  #input: n, the number of random variables wanted. mean,default 0. sd,standard deviation default 1
  #output: n numbers of random variables that are from normal distribution 
  if(sd<0|n<0) stop("invalid arguments") # sd and n must be positive (or =0)
  if(n%%1!=0) stop("invalid arguments") # n must be an integer 
  x<-c() # a empty vector which later I will use to store my results 
  for(i in 1:ceiling(n/2)){ 
    getw<-c(u<-runif(2),ut<-2*u-1,w<-(ut[1])^2+(ut[2])^2) # get w, w is the fifth element in the getw vector 
    while(getw[5]>1){
      getw<-c(u<-runif(2),ut<-2*u-1,w<-(ut[1])^2+(ut[2])^2) # while w>1, getw will run agian and agian until a new w<=1
    }
    v<-sqrt((-2)*(log(getw[5]))/(getw[5])) # for w<=1, v is calculated 
    x1<-ut[1]*v # a pair of normally distributed random variables 
    x2<-ut[2]*v
    x<-c(x,x1,x2) # combine and store the results in x 
    i<-i+1 
    
  }
  if(n%%2!=0){ # when input n is odd, delte one element from x (same as take only one element from a pair)
   return(x[-1])
  }else{ # when input n is even, simply just return x 
    return(x)
  }
}


my.rchisq<-function(n,df=1){
  #purpose: call the function my.rchisq to generate chisq distributed deviates 
  #input: n, the numbers of chisq deviates wanted. df, degree of freedom, default 1
  #output: n numbers of random variables from chisq distribution with degree of freedom 1 
  if(n<0|df<0)stop("invalid arguments") # n and df must be positive integers (or =0)
  z<-c() 
  for(i in 1:n){
    z[i]<-sum(my.rnorm(df)^2) #generate a vector from my.rnorm(df), then take sum of their component-wise square
  }
  return(z) # simply return the results 
}


my.rt<-function(n,df=1){
  #purpose: call the function my.rt to generate student's t-distributed deviates 
  #input: n, the number of deviates wanted, df, degree of freedom, default 1. 
  #output: n numbers of deviated with df=1 
  if(n<0|df<0)  stop("invalid arguments") # n and df must be positve (or=0)
   t<-c() 
  for(i in 1:n){
    z<-my.rnorm(1,0,1) # using the formula provided to calculate t 
    u<-sqrt(my.rchisq(1,df)/df)
    t[i]<-z/u
  }
  return(t) # simply return t 
}


#testing for my.rnorm, use shapiro-wilk test.  
#the null hypothesis is: the data set is normally distributed
test.my.rnorm<-function(n){ 
  #purpose: call the function test.my.rnorm to test normality of my.rnorm 
  #input: n, number of times testing 
  #output:p-values for each test 
  x<-c()
  for(i in 1:n){ 
    # use the shapiro test attach the p-value, to test my.rnorm(3000,0,1), sample size 3000, mean=0, sd=1
    x[i]<-shapiro.test(my.rnorm(3000,0,1))$p.value
    # run the simulation n times and obtian all p-values 
  }
  return (x)
}

hist(test.my.rnorm(300),breaks=30)
 
# run the test 300 times and plot all p-values
# from the histogram, we can see that the p-value is very big,
# so can not reject the hypothesis that the sample comes from a normal distribution. 
#inaddtion, one can use qqnorm in R to test normality. 
#If the datd set is normally distributed, then a trightline is expected.
#plot my.rnorm with a sample zise 3000, the qqnorm plot is a strightline. 
#hence, my.rnorm is normally distributed 
qqnorm(my.rnorm(3000))

qqnorm(my.rnorm(4000))
qqnorm(my.rnorm(2000))
qqnorm(my.rnorm(5000))
#repeat for several times, same results.

# After some research, I decided to use Kolmogorov-Smirnov test to test my.rchisq and my.rt
#KS test ( 2 sample test), comapraing my.rchisq with pchisq and my.rt with pt
# the null hypothesis of KS test is: the two samples are from the same distribution. 
# by plotting the p-values, we can see that the p-value is very big, so can not reject the null hypothesis. 

test.my.rchisq<-function(n,df){
  y<-c()
  for(i in 1:n){
    y[i]<-ks.test(my.rchisq(1000,df),pchisq,df)$p.value
  }
  return(y)
}
hist(my.rchisq(1000,df=1))
hist(rchisq(1000,df=1))

hist(test.my.rchisq(1000,1),breaks=30)
hist(test.my.rchisq(1000,2),breaks=30)
hist(test.my.rchisq(2000,1),breaks=30)

# hence my.rchisq and pchisq are from the same chisq distribution  

test.my.rt<-function(n,df){
  y<-c()
  for(i in 1:n){
    y[i]<-ks.test(my.rt(1000,df),pt,df)$p.value
  }
  return(y)
}


hist(test.my.rt(1000,1),breaks=30)
hist(test.my.rt(1000,2),breaks=50)
hist(test.my.rt(2000,1),breaks=30)
# from the histograms above, the p-values are very big
# we can not reject the null hypothesis 
#hence my.rt and pt are from the same t-distribuition 