# Assignment 1
#I confirm that the attached is my own work, except where clearly indicated in the text.

# Aim of Assignment: 
# From output runif(), write functions: my.rnorm, my.rchisq and my.rt
# to produce random deviates conforming to three distributions : normal, x^2 and student's t

# 1st function : my.rnorm (Using Marsaglia and Bray algorithm)
# Purpose : To return pseudo-random values from a normal distribution 
#           with arguments: n - no default, mean - default 0 and sd - default 1
# Input   : n - the number of random normal variates the user wants to generate
# Output  : n random normal variates 

my.rnorm<-function(n=n,mean=0,sd=1){ #Define my.rnorm with no default for n, default mean=0 and sd=1
  if (missing(n)|| n<=0){    # executes " invalid arguments" when input is incorrect
    stop("invalid arguments")
  }
  count=0 
  results=list() 
  
  if(n%%2==0){  # if n is an even number
    while(count<n/2){ 
      u <- runif(2)   # Marsaglia and Bray's algorithm in code
      u <- 2*u-1      
      w <- sum((u)^2)
      if (w > 1){     # if w >1 go through loop again to find another pair of random uniform deviates
      }
      else if(w <= 1){    # if uniform deviates satisfy criterion continue with the rest of algorithm     
        v <- sqrt((-2*log(w))/w) 
        x <- u*v  
        xt <- x*sd + mean # transform values if mean and sd does not equal to the default
        results[[length(results)+1]] <- xt # appends new elements to list
        count=count+1                    
      }
    }
    results_2<-unlist(results) # generates even number of random normal deviates
    
  }
  else if(n%%2!=0){ # if n is an odd number
    while(count<n/2){
      u <- runif(2)   
      u <- 2*u-1      
      w <- sum((u)^2)
      if (w > 1){ 
      }
      else if (w <= 1){         
        v <- sqrt((-2*log(w))/w) 
        x <- u*v  
        xt <- x*sd + mean
        results[[length(results)+1]] <- xt
        count=count+1  
      }
      results_2<-unlist(results) 
      results_2<-results_2[-1] # get rid of one element to produce odd number of random normal deviates
      
      
    }
    
    
  }
  return(results_2) # returns n number of random normal deviates
}

# 2nd function: my.rchisq
# Purpose : To return psuedo-random x^2 distributed deviates
# Input   : n - number of random x^2 deviates the user wants to generate
# Output  : n number of random x^2 deviates

my.rchisq <- function(n=n, df=1){ # define my.rchisq, set no default for n and default df = 1
  if (missing(n)|| n<=0){    
    stop("invalid arguments")
  }
  chi = NULL  
  for (i in 1:n){                        
    chi[i] <- sum((my.rnorm(n))^2) # x^2 distribution algorithm in code
   # c[1] is the sum of squares of each deviate in the first set of random normal deviates from rnorm(n)
   # which will produce our first x^2 deviate (z1^2 + z2^2 + ... zn^2 ~ x^2)
   # this is done up till the nth set of random normal deviates to produce n number of x^2 deviates
  }                                        
  return (chi) # returns n number of random x^2 deviates
}

# 3rd function: my.rt
# Purpose : To return pseudo-random t-distributed deviates
# Input   : n - number of random t-distributed deviates the user wants to generate
# Output  :n number of random t-distributed deviates

my.rt <- function(n=n, df=1 ){ # define my.rt, set no default for n and default df=1
  if (missing(n) || n<=0){   
    stop("invalid arguments")
  }
  z <- my.rnorm(n)
  c <- sqrt(my.rchisq(n))
  k <- df
  t <- z/(c/k) # student- t distribution algorithm in code
  # t = random uniform variate/sqrt(x^2 random variate/degrees of freedom)
  #cat("z=",z, "c=",c)
  return(t)   # returns n number of random t-distributed deviates
}

# Purpose : To test the functions: my.rnorm, my.rchisq and my.rt, 
#           specifically if it produces length n random deviates and if its numeric
# Input   : n - the number of deviates the user generates
# output  : Statements Pass or fail to check if the functions work 
pass.test <- function(n){
  for ( i in 1:n){
    a <- (length(my.rnorm(n)) == n & is.numeric(my.rnorm(n)))   # returns logic statement true or false
    b <- (length(my.rchisq(n)) == n & is.numeric(my.rchisq(n))) 
    d <- (length(my.rt(n)) == n & is.numeric(my.rt(n)))}
    #cat("a=",a,"b=",b,"d=",d)
    if (a & b & d == TRUE){  # if all true then print all pass i.e our functions work!
     print ("ALL PASS") 
    }else {   # if some or all fail then print fail i.e some or all of our functions don't work
      print(" FAIL")
   }
}