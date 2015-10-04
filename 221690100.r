# I comfirm that the attachment is my own work, except where clearly indicated in the text.


my.rnorm <- function (n,mean=0,sd=1) {
#Purpose: Call this function to generate n random deviates from a normal distribution.
#Inputs: n - number of random normal deviates required
#Outputs: a vector of n pseudo-random values from a normal distribution
  
  #Error check on input
  if(n<0) stop ('Input "n" must be positive')
  if(length(n) !=1) stop ('Input "n" must be a scalar')
  
    for (i in 1:n){
    x <- runif(2) #Assigning vector x to 2 random deviates from a uniform distribution
    x <- 2*x-1
    
    w <- x[1]^2+x[2]^2
    
    while (w>=1) {
      v <- sqrt((-2*log(w))/w)
    }
    u[i] <- v*x
  }
  return (u)
}




my.rchisq <- function (n,df=1){
#Purpose: Call this function to generate n random deviates from a chisquare distribution
#Inputs: n - number of random chisquared deviates required
#Outputs: a vector of n pseudo-random values from a chisquare distribution
#Note: rnorm used instead of my.rnorm
  
  #Error check on input
  if(n<=0) stop ('Input "n" must be positive')
  
  x <- NULL
  for (i in 1:n) {
    x [i] <- rnorm(df,mean=0,sd=1)
    x [i] <- (x[i]^2)
    x [i] <- sum(x)
    
  }
  return (x)
}





my.rt <- function (n,df=1){
#Purpose: Call this function to generate n random deviates from a t distribution
#Inputs: n - number of random t deviates required
#Outputs: a vector of n pseudo-random values from a t distribution
#Note: rnorm used instead of my.rnorm  
  
  #Error check on input
  if(n<=0) stop ('Input "n" must be positive')
  
  t <- NULL
  for (i in 1:n) {
    z [i] <- rnorm(1,mean=0,sd=1)
    u [i] <- my.rchisq(1,df=1)
    t [i] <- z[i]/(sqrt(u[i]/df))
    
  }
  return (t)
}



my.rchisq.test1 <- function (n) {
#Purpose: This function checks if the function my.rchisq produces a vector of n numbers when asked.
  x <- my.rchisq(n)
  if (length(x)==n & is.numeric(x)) print ("Yes")
}



my.rt.test1 <- function (n) {
  #Purpose: This function checks if the function my.rt produces a vector of n numbers when asked.
  x <- my.rt(n)
  if (length(x)==n & is.numeric(x)) print ("Yes")
}


my.rchisq.test2 <- function (n) {
#Purpose: This function checks if the deviates returned by my.rchisq are all positive.
  x <- my.rchisq(n)
  
  for (i in 1:length(x)) {
    if(x[i]>0) print ("Yes")
  }
}


my.rchisq.test3 <- function (n) {
#Purpose: This function checks whether my.rchisq throws an error message if n is negative.
  x <- my.rchisq(n)
  if (n <0) expect_that (x,throws_error())
}


my.rt.test2 <- function (n) {
  #Purpose: This function checks whether my.rt throws an error message if n is negative.
  x <- my.rt(n)
  if (n <0) expect_that (x,throws_error())
}