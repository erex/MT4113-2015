# I confirm that attached is my own work, except where clearly indicated in the text.


#-----------------------------RANDOM NUMBER GENERATING FUNCTIONS--------------------------------#


my.rnorm <- function(n, mean= 0, sd= 1) {                   

# Purpose: generate normal random numbers with the Marsaglia and Bray's Method
# Input: 
#      n - number of values to return (must be a positive and integer scalar)
#   mean - mean of values to return (default = 0)
#     sd - stand dev of values to return (default = 1)
# Ouput:
#      A vector with length n of normal deviates
  
 if((class(n)=="character") || (class(mean)=="character") || (class(sd)=="character")) stop("invalid arguments")
 if((length(n) != 1) || (n%%1 != 0) || (n <= 0)) stop("invalid arguments")       
  
 if(n%%2 != 0) {              #next 3 lines adapt the function to odd values of n by adding 1 to n
   g <- n + 1 
 } else {g <- n} 
   
 a <- c()              # a vector with all random numbers satisfying the condition
 b <- c()              # a vector with all the evaluations for the criterion
 
 g <- g/2
 for(i in 1:g) {
  u <- c(2,2)            # vector with an initial pair of numbers (to start the while loop)
  w <- sum(u^2)         # a vector for the condition (same rationale as above)
   while( w > 1) {        # checks for the right pair of numbers
   u <- runif(2, 0, 1)
   k <- (2*u)-1
   w <- sum(k^2)      
   } 
   a <- append(a, k, after = length(a))
   b <- append(b, w, after = length(b))       # NB: its length is half of a!!
 }

 v <- rep(sqrt((-2*log(b))/b), each= 2)
 rsnorm <- (sd*a*v) + mean                 # vector with normally distr random numbers
 if(n%%2 != 0) rsnorm <- head(rsnorm, -1)   # if n was odd, this line drops the last element of the random vector
 return(rsnorm)
}



my.rchisq <- function(n, df=1) {
  
# Purpose: Generate random Chi-squared-distributed numbers with the Larsen and Marx method.
# Input:
#       n - number of values to return (must be a positive and integer scalar)
#       df - degrees of freedom (must be a positive and integer scalar)
# Output: 
#       a vector with length n of random-chi-squared numbers with df degrees of freedom
 
 if((class(n)=="character") || (class(df)=="character")) stop("invalid arguments")  
 if((length(n) != 1) || (length(df) !=1)) stop("invalid arguments")       
 if((n%%1 != 0) || (df%%1 != 0)) stop("invalid arguments")
 if((n <= 0) || (df <= 0)) stop("invalid arguments")
    
 chi2 <- c()
 for (i in 1:n) {                         # the for loop generates the vector chi2 of length n using st.norm.random vectors of length df
   d <- my.rnorm(df)
   k <- sum(d^2)
   chi2 <- append(chi2, k, after= length(chi2))
 }
 return(chi2)
}



my.rt <- function(n, df=1) {

    
# Purpose: Generate random Student-t distributed numbers with the Mood, Graybill and Boes method.
# Input:
#       n - number of values to return (must be a positive and integer scalar)
#       df - degrees of freedom (must be a positive and integer scalar)
# Output: 
#       a vector with length n of random Student-t numbers with df degrees of freedom  
  
  if((class(n)=="character") || (class(df)=="character")) stop("invalid arguments")  
  if((length(n) != 1) || (length(df) !=1)) stop("invalid arguments")       
  if((n%%1 != 0) || (df%%1 != 0)) stop("invalid arguments")
  if((n <= 0) || (df <= 0)) stop("invalid arguments")

  y <- my.rnorm(n,0,1)/sqrt((my.rchisq(n,df)/df))               
  return(y)
}





#------------------------------FUNCTIONs TESTING------------------------------------------------#

# The following functions perform the Kolmogorov-Smirnov test to check whether the random numbers 
# generated by "mine" functions possibly come from the assumed distributions (Normal, Chi-sqr and 
# Student t).

# Null Hypothesis: x has been generated by the distribution y (i.e. second term of the syntax).
# Level of confidence chosen: alpha = 0.05 (i.e. reject the null if p-value < alpha).


my.norm.test <- function(m=0, stdev=1) {
  
# Purpose: Test of Normality fit
# Input: x - numeric vector to be tested
#        m - mean of the testing norm distr 
#    stdev - standard deviation of the testing norm distr
# Output: 
#        A character string indicating the test outcome
  
  k <- ks.test(my.rnorm(1000), pnorm, m, stdev)
  pass.test <- ((k$p.value < 0.05)== FALSE) 
  return(cat("Does it pass the test?", pass.test, "\n"))
}



my.chisq.test <- function(df=1) {
  
# Purpose: Test of Chi-sqrd fit 
# Input: x - numeric vector to be tested
#       df - degrees of freedom of the testing chi sqr
# Output: 
#        A character string indicating the test outcome
  
  w <- ks.test(my.rchisq(1000), pchisq, df)
  pass.test <- ((w$p.value < 0.05)== FALSE) 
  cat("Does it pass the test?", pass.test, "\n")
}



my.t.test <- function(df=1) {
  
# Purpose: Test of Student t fit 
# Input: x - numeric vector to be tested
#       df - degrees of freedom of the testing t
# Output: 
#        A character string indicating the test outcome  
  
  h <- ks.test(my.rt(1000), pt, df)
  pass.test <- ((h$p.value < 0.05)== FALSE) 
  cat("Does it pass the test?", pass.test, "\n")
}


# The following function tests for invalid input arguments in my.rnorm function

test.error.norm <- function(n) {
  
# Purpose: Test my.rnorm behaviour with invalid arguments
# Input: n - a value
# Output: 
#        A character string indicating the test outcome
   
   
 k <- tryCatch(my.rnorm(n), error= function(e) e)
 w <- any(class(k) == "error")
 if(w==TRUE) {
   cat("Test Passed \n")
 } else {
   cat("Test Failed \n")
 } 
 cat("Tester value:", n, "\n")
}







