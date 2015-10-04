#I confirm that the attached is my own work, except where clearly indicated in the text.

# FUNCTIONS: my.rnorm / my.rchisq / my.rt 

my.rnorm <- function(n,mean=0,sd=1){
#   Purpose:
#     To produce vector of random values from Normal Distribution
#   Input: (all inputs must be numeric)
#     n - number of values to return
#     mean - mean of distribution (default=0)
#     sd - standard deviation of distribution (default=1)
#   Output:
#     Numeric object of 'n' random normally distributed values
  if (n<=0 | sd<0 ) stop("invalid arguments")
  norm_values<- numeric()
  while (length(norm_values) < n) { 
    random_u <- runif(2)
    u1<- (2*random_u[1]) - 1
    u2<- (2*random_u[2]) - 1
    w=(u1**2)+(u2**2)
    if ((w < 1) & (w!=0)) {
      v=sqrt((-2*log(w))/w)
      x1 = u1*v
      x2 = u2*v
      norm_values<- c(norm_values,x1)
      norm_values<- c(norm_values,x2)
    } 
    if (length(norm_values)>n){
      norm_values <- norm_values[-(n+1)]
    }
  }
  return((norm_values*sd)+mean)
}


#______________________________________________________________________________________________


my.rchisq <- function(n,df=1) {
#   Purpose:
#     To produce a vector of random Chi-Squared Distributed Deviates
#   Input: (all inputs must be numeric)
#     n - number of values to return
#     df - degrees of freedom of the distribution (default=1)
#   Output:
#     Numeric object of 'n' random Chi-Squared Distributed Deviates with degrees of freedom, df
  if (n<=0 | df<1) stop("invalid arguments")
  chi_values <- numeric()
  while (length(chi_values) < n) {
    valuesc <- (my.rnorm(df))**2
    valuesc <- sum(valuesc)
    chi_values <- c(chi_values,valuesc)
    }
  return(chi_values)
} 


#______________________________________________________________________________________________


my.rt<- function(n,df=1){
#   Purpose:
#     To produce a vector of random t-distributed deviates
#   Input: (all inputs must be numeric)
#     n - number of values to return
#     df - degrees of freedom of the distribution (default=1)
#   Output:
#     Numeric object of 'n' random t-distributed values with degrees of freedom, df  
  if (n<=0 | df<1) stop("invalid arguments")
  tvalues <- numeric()
  while (length(tvalues) < n){
    denom <- (my.rchisq(n,df))/df
    valuest <- (my.rnorm(n))/sqrt(denom)
    tvalues <- c(tvalues,valuest)
  }
  return(tvalues)
}

#______________________________________________________________________________________________

#FUNCTION TESTS : 

length.test <- function(n,f){
#   Purpose:
#     To test if length of function "f(n)"'s output is equal to input n
#     Also to check functions return odd number of values when n is odd
#   Input: 
#     n - number of values for function to return
#     f - function (my.rnorm / my.rchisq / my.rt)  
#   Output:
#     TRUE - if length of function = n
#     FALSE - if length of function doesnt = n
  if (n == length(f(n))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#_________________________________________________________________________

chi.positive.test <- function(n,df=1){
#   Purpose:
#     To test all generated chi-square values are positive
#   Input: 
#     n - number of values for my.rchisq function to return
#     df - degrees of freedom of the distribution (default = 1)
#   Output:
#     "error: negative chi-square value" - if a negative value is found in my.rchisq output
#     "no error: all values positive" - if no negative values are found in my.rchisq output
  chi_vals<-my.rchisq(n,df)
  for (i in 1:n){
    if (chi_vals[i] < 0) stop("error: negative chi-square value")
     else {
      return("no error: all values positive")
    }
  }
}

#___________________________________________________________________________

output.numeric.test <- function(n,f){
#   Purpose:
#     To test if output of function "f(n)" is numeric
#   Input: 
#     n - number of values for function to return
#     f - function (my.rnorm / my.rchisq / my.rt)  
#   Output:
#     TRUE - if function output is numeric
#     FALSE - if function output is NOT numeric
  return(is.numeric(f(n)))
}

#_____________________________________________________________________________
#tested my normal, chi suare, and t distribution function against R's functions to look for similarly distributed values
