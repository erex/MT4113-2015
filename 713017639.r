## MT4113 ##
## ASSIGNMENT 1 ##
# I confirm that the attached code is my own work, except where clearly indicated in the text.



# PSEUDO-RANDOM NORMAL DISTRIBUTION DEVIATES FUNCTION

my.rnorm  <- function (n, mean=0, sd=1) {
  # Purpose: generate a vector of length n  of pseudo-random normally-distributed deviates.
  # Input: "n" is the number of values to return, "mean" is the mean of values where the default is set to 0,
  #        "sd" is the standard deviation of values where the default is set to 1.
  # Output: n independent and normally distributed deviates.
  # Method: Theorem used from Marsaglia and Bray's method in Maindonald, John (1984) "Statistical Computation".
  #         John Wiley and Sons

  #returns error message if arguments are not in valid bounds
  if (n < 1) stop("invalid n argument. n must be a positive numeric number")
  if (sd < 0) stop("invalid sd argument. NaN")
  if (is.character(n)) stop ("invalid n argument")
  if (is.character(mean)) stop ("invalid mean argument")
  if (is.character(sd)) stop ("invalid sd argument")
  
  w <- v <- numeric(n)
  u1 <- u2 <- x1 <- x2 <- numeric(n)
  
  for (i in 1:n) {
    #Generate a vector of random uniform deviate pairs inside unit circle
    repeat {
      y1 <- runif(1*n)
      y2 <- runif(1*n)
      u1[i] <- 2*y1[i] -1
      u2[i] <- 2*y2[i] -1
      
      #build vector w of pair of deviates inside the unit circle
      w[i] <- (u1[i])^2 +(u2[i])^2 
      if (w[i] < 1) break      #when all w values are inside unit circle stop procedure
    }
    #build vector v from pair of deviates inside the unit circle
    v[i] <- sqrt((-2*log(w[i])) / w[i])
    
    #transfor random uniform distributed deviates pairs to normally distributed deviates pairs
    x1[i] <- u1[i] * v[i]
    x2[i] <- u2[i] * v[i]
}
  #create vector of pairs normally distributed deviates
  pairs <- c(x1,x2)
  
  #sample randomly n number (or half) of the vector pairs into one vector X
  X <- sample(pairs,n)
  
  #transform this vector of random normal deviates depending on input of mean and sd
  X <- X*sd + mean
  
  #return X transformed
  return (X)
}





# PSEUDO-RANDOM CHI-SQUARED DISTRIBUTION DEVIATES FUNCTION

my.rchisq <- function (n, df=1) {
  # Purpose:  generate a vector of length n  of pseudo-random chi-squared distributed deviates 
  # Input: "n" number of values to return, "df" degree of freedom with default 1
  # Output: vector of n randomly chi-squared distributed deviates
  # Method: Theorem used from Larsen and Marx (1981) "An introduction to mathematical statistics
  #         and its applications" Prentice-Hall INc.
  
  #returns error message if arguments are not in valid bounds
  if (n < 1) stop("invalid n argument. n must be a positive numeric number")
  if (df < 1) stop("invalid n argument. n must be a positive numeric number")
  if (is.character(n)) stop ("invalid n argument")
  if (is.character(df)) stop ("invalid df argument")
  
  x <- rn <- numeric(n)
  #in a chi-squared distribution the degree of freedom s equal to the number of standard normal deviates being summed
  df <- n 
  
  for (i in 1:n){
    #generate a vector of independent standard normal random deviates
    rn[i] <- my.rnorm(1)
    #build a vector of chi-squared deviates of random standard normal deviates squared being summed
    x[i] <- x[i] + (rn[i])^2 
  }
  #return vector of random chi-squared deviates
  return(x)
}




# PSEUDO-RANDOM t-DISTRIBUTION DEVIATES FUNCTION

my.rt <- function (n, df=1) {
  # Purpose: generate a vector of length n  of pseudo-random t-distributed deviates 
  # Input: "n" number of values to return, "df" degree of freedom with default value 1
  # Output: vector of n randomly t-distributed deviates
  # Method: Theorem used from Mood, A.M., F.A. Graybill and D.C. Boes (1974) "Introduction to 
  #         the theory of Statistics." Third Edition. McGraw Hill
  
  if (n < 1) stop("invalid n argument. n must be a positive numeric number")
  if (df < 1) stop("invalid n argument. n must be a positive numeric number")
  if (is.character(n)) stop ("invalid n argument")
  if (is.character(df)) stop ("invalid df argument")
 
  x <- rn <- rch <- y <- numeric(n)
  df <- n
  
  for (i in 1:n) {
    #generates a vector of random normal deviates
    rn[i] <- my.rnorm(1)
    #generates a vector of random chi-squared deviates
    rch[i] <- my.rchisq(1)
   
    #build vector of t-distributed deviates with df degrees of freedom
    y[i] <- ( rn[i] / sqrt(rch[i]/df) )
  }
  #return vector of randomly t-distributed deviates
  return (y)
}






# TEST FUNCTIONS

pass.test <- function(n) {
  #Purpose: test pseudo-random deviates 
  #input:  "n" number of deviates gene
  #output: return logical vector TRUE if pass all test or FALSE if fails
  x <- my.rnorm(n)
  
  t1 <- is.numeric(x)
  t2 <- length(x) > 1
  t3 <- length(x) == n
  return (c(t1,t2,t3))
}


pass.test2 <- function (n) {
  #Purpose: test pseudo-random deviates my.rchisq
  #input: n number of rchisq deviates to be generated
  #output: return logical vector TRUE if pass all test or FALSE if fails
  x <- my.rchisq(n)
  
  t1 <- is.numeric(x)
  t2 <- length(x) > 1
  t3 <- length(x) == n
  return (c(t1,t2,t3))
  
}


pass.test3 <- function (n) {
  #Purpose: test pseudo-random deviates my.rt
  #input: n number of rt deviates to be generated
  #output: return logical vector TRUE if pass all test or FALSE if fails
  x <- my.rt(n)
  
  t1 <- is.numeric(x)
  t2 <- length(x) > 1
  t3 <- length(x) == n
  return (c(t1,t2,t3))
}

