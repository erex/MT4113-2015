#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n,mean=0,sd=1) {
#  Purpose:
#    Generate random numbers from a normal distribution
#  Input:
#    n - how many random normally-distributed values to produce
#    mean - mean of normal distribution (0 by default)
#    sd - standard deviation of normal distribution (1 by default) 
#  Output:
#    vector containing n random normally-distributed numbers

  
  # Error checking on inputs:
  # First check that no character or logical values have been entered:
  if (is.numeric(n)==F | is.numeric(sd)==F | is.numeric(mean)==F) {
    stop("invalid arguments")
  }
  # Then check that scalar arguments have been entered, subject to certain restrictions for n and sd:
  if (!length(n) == 1 | !length(mean) == 1 | !length(sd) == 1 | n <= 0 | sd < 0) {
    stop("invalid arguments")
  }
  # Finally check that an integer number of values has been requested:
  if (!n%%1 == 0) {           
    stop("invalid arguments")   
  }
  
  # Create a vector to store the results:
  X <- vector(length=n)
  
  # Set up function to behave differently for odd/even numbers:
  if (n%%2 == 1) {         # if the number of random values required is odd...
    n <- n+1               # ... instead consider n+1 random values (needed since the method generates pairs of random numbers)  
    odd <- TRUE
  } else {
    odd <- FALSE
  }
  
  # Run Marsaglia and Bray's algorithm to find and store pairs of standard normal random deviates:
  for (i in 1:(n/2)) {
    w <- 11/10          
    while (w > 1) {
      U <- runif(2)   
      U <- 2*U - 1    
      w <- U[1]^2 + U[2]^2
    }
    v <- sqrt((-2*log(w))/w)
    
    # The if-else construct below works as follows:
    # When an odd number of random deviates has been requested, and the code reaches the last pair of
    # random values to generate in the loop, only one random number will need to be stored from the pair.
    # In all other cases, both random numbers need to be stored.
    if (odd & i==(n/2)) {    
      X[2*i-1] <- U[1]*v     
    } else {
      X[c(2*i-1,2*i)] <- U*v   
    }
    
  }
  
  #  Transform output if non-standard mean or sd required:
  if (!sd==1) {
    X <- sd*X      
  }
  if (!mean==0) {
    X <- X + mean   
  }
  
  return(X)
}
  

my.rchisq <- function(n, df=1) {
  #  Purpose:
  #    Generate random numbers from a chi-squared distribution
  #  Input:
  #    n - how many random chi-squared distrubuted values to produce
  #    df - the number of degrees of freedom the distribution has
  #  Output:
  #    vector containing n random chi-squared distributed numbers
  
  # Error checking on input (n):
  # Note: error checking for df will be done by the my.rnorm function
  if (is.numeric(n)==F | !length(n) == 1 | n <= 0) {
    stop("invalid arguments")
  }  
  # Checking that n is an integer is done next since it can cause issues with character inputs
  if (!n%%1 == 0) {
    stop("invalid arguments")
  }
  
  # Set up a vector to store the results:
  S <- vector(length=n)

  # Run algorithm to transform normal random deviates into chi-squared random deviates:
  for (i in 1:n) {
    Z <- my.rnorm(df)
    S[i] <- sum(Z^2)
  }
  return(S)
}


my.rt <- function(n, df=1) {
  #  Purpose:
  #    Generate random numbers from a Student's t distribution
  #  Input:
  #    n - how many random numbers to produce
  #    df - the number of degrees of freedom the distribution has
  #  Output:
  #    vector containing n random numbers
  
  # Error-checking on inputs:
  # Again, error checking for df will be done by the my.rnorm function
  if (is.numeric(n)==F | !length(n) == 1 | n <= 0) {
    stop("invalid arguments")
  } 
  # Checking that n is an integer is done next since it can cause issues with character inputs
  if (!n%%1 == 0) {
    stop("invalid arguments")
  }
  
  # Set up a vector to store the results:
  t <- vector(length=n)
  
  # Run algorithm to generate student's t-distributed random deviates:
  for (i in 1:n) {
    Z <- my.rnorm(1)
    U <- my.rchisq(1,df)
    t[i] <- Z/sqrt(U/df)
  }
  return(t)
}

# **TESTING FUNCTIONS**
# Below are functions which can test the three functions defined above

output.test <- function(n) {
  #  Purpose:
  #    Check if each function produces the correct number of random numerical deviates
  #  Input:
  #    n - the number of random values to generate
  #  Output:
  #    A logical list, with TRUE for correct output for each distribution (n random numbers generated) and FALSE otherwise
  
  x <- my.rnorm(n)
  logic.x <- (length(x)==n & is.numeric(x))
  y <- my.rchisq(n)
  logic.y <- (length(y)==n & is.numeric(y))
  z <- my.rt(n)
  logic.z <- (length(z)==n & is.numeric(z))
  return(list(norm=logic.x,chisq=logic.y,t=logic.z))
}


expected.values.test <- function(n,mean=0,sd=1,df=1) {
  #  Purpose:
  #    Compare expected mean and variance values for the three distributions with observed values for my functions
  #  Input:
  #    n - the number of random values to generate for each function
  #    mean - the mean of the normal distribution (default=0)
  #    sd - the standard deviation of the normal distribution (default=1)
  #    df - the number of degrees of freedom the chi squared and t distributions have (default=1)
  #  Output:
  #    A list with the differences between observed and expected values for mean and variance for all 3 functions
  
  # Comparisons for Normal distribution:
  norm.mean <- mean(my.rnorm(n,mean,sd))
  true.norm.mean <- mean
  a <- norm.mean - true.norm.mean
  norm.var <- var(my.rnorm(n,mean,sd))
  true.norm.var <- sd^2
  b <- norm.var - true.norm.var
  
  # Comparisons for Chi-squared distibution:
  chisq.mean <- mean(my.rchisq(n,df))
  true.chisq.mean <- df
  c <- chisq.mean - true.chisq.mean
  chisq.var <- var(my.rchisq(n,df))
  true.chisq.var <- 2*df
  d <- chisq.var - true.chisq.var
  
  # Comparisons for Student's t-distribution:
  t.mean <- mean(my.rt(n,df))
  true.t.mean <- 0
  e <- t.mean - true.t.mean
  if (df >=3) {
    t.var <- var(my.rt(n,df))
    true.t.var <- df/(df-2)
    f <- t.var - true.t.var
  } else {
    f <- NA        # Since the variance is infinite if df is less than or equal to 2, set the output to be NA in these cases
  }
  return(list(diff_norm_mean=a,diff_norm_var=b,diff_chisq_mean=c,diff_chisq_var=d,diff_t_mean=e,diff_t_var=f))
}


qqplots.test <- function(n,mean=0,sd=1,df=1) {
  #  Purpose:
  #    Use QQ plots to compare my functions (my.rnorm, my.rchisq, my.rt) with R's corresponding functions (rnorm, rchisq, rt)
  #  Input:
  #    n - how many observations to use in QQ plots
  #    mean - the mean of the normal distribution (default=0)
  #    sd - the standard deviation of the normal distribution (default=1)
  #    df - the number of degrees of freedom the chi squared and t distributions have (default=1)
  #  Output:
  #    Three QQ-plots, one for each distribution
  
  # Set up the output to show four plots on one page.
  # We only require 3 plots, but this ensures plots are roughly 'square' shape and easier to read.
  # Stores original output settings and restores once plots created.
  old.par<-par("mfrow")
  par(mfrow=c(2,2))
  on.exit(par(mfrow=old.par))
  
  # Creates QQ-plots for each distribution:
  qqplot(rnorm(n,mean,sd), my.rnorm(n,mean,sd),xlab="Values generated by rnorm",ylab="Values generated by my.rnorm")
  qqplot(rchisq(n,df),my.rchisq(n,df),xlab="Values generated by rchisq",ylab="Values generated by my.rchisq")
  qqplot(rt(n,df),my.rt(n,df),xlab="Values generated by rt",ylab="Values generated by my.rt")
}


kolsmir.test <- function(n,mean=0,sd=1,df=1) {
  #  Purpose:
  #    Use Kolmogorov-Smirnov tests to compare the values produced by my functions (my.rnorm, my.rchisq, my.rt) with values produced by R's corresponding functions (rnorm, rchisq, rt)
  #  Input:
  #    n - how many random values to create (i.e. sample size)
  #    mean - the mean of the normal distribution (default=0)
  #    sd - the standard deviation of the normal distribution (default=1)
  #    df - the number of degrees of freedom the chi squared and t distributions have (default=1)
  #  Output:
  #    Results of these tests stored in a named list
  #    If a given p-value is under 0.05, we reject the null hypothesis that the two sets of values come from the same probability distribution
  
  ks.norm <- ks.test(my.rnorm(n,mean,df),rnorm(n,mean,df))
  ks.chisq <- ks.test(my.rchisq(n,df),rchisq(n,df))
  ks.t <- ks.test(my.rt(n,df),rt(n,df))
  return(list(KS_normal=ks.norm, KS_chisq=ks.chisq, KS_t=ks.t))
}