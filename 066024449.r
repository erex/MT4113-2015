##MT 4113 Assignment 1
#Name: Yiwen Jiang
#ID: 150020581

#################################
###### Normal Distribution ######
#################################

my.rnorm <- function (n, mean = 0, sd = 1) {
  
  if (is.numeric(n) == TRUE) {    # To make sure n is a numeric argument. 
    
    if (round(n) == n &           # To make sure n is natural number, 
        n > 0 &                   # test if the integer part of n equals to itself and > 0.
        sd >= 0) {                # As well, keep sd >=0.
      
      # This is a function generates m pairs of pseudo-random values from a normal distribution. 
      generator <- function(m, mean = 0, sd = 1) {
        values <- numeric(m*2) 
        for (i in 1:m) {
          
          w <- 2                    # Any number larger than 1 will do.
          while (w > 1) {           # the rejection step
            U <- runif(2)
            U <- U*2 - 1
            w <- U[1]^2 + U[2]^2
          }                         # U is ensured in the unit cycle now.
          v <- sqrt(-2*log(w)/w)
          X <- U*v                  # Now X follows standard normal distribution.
          X <- X*sd + mean          # Now X follows normal distribution with corresponding mean and sd.
          values[i*2 - 1] <- X[1]   # Transfer the variates to results.
          values[i*2] <- X[2]
        }
        return(values) 
      }
      
      if (n%%2 == 0) {            # In the even number case, generate n/2 pairs of variates.
        results <- generator(n/2, mean = mean, sd = sd)
        return(results)
        
      } else {                    # In the odd number case, generate n+1/2 pairs of variates, and delete one variate before output.
        results <- generator((n+1)/2, mean = mean, sd = sd)
        return(results[1:n])
      }
      
    } else {                     
      stop ("invalid arguments")    # error trap on invalid imputs (for n is not natural number)
    }
    
  } else {
    stop ("invalid arguments")    # error trap on invalid imputs (for n is non-numeric)
  }
}

#####################################
###### Chi-square Distribution ######
#####################################

my.rchisq <- function(n, df = 1) {
  
  if (is.numeric(n) == TRUE) {
    if (round(n) == n & n > 0&
        round(df) == df & df > 0) {
      
      rchisq_values <- numeric(n)       # Create a vector of length n to save results.
      for (i in 1:n) {
        Z <- my.rnorm(df)               # Simplily call the previous funciton here.
        rchisq_values[i] <- sum(Z^2)    # generate the results.
      }
      
      return (rchisq_values)
      
    } else {
      stop ("invalid arguments")
    }
    
  } else {
    stop ("invalid arguments")
  }
}

######################################
###### Student's t Distribution ######
######################################

my.rt <- function(n, df = 1){
  
  if (is.numeric(n) == TRUE) {
    if (round(n) == n & n > 0&
        round(df) == df & df > 0) {
      
      rt_values <- numeric(n)
      for (i in 1:n) {
        Z <- my.rnorm(1)
        U <- my.rchisq(1,df)
        rt_values[i] <- Z/sqrt(U/df)
      }
      return (rt_values)
      
    } else {
      stop ("invalid arguments")
    }
    
  } else {
    stop ("invalid arguments")
  }
}

#############################
###### Test Function 1 ######
#############################

# Test whether the resluts of these three functions follow their corresponding distributions BY GRAPH. 
# Default figures are set but feel free to switch them. 

test.1 <- function (n = 5000, mean = 0, sd = 1, df= 3) {  
  
  # Generate samples to make the graphs. 
  # Due to the havey-tail of t-distibution, extreme values always appear in the deviates. 
  # I found it's hard to draw a readable histogram of all values with default R packages. 
  # So i neglect the deviates which are out of [-5,5].
  
  norm_sample <- my.rnorm(n,mean,sd)
  chisq_sample <- my.rchisq(n,df)
  rt_sample <- my.rt(n,df)
  rt_sample <- rt_sample[rt_sample > -5 & rt_sample < 5]
  
  qqnorm(norm_sample)                                        
  
  hist(norm_sample, prob = TRUE,breaks = "FD")                              
  curve(dnorm(x, mean, sd), col = "red", add = TRUE)
  
  hist(chisq_sample, prob = TRUE, breaks = "FD")
  curve(dchisq(x, df), col = "red", add = TRUE)
  
  hist(rt_sample, prob = TRUE, breaks = "FD")
  curve(dt(x,df), col = "red", add = TRUE)
}

# If the Q-Q plot is close to a stright line, 
# and the first histogram fits the normal distribution denstiy curve well,
# we could say the results of my.rnorm is normally distributed. 

# If the second histogram fits the chi-square distribution density curve well, 
# we could say the results of my.nchiq follow chi-squared distribution. 

# If the third histogram fits the student's t distribution density curve well, 
# we could say the results of my.rt follow student's t distribution. 

#############################
###### Test Function 2 ######
#############################

#Test whether the resluts of these three functions follow their corresponding distributions by Kolmogorov-Smirnov Tests.
# Default figures are set but feel free to switch them. 

test.2 <- function (n = 10000, mean = 0, sd = 1, df= 3) {
  
  temp1 <- ks.test(my.rnorm(n,mean,sd), pnorm, mean, sd)
  temp2 <- ks.test(my.rchisq(n,df), pchisq, df)
  temp3 <- ks.test(my.rt(n,df), pt, df)
  
  print(list(result_my.rnorm = temp1, result_my.rchisq = temp2, result_my.rt = temp3))
}

# Check the p-values of these three tests. 
# Large p-value(>0.05 is fair enough) shows there is no evidence to reject the hypothesis that 
# the results of the functions follow their corresponding target distributions, i.e, the function works well.z  

#There is many more methods that work in a similar way to test for normality, such as Shapiro-Wilk tests. 