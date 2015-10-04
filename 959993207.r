#---------------------------------------------------------------------------------------
#I confirm that the attached is my own work, except where clearly indicated in the text
#---------------------------------------------------------------------------------------
Marsaglia.method <- function() {
  #Function of "Marsaglia.method" is to generate a pair of standard
  #  normally distributed variates. 
  #
  #No args.
  #Returns:
  #  A pair of standard normally distributed variates.
  repeat {
    #Produce a pair of standard uniform deviates with transformed range [-1,1]
    u1 <- 2*runif(1) - 1   
    u2 <- 2*runif(1) - 1
    w <- u1^2 + u2^2
    if (w < 1 && w != 0) break
    } 
  v <- sqrt(-2*log(w, exp(1))/w)
  variates <- c(u1*v, u2*v)
  return(variates)
}

my.rnorm <- function(n, mean=0, sd=1) {
  # Function of "my.rnorm" is to generate normally distributed random numbers.
  #
  # Args:
  #   n: Numbers of deviates for output; 
  #   mean: The mean of normal distribution with defualt=0.
  #   sd:  The standard deviation of normal distribution with default=1.
  # Returns: 
  #   Random numbers with normal distribution, ~N(mean,sd)
  #
  # Error handling
  if (class(n)!="numeric" || n < 1) {
    stop("invalid arguments\n")
  } 
  quotient <- n%/%2
  rem <- n%%2
  # Error handling
  if (rem!=1 && rem!=0) {
    stop("invalid arguments\n")
  } else {
    variates <- rep(0, n)
    if (quotient > 0)
      for (i in 1:quotient) {
        temp <- Marsaglia.method()*sd + mean
        variates[i*2-1] <- temp[1]
        variates[i*2] <- temp[2]
      }
    if (rem==1) {
      variates[n] <- Marsaglia.method()[1]*sd+mean
    }
    return(variates)
  }
}

#---------------------------------------------------------------------------
my.rchisq <- function(n, df=1) {
  #Computes the chi-square variates
  #
  #Args: 
  #  n: Number of values to return; 
  #  df: Degrees of freedom, default=1.
  #
  #Returns:
  #  Random numbers with the chi-square distribution. ~X2(df)
  #
  #Error handling
  if (class(n)!="numeric" || n < 1 || class(df)!="numeric" || df < 1) {
    stop("invalid arguments\n")
  }
  rem1 <- n%%2
  #Error handling
  if (rem1!=1 && rem1!=0) {
    stop("invalid arguments\n")
  }
  rem2 <- df%%2 
  if (rem2!=1 && rem2!=0) {
    stop("invalid arguments\n")
  } else {
      variate <- rep(0, n)
      for (i in 1:n) {
        variate[i] <- sum(my.rnorm(df)^2)
      }
    return(variate)  
  }
}

#-------------------------------------------------------------------
my.rt <- function(n, k=1) {
  #Generate random numbers with Student's t-distributed 
  #
  #Args:
  #  n: The number of values;
  #  k: Degrees of freedom, default=1.
  #
  #Returns:
  #  Random numbers with a student's t-distribution. ~t(k)
  #
  #Error handling
  if (class(n)!="numeric" || n < 1 || class(k)!="numeric" || k < 1) {
    stop("invalid arguments\n")
  }
  rem1 <- n%%2
  #Error handling
  if (rem1!=1 && rem1!=0) {
    stop("invalid arguments\n")
  }
  rem2 <- k%%2 
  if (rem2!=1 && rem2!=0) {
    stop("invalid arguments\n")
  }
  else {
    t <- rep(0, n)
    for (i in 1:n) {
      z <- my.rnorm(1)
      U <- my.rchisq(1, k)
      t[i] <- z/sqrt(U/k)
      }
    return(t)
  }
}

test <- function() {
  #----------------------1-------------------------
  #Boundary-value analysis
  my.rnorm(1); my.rchisq(1); my.rt(1)
  #-----------------------2--------------------------
  #Error traps on the input
  #  including: 0, negative numbers, float number, non-numeric, charactor
  # Many of the args should be integers greater than 0.
  #   (e.g: n, df, k)
  #There are many possible combinations of error trpas,
  #  only a few are listed below.
  my.rnorm(0)
  my.rchisq(3.3, 2,1)
  my.rt("h", -3)
  #------------------------3----------------------------
  #Test of normality by drawing Q-Q plot
  #All random numbers tend to obey a normal distribution.
  #Note that both t and chi-square distribution tend to follow 
  #  normal distribution when the degrees of freedom is great enough
  #The more values generated, the more clear the trend will be
  par(mfrow = c(1, 3))
  qqnorm(my.rnorm(100))
  qqnorm(my.rt(100,10))
  qqnorm(my.rchisq(100,10))
  #------------------------4----------------------------
  #Test of normality by shapiro-Wilk test()
  #  which produce the p.value for the test.
  shapiro.test(my.rnorm(1000))
  #There are many functions used for normality test
  #Referenced from: 
  # 'https://stat.ethz.ch/R-manual/R-patched/library/stats/html/shapiro.test.html'
  #-------------------------5------------------------------------------------------
  #Branch coverage test
  #This test aimes to test every line of the codes works by considering
  #  every possible conditions.
  #--------------------------6-----------------------------------
  #Test of elapsed time and make a contrast with R-functions
  system.time(for(i in 1:100) mad(my.rnorm(1000)))
  system.time(for(i in 1:100) mad(rnorm(1000)))
  #The result of elapsed time may suggests that the R-function rnorm()
  #  is faster than the M_B method.
  #The same pattern works on my.rchiq() vs rchiq(), and my.rt() vs rt().
}
