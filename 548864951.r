my.rnorm <- function(n,mean=0,sd=1) {
  #Purpose: generates deviates from a user-specified normal distribution.
  #Inputs: n - float, number of deviates required.
  #        mean - float, mean of distribution.
  #        sd - positive float, standard deviation of distribution.
  #Outputs: a numeric vector of deviates.
  if (!(is.double(mean) & is.double(sd))) stop("invalid arguments") #Checks inputs are of correct form.
  if (!(n >= 1 & sd >= 0)) stop("invalid arguments")
  if (!(length(n) == 1 & length(mean) == 1 & length(sd) == 1)) stop("invalid arguments")
  if (!(n - floor(n) == 0)) stop("invalid arguments")
  out <- rep(0,n) #Generates empty vector to populate with deviates.
  for (i in 1:n) {
    w <- 1.5
    v <- 0
    nor <- rep(0,2) #Generates empty vector for each pair of deviates.
    while (w > 1) {
      u <- c(runif(1),runif(1)) #Obtains uniform variates.
      u <- 2*u-1 #Transforms to unit square.
      w <- (u[1])^2 + (u[2])^2 #Generates test value for rejection step.
    }
    v <- sqrt(((-2)*log(w))/w) #Performs final transformation to obtain standard normal deviates.
    nor[1] <- u[1]*v #Places pair of standard normal deviates in relevant vector.
    nor[2] <- u[2]*v
    nor <- sd*nor + mean #Transforms standard normal deviates to the required distribution.
    out[i] <- nor[1] #Places one of each pair of deviates in the output vector.
  }
  return(out)
}



my.rchisq <- function(n,df=1) {
  #Purpose: generates deviates from a user-specified chi-squared distribution.
  #Inputs: n - float, number of deviates required.
  #        df - float, degrees of freedom of distribution.
  #Outputs: a numeric vector of deviates.
  if (!(is.double(n) & is.double(df))) stop("invalid arguments") #Checks inputs are of correct form.
  if (!(n >= 1 & df >= 1)) stop("invalid arguments")
  if (!(length(n) == 1 & length(df) == 1)) stop("invalid arguments")
  if (!(n - floor(n) == 0)) stop("invalid arguments")
  out <- rep(0,n) #Generates empty vector to populate with deviates.
  norm <- rep(0,df) #Generates empty vector to repeatedly populate with squared Z variates.
  for (i in 1:n) {
    for (j in 1:df) {
      norm[j] <- (my.rnorm(1))^2 #Populates a vector with deviates from squared Z distributions.
    }
    out[i] <- sum(norm) #Places the sum of the squares in the output vector.
  }
  return(out)
}



my.rt <- function(n,df=1) {
  #Purpose: generates deviates from a user-specified Student's t-distribution.
  #Inputs: n - float, number of deviates required.
  #        df - float, degrees of freedom of distribution.
  #Outputs: a numeric vector of deviates.
  if (!(is.double(n) & is.double(df))) stop("invalid arguments") #Checks inputs are of correct form.
  if (!(n >= 1 & df >= 1)) stop("invalid arguments")
  if (!(length(n) == 1 & length(df) == 1)) stop("invalid arguments")
  if (!(n - floor(n) == 0)) stop("invalid arguments")
  out <- rep(0,n) #Generates empty vector to populate with deviates.
  tea <- 0
  for (i in 1:n) {
    tea <- my.rnorm(1)/sqrt((my.rchisq(n=1,df))/df) #Calculates deviate from t-distribution.
    out[i] <- tea #Places deviate in output vector.
  }
  return(out)
}



test1.rnorm <- function(n,mean=0,sd=1) {
  #Purpose: tests equality of the means of my.rnorm and rnorm with 2-sample t-test.
  #Inputs: n - float, number of deviates required.
  #        mean - float, mean of distribution.
  #        sd - positive float, standard deviation of distribution.
  #Outputs: a summary of the t-test results.
  x <- my.rnorm(n,mean,sd) #Generates a vector of deviates with my.rnorm.
  y <- rnorm(n,mean,sd) #Generates a vector of deviates with rnorm.
  test1 <- t.test(x,y,mu=0) #Performs the 2-sample t-test.
  original.par <- par("mfrow") #Saves global graphics parameters.
  par(mfrow=c(1,2)) #Sets new parameters.
  hist(x) #Generates histograms.
  hist(y)
  on.exit(par(original.par)) #Restores original parameters
  return(test1) #Returns the results.
}



test2.rnorm <- function(n,mean=0,sd=1) {
  #Purpose: tests equality of variance of my.rnorm and rnorm with an F-test.
  #Inputs: n - float, number of deviates required.
  #        mean - float, mean of distribution.
  #        sd - positive float, standard deviation of distribution.
  #Outputs: a summary of the F-test results.
  x <- my.rnorm(n,mean,sd) #Generates a vector of deviates with my.rnorm.
  y <- rnorm(n,mean,sd) #Generates a vector of deviates with rnorm.
  test2 <- var.test(x,y,ratio=1) #Performs the F-test.
  original.par <- par("mfrow") #Saves global graphics parameters.
  par(mfrow=c(1,2)) #Sets new parameters.
  hist(x) #Generates histograms.
  hist(y)
  on.exit(par(original.par)) #Restores original parameters.
  return(test2) #Returns the results.
}



test3.chisq <- function(n,df=1) {
  #Purpose: tests equality of location of my.rchisq and rchisq with a 2-sample permutation test.
  #Inputs: n - float, number of deviates required.
  #        df - float, degrees of freedom of distribution.
  #Outputs: a summary of the permutation test results.
  x <- my.rchisq(n,df) #Generates a vector of deviates with my.rchisq.
  y <- rchisq(n,df) #Generates a vector of deviates with rchisq.
  test3 <- wilcox.test(x,y) #Performs the permutation test.
  original.par <- par("mfrow") #Saves global graphics parameters.
  par(mfrow=c(1,2)) #Sets new parameters.
  hist(x) #Generates histograms.
  hist(y)
  on.exit(par(original.par)) #Restores original parameters.
  return(test3) #Returns the results.
}



test4.rt <- function(n,df=1) {
  #Purpose: tests equality of location of my.rt and rt with a 2-sample permutation test.
  #Inputs: n - float, number of deviates required.
  #        df - float, degrees of freedom of distribution.
  #Outputs: a summary of the permutation test results.
  x <- my.rt(n,df) #Generates a vector of deviates with my.rt.
  y <- rt(n,df) #Generates a vector of deviates with rt.
  test4 <- wilcox.test(x,y) #Performs the permutation test.
  original.par <- par("mfrow") #Saves global graphics parameters.
  par(mfrow=c(1,2)) #Sets new parameters.
  hist(x) #Generates histograms.
  hist(y)
  on.exit(par(original.par)) #Restores original parameters.
  return(test4) #Returns the results.
}