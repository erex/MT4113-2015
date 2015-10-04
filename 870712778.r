#Matriculation Number 150014906
#I confirm that the attached is my own work, except where clearly indicated in the text.

## NORMAL DISTRIBUTION ##

# PURPOSE:
#   generate a user-specifed number of normally distributed values
#
# INPUTS:
#   n: number of values to return from a normal distribution
#   mean: mean value for the returned normal distribution
#   sd: standard deviation for the returned normal distribution
#
# OUTPUTS:
#   vector of n values from a normal distribution with specified mean and standard deviation

my.rnorm <- function(n,mean=0,sd=1) {
  #traps for missing, bad, or nonscalar inputs
  if (missing(n) ||
      (n%%1!=0) ||
      (n < 0) ||
      (sd < 0) ||
      (length(n) != 1) ||
      (length(mean) != 1) ||
      (length(sd) != 1)) {
    stop("invalid arguments")
  }
  
  nn <- n
  #if n is odd, increase the number of deviate pairs to create by 1
  if (nn%%2!=0) nn<-n+1 
  
  #now start to generate pair of uniform distributions of size n
  #and then shift them from range[0,1] to [-1,1]
  nu <- matrix(data=rep(1,times=nn),ncol=2)
  us <- apply(nu,c(1,2),function(x) 2*runif(x) - 1)

  #Replace values outside of the unit circle
  #select indices in w to replace, and loop over only those indices
  w <- us[,1]^2 + us[,2]^2
  wi <- which(w > 1)
  
  for (elem in wi){
    #generate new U values until they fall inside the unit circle
    repeat{
      u1 <- 2*runif(1) - 1
      u2 <- 2*runif(1) - 1
      if (u1^2 + u2^2 <= 1) break
    }
    #replace the old U and w values with the new
    us[elem,1]<-u1
    us[elem,2]<-u2
    w[elem] <- u1^2 + u2^2
  }
  
  v <- sqrt(-2*log(w)/w)
  X <- us*v
  #now transform to the specified standard deviation and mean
  X <- X*sd + mean
  #check that output has correct user-specified number of deviates
  out <- as.vector(X)[1:n]
  return(out)
}


## CHI-SQUARED DISTRIBUTION ##

# PURPOSE:
#   generate chi-squared distributed deviates
#
# INPUTS:
#   n: number of values to return from a chi-squared distribution
#   df: standard deviation for the returned normal distribution
#
# OUTPUTS:
#   vector of n chi-squared-distributed values with standard deviation df

my.rchisq <- function(n,df=1) {
  #traps for missing, bad, or nonscalar inputs
  if (missing(n) ||
      (n%%1!=0) ||
      (df%%1!=0) ||
      (length(n) != 1) ||
      (length(df) != 1)) {
    stop("invalid arguments")
  }
  
  ##chi-squared uses standard normal (default, mean 0, sd 1)
  #create Z matrix of n rows and df columns
  zs <- matrix((my.rnorm(df*n))^2,ncol=df)
  #sum rows of Zs into chis
  chis <- apply(zs,1,sum)
  return(chis)
}


## STUDENT'S T DISTRIBUTION ##

# PURPOSE:
#   generate student's t-distributed deviates
#
# INPUTS:
#   n: number of values to return from a chi-squared distribution
#   df: standard deviation for the returned normal distribution
#
# OUTPUTS:
#   vector of n chi-squared-distributed values with standard deviation df

my.rt <- function(n,df=1){
  #traps for bad inputs will be caught by my.rnorm or my.rchisq
  #apply transformation from normal and chisq distributions to get t distr
  t <- my.rnorm(n)/sqrt(my.rchisq(n,df)/df)
  
  return(t)
}


#### TESTS ####
# Tests1-4 take my custom distribution functions as input

## Test 1: No arguments given to function. Should stop with error 'invalid arguments'
noargs <- function(FUN) {
#  print(FUN)
  FUN()
}

## Test 2: Optional arguments only. Should stop with error 'invalid arguments'
optargs <- function(FUN) {
  FUN(m=1)
  FUN(df=1)
}

## Test3: Bad argument format. Should stop with error 'invalid arguments'
badargs <- function(FUN) {
  FUN(c(1,2))
  #FUN(1,m=c(1,2))
  #FUN(11.1)
}

## Test4: Check that return vector has correct number of values (n), test even & odd
retcount <- function(FUN) {
  #some n values to test
  ns <- c(1,2,7,10,15,31,100,2000)
  for (n in ns) {
    out <- FUN(n)
    if (length(out==n)) {
      result<-'PASS'
    } else {
      result<-'FAIL'
    }
    cat('n=',n,result,'\n')
  }
}

## Test5: Plot my.* and R's corresponding function with the same arguments.  
#Try this with lots of different n,m,s. Plots should mostly overlap.
#Result is determined by inspection.  If plots are not consistent, something is wrong.

rnormcomp <- function(n,m,s) {
  rn <- rnorm(n,mean=m,sd=s)
  mrn <- my.rnorm(n,mean=m,sd=s)
  xmin <- m-s*5
  xmax <- m+s*5
  bins <- seq(xmin,xmax,by=1)
  hist(rn,breaks=bins,col="red",density=10,angle=135, xlim=c(xmin,xmax))
  hist(mrn,breaks=bins,col="blue",density=10,angle=45,xlim=c(xmin,xmax),add=T)
}

rchisqcomp <- function(n,df){
  rc <- rchisq(n,df)
  mrc <- my.rchisq(n,df)
  last <- ceiling(max(rc,mrc))
  bins <- seq(0,last,by=0.5)
  hist(rc,breaks=bins,col="red",density=10,angle=135, xlim=c(0,last))
  hist(mrc,breaks=bins,col="blue",density=10,angle=45, xlim=c(0,last),add=T)
}

rtcomp <- function(n,df){
  rt <- rt(n,df)
  mrt <- my.rt(n,df)
  xmin <- floor(min(rt,mrt))
  xmax <- ceiling(max(rt,mrt))
  bins <- seq(xmin,xmax,by=1)
  hist(rt,breaks=bins,col="red",density=10,angle=135, xlim=c(xmin,xmax))
  hist(mrt,breaks=bins,col="blue",density=10,angle=45, xlim=c(xmin,xmax),add=T)
}