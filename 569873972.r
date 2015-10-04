### ASSIGNMENT 1 - my.rnorm, my.rchisq, my.rt

### I confirm that the attached is my own work, except where clearly indicated
### in the text.

my.rnorm <- function(n, mean=0, sd=1) {
#Purpose: produce pseudo-random normally-distributed deviates
#Inputs: n - number of values to return
#        mean - mean of values to return (default=0)
#        sd - standard devation of values to return (default=1)
#Outputs: vector of random normally-distributed values
  if(n<=0 | sd<0 | n%%1 != 0) {
    stop("invalid arguments")    #return warning if input arguments not valid
  }
  norm.values <- vector()
  while(length(norm.values) < n) {
    u <- (2*runif(2)-1)
    w <- sum(u**2)
    if(0 < w && w <= 1) {    # w has to be greater than 0 to overcome log(0) issue
      v = sqrt(((-2)*log(w))/w)
      norm.values <- c(norm.values, u*v)
    }
  }
  return(((norm.values*sd)+mean)[1:n])    #[1:n] allows for return of odd number of values
}

#############################################################

my.rnorm.test <- function() {
#Purpose: generate 9999 random values for different normal 
#         distributions to compare my.rnorm function with rnorm 
#Inputs: N/A
#Outputs: returns histogram and Q-Q plot for my.rnorm(9999,0,1) data and 
#         a matrix with summary outputs for both my.rnrom(9999,21.1,sqrt(3)) 
#         and rnorm(9999,21.1,sqrt(3))
  summary.list <- list()
  summary.list[[1]] <- rnorm(9999,21.1,sqrt(3))
  summary.list[[2]] <- my.rnorm(9999,21.1,sqrt(3))
  my.rnorm.hist <- my.rnorm(9999)
  par(mfrow=c(1,2))
  on.exit(par(mfrow=c(1,1)))    #prevent global changes
  hist(my.rnorm.hist, prob=TRUE,xlim=c(-4,4),main="Hist. of my.rnorm values")
  lines(density(my.rnorm.hist, adjust=2), col="blue", lwd=2)    #add smooth density line to histogram of my.rnorm(9999,0,1) values
  qqnorm(my.rnorm.hist)    #Q-Q plot
  return(sapply(summary.list, summary))
}

#############################################################

my.rchisq <- function(n, df=1) {
#Purpose: produce pseudo-random chi-squared distributed deviates
#Inputs: n - number of values to return
#        df - degrees of freedom (default=1)
#Outputs: vector of random chi-squared distributed values
  if(n<=0 | df<1 | n%%1 != 0 | df%%1 != 0) {
    stop("invalid arguments")
  }
  chisq.values <- vector()
  while(length(chisq.values) < n) {
    z2 <- my.rnorm(df)**2
    chisq.values <- c(chisq.values, sum(z2))
  }
  return(chisq.values)
}

##############################################################

my.rchisq.test <- function() {
#Purpose: generate 9999 random values for different chi-squared 
#         distributions to compare my.rchisq function with rchisq  
#Inputs: N/A
#Outputs: returns plot of chi-squared distributions(data from my.rchisq function) 
#         with different degrees of freedom.
#         also returns matrix with summary outputs for both my.rchisq(9999,10) 
#         and rchisq(9999,10)
#N.B. takes approx. 10 secs to run!
  summary.list <- list()
  all.chi.values <- list()
  summary.list[[1]] <- rchisq(9999,10)
  summary.list[[2]] <- my.rchisq(9999,10)
  df <- c(1,2,3,5,10)    #degrees of freedom
  colour <- rainbow(5)    #allows a different colour for each density line
  for(i in 1:5) {
    all.chi.values[[i]] <- my.rchisq(9999,df[i])    #generates values for 5 chi-squared distributions, my.rchisq(9999, df=1,2,3,5,10)
  }
  plot(density(all.chi.values[[1]], adjust=2), main="my.rchisq distributions",
       col = colour[1], lwd=2)
  for(j in 2:5) {
    lines(density(all.chi.values[[j]], adjust=2), col = colour[j], lwd=2)    #plot smooth density line for each chi-squared distribution
  }
  legend('topright', c("df=1","df=2","df=3","df=5","df=10"),lwd=2, 
         col=c(colour[1],colour[2],colour[3],colour[4],colour[5]))    #add legend to plot
  return(sapply(summary.list, summary))
}

##############################################################

my.rt <- function(n, df=1) {
#Purpose: produce pseudo-random t-distributed deviates
#Inputs: n - number of values to return
#        df - degrees of freedom (default=1)
#Outputs: vector of random t-distributed values
  if(n<=0 | df<1 | n%%1 != 0 | df%%1 != 0) {
    stop("invalid arguments")
  }
  t.values <- vector()
  while(length(t.values) < n) {
    denom <- sqrt(my.rchisq(n,df)/df)   #denominator of fraction
    t.values <- c(t.values, my.rnorm(n)/denom)
  }
  return(t.values)
}

#############################################################

my.rt.test <- function() {
#Purpose: generate 999 random values for different t-distributions 
#         to compare my.rt function with rt  
#Inputs: N/A
#Outputs: returns plot of t-distributions(data from my.rt function) with 
#         different degrees of freedom.
#         also returns matrix with summary outputs for both my.rt(999,10) 
#         and rt(999,10)
  summary.list <- list()
  all.t.values <- list()
  summary.list[[1]] <- rt(999,10)
  summary.list[[2]] <- my.rt(999,10)
  df <- c(2,3,5,10)    #degrees of freedom
  colour <- rainbow(4)    #allows a different colour for each density line
  for(i in 1:4) {
    all.t.values[[i]] <- my.rt(999,df[i])    #generates values for 4 t-distributions, my.rt(999, df=2,3,5,10)
  }
  plot(density(all.t.values[[1]], adjust=2), xlim=c(-4,4), ylim=c(0,0.40),
       main="my.rt distributions",col = colour[1], lwd=2)
  for(j in 2:4) {
    lines(density(all.t.values[[j]], adjust=2), col = colour[j], lwd=2)    #plot density line for each t-distribution
  }
  legend('topright', c("df=2","df=3","df=5","df=10"),lwd=2, 
         col=c(colour[1],colour[2],colour[3],colour[4]))    #add legend to plot
  return(sapply(summary.list, summary))
}
