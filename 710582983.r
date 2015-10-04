#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){
  # Purpose: generates n (user-specified) pseudo-random deviates from the normal distribution with mean and standard deviation specified by the user (default mean 0 and sd 1).
  # Inputs: n - number of deviates; mean - mean of normal distribution (defaults to 0); sd - standard deviation of normal distribution (defaults to 1).
  # Outputs: a vector of n deviates.
  x<-numeric(n)
  for(i in 1:n){
        # Loop to generate U[i] and reject values that do not lie within the unit circle.
        w<-2
        while(w>1){
          u<-numeric(2)
          # Generates U[i] for i=1,2.
          for(k in 1:2){
              u[k]<-2*runif(1)-1
          }
          # Calculates w as the sum of the squares of U[i] for i=1,2.
          usq<-numeric(2)
          for(l in 1:2){
            usq[l]<-u[l]^2
          }
          w<-sum(usq)
        }
        # Calculates normal deviates and puts the deviates into the vector x.
        v<-sqrt(((-2*log(w))/w))
        x[i]<-((u[1]*v)*sd)+mean
  }
      # Outputs vector x.
      return(x)
}

# Testing of my.rnorm
# Generates a histogram and a QQ plot for n (user-specified) deviates from the
# Normal distribution with mean (user-specified, defaults to 0) and standard 
# deviation (user-specified, defaults to 1).
my.rnormtest<-function(n,mean=0,sd=1){
  testnorm<-my.rnorm(n,mean=0,sd=1)
  par(mfrow=c(1,2))
  hist(testnorm)
  qqnorm(testnorm)
}
# In my test of this function, the histogram and QQ plots appeared consistent 
# with the Normal distribution.
my.rnormtest(10000)

my.rchisq<-function(n,df=1){
  # Purpose: generates n (user-specified) pseudo-random deviates from the 
  # chi-squared distribution with df (user-specified, default 1) degrees of
  # freedom.
  # Inputs: n - number of deviates; df - degrees of freedom of chi-squared
  # distribution.
  # Outputs: a vector of n deviates.
  # Creates numerical vector for final deviates to be stored in.
  x<-numeric(n)
  # Creates numerical vector for the squares of normal deviates to be stored in.
  norms<-numeric(df)
  # Generates the squares of df normal deviates n times.
  for(i in 1:n){
    for(m in 1:df){
      norms[m]<-(my.rnorm(1))^2
    }
    # Fills in numerical vector x with the chi-squared deviates.
    x[i]<-sum(norms)
  }
  # Returns the numerical vector x, containing the chi-squared deviates.
  return(x)
}
        
# Testing of my.rchisq
# Generates a histogram of n (user-specified) deviates from the chi-squared 
# distribution with df (defaults to 1) degrees of freedom.
my.chisqtest<-function(n,df=1){
  testchisq<-my.rchisq(n,df=1)
  par(mfrow=c(1,1))
  hist(testchisq)
}
my.chisqtest(1000,10)
# In my test of this function, the histogram appeared consistent 
# with the chi-squared distribution.

my.rt<-function(n,df=1){
  # Purpose: generate n (user-specified) pseudo-random deviates from the 
  # Student's t-distribution with df (user-specified, default 1) degrees of freedom.
  # Inputs: n - number of deviates to be generated; df - degrees of freedom of
  # Student's t-distribution.
  # Outputs: a vector containing the deviates.
  # Creates numeric vectors z, u, and t of lengths n.
  z<-numeric(n)
  u<-numeric(n)
  t<-numeric(n)
  # Produces n deviates from the Student's t-distribution.
  for(i in 1:n){
    # Generates a standard normal deviate.
    z[i]<-my.rnorm(1)
    # Generates a chi-squared deviate.
    u[i]<-my.rchisq(1,df)
    # Uses the algorithm to produce a deviate from the Student's t-distribution 
    # with df degrees of freedom and put the deviate into vector t.
    t[i]<-z[i]/(sqrt(u[i]/df))
  }
  # Returns vector t.
  return(t)
}

# Testing of my.rt
# Generates a histogram with n (user-specified) deviates from the Student's
# t-distribution with df (user-specified; defaults to 1) degrees of freedom.
my.ttest<-function(n,df=1){
  testt<-my.rt(n,df=1)
  par(mfrow=c(1,1))
  hist(testt)
}
my.ttest(10000,15)
# In my test of this function, the histogram appeared consistent 
# with the Student's t-distribution.



