my.rnorm<-function(n,mean=0,sd=1){
  #Generates random normal variables
  if(n<=0) {
    #If someone types in an invalid value for deviates then 
    #returns an empty vector
    res<-c(length=NULL)
    cat('invalid arguments')
  }
  else if(sd<0){
    #similar for above if statement with sigma
    res<-c(length=NULL)
    cat('invalid arguments')
  }
  else {
    iteration=0
    dev=trunc(n)
    if (dev=)
    # if someone types in a decimal number for deviates, then
    # the value is rounded down 
    res<-c(0)
    repeat{
      #loop generates the required amount of deviates
      repeat{
        #loops until w is of a valid value for the marsaglia
        # and bray algorithm
        u<-runif(2,0,1)
        u<- 2*u-1
        w=u[1]^2 + u[2]^2
        if (w<1) break
      }
      v<-sqrt(-2*log(w)/w)
      res[2*iteration+1]<-u[1]*v
      res[2*iteration+2]<-u[2]*v
      if (length(res)>=dev) break
      iteration=iteration+1
    }
    if (length(res)> dev) {
      #when an odd number of deviates is required, it cuts off
      # last value in the vector so that there isn't too many 
      #deviates
      res<-res[-length(res)]
    }
    res<-sd*res + mean
  }
  return(res)
}


my.rchisq<-function(n,df=1){
  #Generates random deviates from a chi squared distribution
  if(n<=0) {
    #If someone types in an invalid value for deviates then 
    #returns an empty vector and error message
    res<-c(length=NULL)   
    cat('invalid arguments')
  }
  else if (df<0){
    #similar for above
    res<-c(length=NULL)
    cat('invalid arguments')
  }
  else{
    dev<-floor(n)
    iteration=0
    res<-c(0)
    repeat
    {
      nor<-my.rnorm(df,0,1)
      nor.squared<-nor^2
      res[iteration+1]<-sum(nor.squared)
      if (iteration >= dev-1) break
      iteration=iteration+1
    }
  }
  return(res)
}

my.rt<-function(n, df=1){
  if(n<=0) {
    #If someone types in an invalid value for deviates then 
    #returns an empty vector and error message
    res<-c(length=NULL)
    cat('invalid arguments')
  }
  #similar for above
  else if (df<=0){
    res<-c(length=NULL)
    cat('invalid arguments')
  }
  else{
    nfree<-floor(df)
    n.dev<-my.rnorm(n,0,1)
    chi.dev<-my.rchisq(n,nfree)
    res<- n.dev/sqrt(chi.dev/nfree)
  }
  return(res)
}



# Testing functions

 
my.norm.test<-function(n,mean,var){
  #function is designed to put my.rnorm through a series of 
  #common statistical tests. It should return the generated data
  # back and a message showing which tests were passed
  dat<-my.rnorm(n,mean,var)
  mean.ts<-(mean(dat)-mean)/(var/sqrt(n))
  var.ts<- (n-1)*(var(dat)/)
  if (length(dat)==n){
    # Tests whether the number of deviates generated is
    # exactly as expected from input
    cat('number of deviates returned is correct. ')
  }
  else{
    cat('Number of deviates returned is incorrect. ')
  }
  if (abs(mean.ts) >qnorm(0.95)) {
    #runs a 90% significance test on the mean assuming
    #variance is known
    cat(' mean is different than expected. ')
  }
  else{
    cat(' mean is similar to expected. ')
  }
  if(var.ts>qchisq(0.95,n-1)|var.ts<qchisq(0.05,n-1)){
    #does a 90% chi-squared test on the variance
    cat(' Variance is significantly different than expected value')
  }
  else {
    cat('Variance is similar to expected value')
  }
  # returns generated data for user analysis
return(dat)
}

my.chisq.test<-function(n,df){
  #performs the same series as tests as my.norm.test, with
  #parameters being changed such that there expected 
  #moments have been replaced. normal approximation is 
  # being used
  dat<-my.rchisq(n,df)
  mean.ts<-(mean(dat)-df)/(2*df/sqrt(n))
  var.ts<- (n-1)*(var(dat)/(2*df))^2
  if (length(dat)==n){
    # Tests whether the number of deviates generated is
    # exactly as expected from input
    cat('number of deviates returned is correct. ')
  }
  else{
    cat('Number of deviates returned is incorrect. ')
  }
  if (abs(mean.ts) >qnorm(0.95)) {
    #runs a 90% significance test on the mean assuming
    #variance is known
    cat(' mean is different than expected. ')
  }
  else{
    cat(' mean is similar to expected. ')
  }
  if(var.ts>qchisq(0.95,n-1)|var.ts<qchisq(0.05,n-1)){
    #does a 90% chi-squared test on the variance
    cat(' Variance is significantly different than expected value')
  }
  else {
    cat('Variance is similar to expected value')
  }
  # returns generated data for user analysis
  return(dat)
}

my.t.test<-function(n,df){
  #performs the same series as tests as my.norm.test, with
  #parameters being changed such that there expected 
  #moments have been replaced. normal approximation is 
  # being used
  dat<-my.rt(n,df)
  v<-(df/(df-2))
  mean.ts<-(mean(dat))/((v)/sqrt(n))
  var.ts<- (n-1)*(var(dat)/(v))^2
  if (length(dat)==n){
    # Tests whether the number of deviates generated is
    # exactly as expected from input
    cat('number of deviates returned is correct. ')
  }
  else{
    cat('Number of deviates returned is incorrect. ')
  }
  if (abs(mean.ts) >qnorm(0.95)) {
    #runs a 90% significance test on the mean assuming
    #variance is known
    cat(' mean is different than expected. ')
  }
  else{
    cat(' mean is similar to expected. ')
  }
  if(var.ts>qchisq(0.95,n-1)|var.ts<qchisq(0.05,n-1)){
    #does a 90% chi-squared test on the variance
    cat(' Variance is significantly different than expected value')
  }
  else {
    cat('Variance is similar to expected value')
  }
  # returns generated data for user analysis
  return(dat)
}
