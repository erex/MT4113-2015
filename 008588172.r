#MT4113: Computing in Statistics, Assignment 1
#I confirm that the attached is my own work, except where clearly indicated in the text.

generateU<-function() {
  #Generates a pair of random deviates from the uniform distribution
  #
  #Arguments: no arguments
  #Returns: a vector U with two random deviates from the uniform distribution
  U<-c(runif(2, 0, 1))
  #Transform to unit square
  U<-2*U-1 
  return(U)
}

my.rnorm<-function(n, mean=0, sd=1) {
  #Generates pseudo-random values from a normal distribution using the
  #Marsaglia and Bray algorithm.
  #
  #Arguments:
  #   n: number of values to return, no default
  #   mean: mean of values to return, default 0
  #   sd: standard deviation of values to return, default 1
  #Returns:
  #   vector of n pseudo-random values from a normal distribution
  #
  #Error handling:
  #   checks if arguments are scalar, and n is positive
  if(length(n)!=1|length(mean)!=1|length(sd)!=1|n<0) {
    stop('invalid arguments')
  }
  if(n==0){
    return(numeric())
  }
  #create an empty vector with length n
  X.values<-vector(length=n)
  length1<-length(X.values)
  #checks if n is odd, adds i to n if it is odd
  if(n%%2==1){
    n<-n+1
  }
  for(i in 1:(n/2)) {
    #use the function generateU to generate a pair of random values
    U<-generateU()
    w<-U[1]^2+U[2]^2  
    while(w>1){
      U<-generateU()
      w<-U[1]^2+U[2]^2
    }
    v<-sqrt((-2*log(w))/w)
    #define pair of normal distributed variates
    X<-sd*(U*v)+mean
    #adds first deviate to ith position in vector
    X.values[i]<-X[1] 
    #adds second deviate to (n-i+1)th position in vector
    X.values[n-i+1]<-X[2] 
  } 
  length2<-length(X.values)
  #check if original length of X.values is equal to final length of X.values
  if(length1==length2) { 
    #return all values if lengths are equal i.e. there is an even number of values
    return(X.values) 
  } else {
    #return all values bar one deviate if lengths are not equal 
    #this returns an odd number of deviates
    return(X.values[1:length1]) 
  }
}

my.rchisq<-function(n, df=1){
  #Generates pseudo-random values from a chi-squared distribution using
  #normal deviates generated by the function my.rnorm
  #
  #Arguments:
  #   n: number of values to return, no default
  #   df: degrees of freedom of the distribution, defualt 1
  #Returns:
  #   vector of n pseudo-random chi-squared distributed values
  #
  #Error handling:
  #   check if arguments are scalar
  #   check if n and  df are positive values
  if(length(n)!=1|length(df)!=1|n<0|df<0) {
    stop('invalid arguments')
  }
  if(n==0){
    return(numeric())
  }
  chisq.values<-vector(length=n)
  for(i in 1:n){
    #generate random deviates from df independent standard normal variables
    z<-my.rnorm(df) 
    chisq<-sum(z^2) 
    chisq.values[i]<-chisq
  }
  return(chisq.values)
}

my.rt<-function(n, df=1){
  #Generates pseudo-random values from a Student's t-distribution using
  #values generated from the my.rnorm, and my.rhisq
  #
  #Arguments:
  #   n: number of values to return, no default
  #   df: degrees of freedom of the distribution, default 1
  #Returns:
  #   vector of n pseudo-random t-distributed values
  #
  #Error handling:
  #   check if arguments are scalar
  #   check if n and df are positive values
  if(length(n)!=1|length(df)!=1|n<0|df<0) {
    stop('invalid arguments')
  }
  if(n==0){
    return(numeric())
  }
  t.values<-vector(length=n)
  for(i in 1:n){
    #generate a random deviate from a standard normal distribution
    z<-my.rnorm(1)
    #generate a random deviate from a chi-squared distribution
    #with df degrees of freedom
    U<-my.rchisq(1,df)
    t<-z/sqrt(U/df)
    t.values[i]<-t
  }
  return(t.values)
}


#Tests
#Check that the functions produce the right number of deviates asked
ten.deviates<-function(){
  x<-my.rnorm(10)
  y<-my.rchisq(10)
  z<-my.rt(10)
  testx<-(length(x)==10 & is.numeric(x))
  testy<-(length(y)==10 & is.numeric(y))
  testz<-(length(z)==10 & is.numeric(z))
  #returns TRUE if the function has produced 10 numerical values
  cat('my.rnorm:',testx,'my.rchisq:',testy,'my.rt:',testz)
}

#Checks that the functions produce an odd number of deviates when asked
seven.deviates<-function(){
  x<-my.rnorm(7)
  y<-my.rchisq(7)
  z<-my.rt(7)
  testx<-(length(x)==7 & is.numeric(x))
  testy<-(length(y)==7 & is.numeric(y))
  testz<-(length(z)==7 & is.numeric(z))
  #returns TRUE if the function has produced 7 numerical values
  cat('my.rnorm:',testx,'my.rchisq:',testy,'my.rt:',testz)
}

#Produces graphs to give a visual representation of the distribution.
#This function was taken from lecture notes. 
# (url: http://www.creem.st-and.ac.uk/ericr/MT4113/lectures/MT4113L4art.pdf)
eda<-function(x){
  par(mfrow=c(1,3))
  hist(x, probability=TRUE)
  lines(density(x))
  boxplot(x,horizontal=TRUE)
  rug(x)
  qqnorm(x)
  summary(x) }
#The plots produced can be compared to how the plots should look like theoretically.
#norm<-eda(my.rnorm(200))
#chisq<-eda(my.rchisq(100))



