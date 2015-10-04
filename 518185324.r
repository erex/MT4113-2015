#Assignment 1

#I confirm that the attached is my own work, except where clearly indicated in the text.

################## Normal Distribution #################

my.rnorm <- function(n, mean=0, sd=1){
  #Purpose: To generate deviates form the normal distribution.
  #Input: n= numer of deviates, mean= requested mean of the normal distribution,
    # sd= requested standard deviation of the distribution.
  #output: a vector containing n normal deviates.
  
  #making sure that the requested number of deviates is a positive number,
  #that the arguments are (numeric) scalars, and that the sd is non-negative
  if (!n>0 || !sapply(c(n,mean,sd),FUN=is.numeric)|| 
      length(n)!=1 || length(mean)!=1 || length(sd)!=1 ||
      !sd>0)  
    stop("invalid arguments", call.= F)
  
  u <- numeric(2)
  y <- numeric()
  
  # in (j in 1:(n/2+1)): n/2 because the algorithm will create two normal deviates, 
  # +1 because n may be an odd number
  for(j in 1:(n/2+1)){
    
    MB.algorithm <- function(w){
      #purpose: this is Marsaglia and Bray’s method encapsulated in a function.
      #input: the input must be a number >1 for the algorithm to work. I have
        #not added any restrictions on w inside the function since it will not
        #be used outside my.rnorm.
      #output: a vector containing two numbers from the standard normal distribution.
      
      while(w>1){
        for(i in 1:2){
          u[i] <- runif(1)
        }
        u[] <- 2*u[] - 1
        w <- u[1]^2+u[2]^2
      }
      v <- sqrt(-2*log(w)/w)
      x <- u[]*v
      return(x)
    }
    
    #applying the function MB.algorithm to an arbitrary number greater than 1,
    #and storing the output in the vector y.
    x <- MB.algorithm(2)
    y <- c(y,x)
    
    #Values are un-standardised
    q <- y[]*sd+mean
    
    # In case n is an odd number (i.e. length(y)=n+1):
    q <- q[1:n]
  } 
  return(q)
}


################## Chi-Squared Distribution ##################

my.rchisq <- function(n,df=1){
  #Purpose: To generate deviates form the Chi-Squared distributionm using
    #the function my.rnorm.
  #Input: n= numer of deviates, df= degrees of freedom of the distribution.
  #output: a vector containing n chi-squared distributed deviates.
  
  #making sure that the requested number of deviates is a positive number,
  #that df is a positive integer, and that the arguments are (numeric) scalars.
  if (!n>0 || !df>0 || (df %% 1 != 0) ||
      !sapply(c(n,df),FUN=is.numeric)|| 
      length(n)!=1 || length(df)!=1)
    stop("invalid arguments", call.= F)
  
  #creating chi-squared deviates according to definition
  z <- numeric(n)
  for(i in 1:n){
    z[i] <- sum(my.rnorm(df)^2)
  }
  return(z)
}

#################### Student's-T Distribution #####################


my.rt <- function(n,df=1){
  #Purpose: To generate deviates form the T distributionm using
  #the functions my.rnorm and my.rchisq.
  #Input: n= numer of deviates, df= degrees of freedom of the distribution.
  #output: a vector containing n T-distributed deviates.
  
  #same limitations as for my.rchisq(). But df doesn't need to be integer here 
    #according to literature.
  if (!n>0 || !df>0 ||
      !sapply(c(n,df),FUN=is.numeric)|| 
      length(n)!=1 || length(df)!=1)
    stop("invalid arguments", call.= F)
  
  #creating T-deviates according to definition
  t <- numeric(n)
  for(i in 1:n){
    t[i] <- my.rnorm(1)/sqrt(my.chisq(1,df)/df)
  }
  return(t)
}


################ functions for testing ##################

norm.sample <- function(n,mean=0,sd=1,...){
  #Purpose: Investigating whether my.rnorm returns n number of normal deviates.
  #Input: same input as for my.rnorm
  #output: a summary of statistics, along with the expected values. 
    #the shaphiro test (provided n meets range restrictions), as well as
    #various plots for assessing normality.
  
  data <- my.rnorm(n,mean,sd)
  
  #testing length
  if(length(data)==n){
    cat("length is correct\n")
  } else{
    cat("length is", length(data), ", should be", n, "\n")
  }
  
  sample.mean <- mean(data)
  sample.sd <- sd(data)
  
  old.par<-par(no.readonly=T)
  par(mfrow=c(2,2))
  on.exit(par(old.par))
  
  #histogram, density plot, qq-plot and boxplot of the data.
  hist(data,...)
  plot(density(data), main="Density plot",...)
  qqnorm(data)
  boxplot(data, main="Boxplot", plot=T,...)
  
  #finding the p-value of the Shapiro-test, provided 3<=n<=5000.
  #Null hypothesis: data is normally distributed
  if(n<=5000 && n>=3){
    test <- shapiro.test(data)
    test <- test$p.value
  } else{
    print("n needs to be between 3 and 5000 in order to perform Shapiro test")
    test <- NULL
  }
  return(c(sample.mean=sample.mean, 
           expected.mean=mean, 
           sample.sd=sample.sd,
           expected.sd=sd, 
           shapiro.p.value=test
           ))
}


chisq.sample <- function(n, df=1,...){
  #Purpose: Investigating whether my.rchisq returns n chi-squared deviates.
  #Input: same input as for my.rchisq
  #output: a summary of statistics along with the expected values,
  #a density plot of the data, and a density plot of data generated by rchisq 
    #(for comparison)
  
  data <- my.rchisq(n,df)
  
  #testing the length
  if(length(data)==n){
    cat("length is correct\n")
  } else{
    cat("length is", length(data), ", should be", n, "\n")
  }
  
  #finding and comparing mean
  sample.mean <- mean(data)
  expected.mean <- df #according to literature
  
  #finding and comparing sd
  sample.sd <- sd(data)
  expected.sd <- sqrt(2*df) #according to literature
  
  old.par<-par(no.readonly=T)
  par(mfrow=c(1,2))
  on.exit(par(old.par))
  
  #plot of data generated by my.rchisq, compared to expected distribution
  plot(density(data), main="chi-squared: my density plot",...)
  plot(density(rchisq(n,df)), main="chi-squared: R's density plot")
  
  return(c(sample.mean=sample.mean, 
           expected.mean=expected.mean,
           sample.sd=sample.sd,
           expected.sd=expected.sd))
}


t.sample <- function(n, df=1,...){
  #Purpose: Investigating whether my.rt returns n number of T-deviates.
  #Input: same input as for my.rt
  #output: a summary of statistics along with the expected values,
  #a density plot of the data, and a density plot of data generated by rt 
  #(for comparison)
  
  data <- my.rt(n,df)
  
  #testing the length
  if(length(data)==n){
    cat("length is correct\n")
  } else{
    cat("length is", length(data), ", should be", n, "\n")
  }
  
  #finding and comparing the mean
  sample.mean <- mean(data)
  if(df>1){
    expected.mean=0
  } else{
    expected.mean=NA #undefined
  }
  
  #finding and comparing the sd
  sample.sd <- sd(data)
  if(df>2){ #literature
    expected.sd <- sqrt(df/(df-2))
  } 
  if(1<df && df<=2){
    expected.sd <- NaN #infinite
  } 
  if(df<1){
    expected.sd <- NA #undefined
  }
  
  old.par<-par(no.readonly=T)
  par(mfrow=c(1,2))
  on.exit(par(old.par))
  
  #plot of data generated by my.rt, compared to expected distribution
  plot(density(data), main="T: my density plot",...)
  plot(density(rt(n,df)), main="T: R's density plot")
  
  return(c(sample.mean=sample.mean, 
           expected.mean=expected.mean,
           sample.sd=sample.sd,
           expected.sd=expected.sd
           ))
}


