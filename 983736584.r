

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~my.rnorm function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.rnorm<-function(n,mean=0,sd=1){  
  # n as number of values to return
  # mean as mean of values to return - default 0
  # sd as standard deviation of values to return - default 1
  #ensure n is a positive integer, standard deviation if positive
  if (!(n==round(n) & n>0&sd>0)) stop (print("number of values (n) you want to return must be a positive integer, sd is the standard deviation and should be positive")
  )else{
    ran=numeric(n) #create out positions for the value we are going to generate
    for (i in 1:n){
      #function to produce 1 value of pseudo-random value from normal distribution.
      f<-function(){ #function f generates 1 random normal deviate with mean mu and sd sigma
        repeat{                   #repeat loop generate a pair of uniform deviates until criterion w is met
          vone <- 2*runif(1,0,1)-1
          vtwo <-2*runif(1,0,1)-1 
          w<-vone^2 +vtwo^2  
          if (w <=1) break}
        xstd = sqrt((-2*log(w))/w)*vone #first value in the pairs is taken out
        x=xstd*sd+mean
        return(x)
      }#end of function
      
      ran[i]<-f() #value generated put into different position each time until space is filled up
      i=i+1
    }
    return(ran)}
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~my.rchisq function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.rchisq<-function(n,df=1){
  # n as number of values to return
  # df degree of freedom of the distribution- default 1
  #ensure n is a positive integer, degree of freedom is positive integer
  if (!(n==round(n) & n>0 &df==round(df)&df>0)) stop (print("number of values (n) you want to return must be a positive integer,and degree of freedom should be a positive interger")
 ) else{
  ranchisqr<-numeric(n) #create out positions for the value we are going to generate
  for (i in 1:n){
    ranchisqr[i]=sum((my.rnorm(df,0,1))^2)   #generate value from normal distribution
    i<-i+1
  }
  return(ranchisqr)
  }
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~my.rt function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my.rt<-function(n,df=1){ 
  # n as number of values to return
  # df degree of freedom of the distribution- default 1
  #ensure n is a positive integer, df is a positive integer
  if (!(n==round(n) & n>0 &df==round(df)&df>0)) stop (print("number of values (n) you want to return must be a positive integer,and degree of freedom should be a positive interger")
  )else{
  rant<-numeric(n)   #create out positions for the value we are going to generate
  for (i in 1:n){
    rant[i]=my.rnorm(1,0,1)/sqrt(my.rchisq(1,df)/df)
    i<-i+1
  }
  return(rant)
  }
  }





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~test functions for code~~~~~~~~~~~~~~~~~~~~~~~~~~~
#run test function by removing # before the line (for testing arguments of the function)



#~~~~~~~~~~~~~~~~~~~~~~~~~~test for my.rnorm function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1. testing arguments of the function
#my.rnorm(-3) #should return error, number of values generated should be positive
#my.rnorm(2.5) #should return error, number of values generated should be integer
#my.rnorm(2,0,-3) #should return error, standard deviation (sd) should be positive


my.rnormtest=function(n,mean=0,sd=1){
  x <- my.rnorm(n,mean,sd)
  #test function my.rnorm produces a vector of n numbers
  pass.testone <- (length(x)==n & is.numeric(x))
  if (n<2){
    return (pass.testone)
  }
  else{
    # when generate more then three values, test whether the value generated is normally distributed
    ###if plot is bell shape centered around 0 and with a standard deviation of 1 
    #then pass this test (normal distribution)
    plot(density(my.rnorm(n,mean,sd)))
    #compare shape of distributions of values we generated 
    #with the inbuilt r normal distribution (in blue) should be similar
    lines(density(rnorm(n,mean,sd)),col=100)
    
    #comparing generated values to a chi-square distribution,
    #if no significant differences between the two, should return true (i.e.,p value >0.05)
    pass.testnormality<-((shapiro.test(x)$p.value)>0.05) 
    a<-c(pass.testone, pass.testnormality)
    
    return (a)}
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~test for my.rchisq function~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# testing arguments of the function
#my.rchisq(-3) #should return error, number of values generated should be positive
#my.rchisq(2.5) #should return error, number of values generated should be integer
#my.rchisq(2,-1) #should return error, degree of freedom (df)) should be positive
#my.rchisq(2,0.5) #should return error, degree of freedom (df)) should be integer

my.rchisqtest=function(n,df=1){
  y<-my.rchisq(n,df)
  #test function my.rchisq produces a vector of n numbers
  pass.testone <- (length(y)==n & is.numeric(y))
  if (n<2){
    return (pass.testone)
  }else{
    #when generate more then three values,test whether the value generated atisfy chi-square distribution
    #compare shape of distributions of values we generated 
    #with the inbuilt r chisquare distribution (in blue)
    plot(density(my.rchisq(n,df)))
    lines(density(rchisq(n,df)),col=100)  #two lines should be approximately same and close to each other
    
    # comparing generated values to a chi-square distribution, if no significant differences between the two, should return true (i.e.,p value >0.05)
    pass.testchisq<-((ks.test(y,"pchisq",df)$p.value)>0.05) 
    a<-c(pass.testone, pass.testchisq)
    return (a)}
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~test for my.rt function~~~~~~~~~~~~~~~~~~~~~~~~
#testing arguments of the function
#my.rt(-3) #should return error, number of values generated should be positive
#my.rt(2.5) #should return error, number of values generated should be integer
#my.rt(2,-1) #should return error, degree of freedom (df)) should be positive
#my.rt(2,0.5) #should return error, degree of freedom (df)) should be integer


my.rttest=function(n,df=1){
  z<-my.rt(n,df)
  #test function my.rt produces a vector of n numbers
  pass.testone <- (length(z)==n & is.numeric(z))
  
  if (n<2){
    return (pass.testone)
  }else{
    #when number of values generated >=3, test whether the value generated satisfy t distribution
    
    #compare shape of distributions of values we generated with the inbuilt r t-distribution (in blue)
    plot(density(my.rt(n,df)))
    lines(density(rt(n,df)),col=100)  #two lines should be approximately same and close to each other
    
    #test whether the value generated satisfy t distribution by comparing to generated values to a t distribution,if no significant difference btw the two return p values >0.05
    pass.testrt<-((ks.test(z,"pt",df)$p.value)>0.05)
    a<-c(pass.testone, pass.testrt)
    return (a)}
}











