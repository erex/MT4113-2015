#I confirm that the attached is my own work, except where clearly indicated in the text.

#--------------------------------------------------------------------
my.rnorm<-function(n,my.mean=0,my.sd=1) {
  #   Purpose:
  #     to generate a vector containing pseudo-random values from a standard normal distribution
  #   Input:
  #     n - the number of values required
  #     my.mean - user generated mean (default=0)
  #     my.sd - user generated standard deviation (default=1)
  #   Output: 
  #     Vector containing n of standard normal deviates
  #--------------
  #   Function variables
  #     i <- interations in the while loop
  #   out <- contains 2 random uniform numbers
  #    n1 <-  
  #    n2 <- transforms u1 to unit square
  #    n3 <- calculates the rejection step - are u inside the unit circle (the'w')
  #           this is reused in the transformation step once values are passed
  #    n4 <- the MB transformation calculation for both random vales
  #     s <- will randomly take a sample from the two values calculated
  #    n5 <- takes the transformed values and adjusts for user generated mean and sd (is the out value)
  #---------------
  if(n<0) stop ('invalid arguments')
  if(my.sd<=0) stop ('invalid arguments')
  out<-list()
  i<-1
  while(i<=n) {
      #generate 2 random uniform numbers and transform to unit square
    n1<-runif(2,min=0,max=1)
    n2<-2*n1-1        
      #rejection step value - reused in the transformation calculation
    n3<-sum(n2^2)
      #set rejection step if else statement
    if(n3<=1) {
        #if passed, calculate transformation
      n4<-sqrt(-2*log(n3)/n3)
      n5<-n2*n4
        #randomly pick one variable and transform with using defined user generated mean and sd
      s<-sample(n5, size=1)
      out[i]<-my.sd*s+my.mean
      i<-i+1
    }                                                #if end
  }                                                  #while end
  return(unlist(out))
}                                                    #function end

#---------------------------------------------------------------------------
my.rchisq<-function(n,my.df=1) {
  #Purpose:
  # to generate a vector of psuedo-random chi-squared distributed deviates
  #Input:
  # n - the number of values required
  # my.df - user generated degrees of freedon (default=1)
  #Output:
  # Vector containing numeric data of length (n)
  #
  # Function variables
  #     i  <- interations used to sepcify postion in the output list
  #     n5 <- standard normal random deviates from function my.rnorm
  #           n5 refers to the value calculated in function my.rnorm - redefined in this function
  #     r1 <- chi square deviates
  #           this is the value in out.r1 - redefined in this function
  #     out.t1 <- the t distributed deviate 
#-----------
  if(n<0) stop ('invalid arguments')
  if(my.df<=0) stop ('invalid arguments')
  out.r1<-list()
  i<-1
  for (n in 1:n) {
    #generate 'my.df' number of standard normal random variables
    n5<-my.rnorm(my.df,0,1)
    out.r1[i]<-sum(n5^2)
    i<-i+1
  }                                    #for end
  return(unlist(out.r1))
}                                      #function end

#------------------------------------------------------------
my.rt<-function(n,my.df=1) {
  #Purpose:
  # to generate a vector of psuedo-random t distributed deviates
  #Input:
  # n - the number of values required
  # my.df - user generated degrees of freedon (default=1)
  #Output:
  # Vector containing numeric data of length (n)
  #
  # Function variables
  #     i  <- interations in the while loop
  #     n5 <- standard normal random deviates from function my.rnorm
  #           n5 refers to the value calculated in function my.rnorm - redefined in this function
  #     r1 <- chi squared deviate independant to n5
  #           r1 referes to the value calucated in function my.chisq (out.r1)
  # out.t1 <- the t deviate 
  #-------------
  if(n<0) stop ('invalid arguments')
  if(my.df<=0) stop ('invalid arguments')
  out.t1<-list()
  i<-1                                 #iteration number - used to list the values successively
  for (n in 1:n) {
    #generate 'my.df' number of standard normal random variables
    n5<-my.rnorm(1,0,1)
    r1<-my.rchisq(1,my.df)
    out.t1[i]<-n5/sqrt(r1/my.df)
    i<-i+1
  }                                    #for end
  return(unlist(out.t1))  
}                                      #function end
#-------------------------------------------------------
#-------------TESTS------------------------
stats.test<-function(x) {
  #   Purpose:
  #     give a summary of a vector of data values
  #     adapted from MT4113 Lecture 4 notes, 2015
  #     (in turn adapted from pg 392 - Using R for introductory statistics)
  #   Input:
  #     x - a vector of numerical data
  #   Output: 
  #     histogram of variables from x
  #     density line plotted on the histogram
  #     qq plot
  #     qq line plotted on qq plot
  
  hist(x, probability=TRUE)
  lines(density(x))
  qqnorm(x)
  qqline(x)
  summary(x)
}                #end function
#----------------------------------------------
test.rnorm<-function(x) {
  #   Purpose:
  #     to run a series of tests on my.rnorm
  #   Input:
  #     x - a vector of numerical data - this will be equivalent to the number of deviates required
  #   Output: 
  #     plots from stats.test - my.rnorm compared to rnorm
  #     error messages from invalid arguments
  #---------------
  t1<-my.rnorm(x,50,10)
  is.numeric(t1)
  length(t1)
  #----plots
  stats.test(my.rnorm(1000,50,3.6))
  stats.test(rnorm(1000,50,3.6))
  #----errors
  my.rnorm(-1,50,3.6)          #comment out in turn as the program should terminate
  my.rnorm(2,0,-5)             #comment out in turn as the program should terminate
}                        #end function
#-----------------------------------------------
test.rchisq<-function(x) {
  #   Purpose:
  #     to run a series of tests on my.chisq
  #   Input:
  #     x - a vector of numerical data - this will be equivalent to the number of deviates required
  #   Output: 
  #     plots from stats.test - my.rnorm compared to rnorm
  #     error messages from invalid arguments
  #---------------
  t1<-my.rchisq(x,2)
  is.numeric(t1)
  length(t1)
  #----plots
  stats.test(my.rchisq(1000,2))
  stats.test(rchisq(1000,2))
  #----errors
  my.rchisq(-1)             #comment out in turn as the program should terminate
  my.rchisq(2,-5)           #comment out in turn as the program should terminate
}                        #end function
#-----------------------------------------------
test.rt<-function(x) {
  #   Purpose:
  #     to run a series of tests on my.rt
  #   Input:
  #     x - a vector of numerical data - this will be equivalent to the number of deviates required
  #   Output: 
  #     plots from stats.test - my.rnorm compared to rnorm
  #     error messages from invalid arguments
  #---------------
  t1<-my.rt(x,2)
  is.numeric(t1)
  length(t1)
  #----plots
  stats.test(my.rt(1000,1))
  stats.test(rt(1000,1))
  #----errors
  my.rt(-1)                #comment out in turn as the program should terminate
  my.rt(2,-5)              #comment out in turn as the program should terminate
}                        #end function
#-----------------------------------------------

#test.rnorm(5)
#test.rchisq(5)
#test.rt(5)

#------------- end test ------------------------

