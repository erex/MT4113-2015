###########################################################################
# Function my.rnorm                                                       #
# Produces random normal deviates by converting random uniformly          #
# distributed numbers using Marsaglia and Bray's method                   #
# Takes 3 inputs:                                                         #
#   n    = the number of random deviates required as output               #
#             must be  positive integer                                   #
#   mean = the mean of the normal distribution,                           #
#             optional, if no mean is entered default is zero             #
#             value must be numeric                                       #
#   sd   = the standard deviation of the normal distribution,             #
#             optional, if no sd entered default is one                   #
#             value must be numeric and positive                          #
# Ouput is a vector of n randomly distributed deviates of the normal      #
# distribution defined by the input mean and standard deviation           #
###########################################################################

my.rnorm <- function(n, mean=0, sd=1) {
  
  #check that n is a positive whole number and the mean & sd are numbers and sd is a positive number
  if (is.numeric(n)==FALSE| n%%1!=0 | n<=0 | is.numeric(mean)==FALSE | is.numeric(sd)==FALSE | sd<=0) stop("invalid arguments") 
  
  counter <- 0
  #create an empty output vector of length n
  pair.out <- vector(length=n)                  
  while (counter<n) { 
      
    #create a pair of uniformly distributed random numbers between 0 & 1, then converts them to be between -1 and 1
    pairz <- runif(2,0,1) * 2-1                 
      
    #calculate omega from the pair of randomly distributed numbers and stores it in variable named pairz.w
    pairz.w <- pairz[1]^2+pairz[2]^2            
      
    #reject any omegas greater than 1
    if (pairz.w<=1) {                           
        
      #calculate upsilon and stores it as variable pairz.v
      pairz.v <- sqrt(-2*log(pairz.w)/pairz.w)  
        
      #counter counts the number of random variates that are output, we have one ready to output so is increased by one
      counter <- counter+1                      
        
      #The random normal variable is the original random uniform variable multiplied by upsilon, this is added to the output vector 
      pair.out[counter]<- pairz[1]*pairz.v      
      if (counter<n) {
        counter = counter + 1
        pair.out[counter]<- pairz[2]*pairz.v
      }
    }
  }
    
  #convert the random numbers from the standard normal distribution to that requested in the input
  return(pair.out*sd+mean)                      
}

##########################################################################################
# Function my.rchisq                                                                     #
# Produces random chisq deviates                                                         #
# Has 2 inputs                                                                           #
#   n - number of random deviates required out of the function                           #
#       must be a positive whole number                                                  #
#   df - degrees of freedom of the chi sq distribution                                   #
#       optional, if no value is entered uses a default of 1                             #
#       must be a positive whole number                                                  #
# output is a vector of n randomly distributed numbers using the chi squared distribution#
##########################################################################################

my.rchisq <- function(n, df=1) {
  
  #check to see if arguments input are positive whole numbers 
  if (is.numeric(n)==FALSE | n%%1!=0 | n<=0 | is.numeric(df)==FALSE | df%%1!=0 & df<=0) stop("invalid arguments")          
  
  #creates an empty output vector of length n
  out.chisq <- vector(length=n)      
  
  #each loop of i creates one of the ouputs in the output vector
  for (i in 1:n) {                  
  
    #create a vector of random normal deviates of length df
    vec.cs <- my.rnorm(df)          
    
    #each loop of j up to the degrees of freedom squares the normal deviates and adds it to the previous ones for this loop of i
    for (j in 1:df) {           
      out.chisq[i] <- vec.cs[j]^2 + out.chisq[i] 
    }
  }
  return(out.chisq)                 
}

##########################################################################################
# Function my.rt                                                                         #
# Produces random t deviates                                                             #
# Has 2 inputs                                                                           #
#   n - number of random deviates required out of the function                           #
#       must be a positive whole number                                                  #
#   df - degrees of freedom of the chi sq distribution                                   #
#       optional, if no value is entered uses a default of 1                             #
#       must be a positive whole number                                                  #
# output is a vector of n randomly distributed numbers using the t distribution          #
##########################################################################################
my.rt <- function(n, df=1) {
  
  #checks to see if arguments input are positive whole numbers
  if (is.numeric(n)==FALSE | n%%1!=0 | n<=0 | is.numeric(df)==FALSE | df%%1!=0 | df<=0) stop("invalid arguments")  
  
  #uses the T distribution function to convert from random normal & chisquared into a t distribution
  out.t <- my.rnorm(n) / sqrt(my.rchisq(n, df) / df) 
  
  return(out.t)
}

###################################################################################################################
# Purpose of the function test is to help check if the outputs of the functions generated above are similar to    #
# the default functions in r                                                                                      #
# It has five input parameters,                                                                                   #
# funk which is the type of function to test and should be equal to "norm", "chisq" or "t"                        #
# n the size of vector the function being tested will output                                                      #
# mean with which to test the function                                                                            #
# sd is standard deviation with which to test the function                                                        #
# df is degrees of freedom with which to test the function                                                        #
# The output is a histogram and a qqplot to allow you to do a visual check if the distributions are similar       #
###################################################################################################################

test.dist <- function(funk,n, mean=0, sd=1, df=1) {
  on.exit(par(mfrow=c(1,1)))
  par(mfrow=c(1,2))
  if (funk=="norm") { #creates normal variables if input asks to test norm function
    x<-my.rnorm(n,mean,sd)
    y<-rnorm(n,mean,sd)
  }
  if (funk=="chisq") { #creates chisq variables if input asks to test chisq function
    x<-my.rchisq(n,df)
    y<-rchisq(n,df)
  }
  if(funk=="t"){ #creates t variables if input asks to test t function
    x<-my.rt(n,df)
    y<-rt(n,df)
  }
  if (n>20) {
  hist(x,col="red", breaks=10) #create a histogram with the my.xxxx function ploted in solid red bars 
  hist(y, col="blue", add=T, density=20, breaks=10) #overlay the standard r function on the histogram in lined (not solid) blue bars
  qqplot(x, y)
  } else {
    hist(x,col="red") #create a histogram with the my.xxxx function ploted in solid red bars 
    hist(y, col="blue", add=T, density=20) #overlay the standard r function on the histogram in lined (not solid) blue bars 
  }
}

######################################################################################################################## 
# Function test.out tests the output of the functions to check they give what is expected                              #
# It should output a vector containing TRUE 4 times for the normal function, and 3 TRUES and NA for chisq & t function #
# inputs are:                                                                                                          #
# funk should be equal to "norm", "chisq" or "t"                                                                       #
# n is the length of the output vector for the function in funk                                                        #
# mean, sd and df are the other inputs for the function in funk                                                        #
# The first test is does the function output a vector, if so the first value in the output vector of test.out is TRUE  #
# the second test checks that funk is outputing numbers in the vector, if so the second value in test.out is TRUE      #
# the third ouput will be TRUE if the length of the output of funk is the same as the input requested n,               #
# the fourth output is only for normal functions and checks if the input mean is within the 95% confidence interval    #
# calculated from the sample using the function being tested. If so the fourth value is TRUE                           #
# Please note: approximately 1 in 20 outputs will fail this test even if they are true, so need to run multiple times  #
########################################################################################################################

test.out <- function(funk,n,mean=0,sd=1,df=1) {
  
  #convert funk to correct function to test
  test.funk <- switch(funk, "norm" = my.rnorm(n,mean,sd), "chisq" = my.rchisq(n,df), "t" = my.rt(n,df))
  output <- vector(length=4)
  output[1] <- is.vector(test.funk) # first test
  output[2] <- is.numeric(test.funk) # second test
  if (length(test.funk) == n) output[3] <- TRUE  #third test
  
  # fourth test if testing a normal distribution, else output NA
  if (funk == "norm") {
    # construct an approx 95% confidence interval for normal distribution 
    if (mean > mean(test.funk)- 1.96 * sd(test.funk) / sqrt(n) & mean < mean(test.funk ) + 1.96 * sd(test.funk) / sqrt(1)) output[4] <- TRUE
  } else {
    output[4] <- NA
  }
  return(output)
}




