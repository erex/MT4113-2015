#I confirm that the attached if my own work, except where clearly indicated in the text

#MT4113 : Computing in Statistics, Assignment 1

#Problem No.1 : Write an R function, my.rnorm that returns a vector of pseudo-random
#values from a normal distribution.

#Inputs: number of values to return(n), mean(default = 0), and standard deviation(default = 1)

#Defining functions and inputs 

my.rnorm <- function(n, mean = 0, sd = 1){
  
  #I am setting mean as a single value and if it is not correct, the message 
  #"ERROR: Invalid Argument" will be printed
  if((lengthC(mean) == 1) == T & is.numeric(mean) == T){
    
    #I am setting sd as a non-negative value and if it is not correct, then the message
    #"ERROR: Invalid Argument" will be printed
    if(sd >= 0 & (length(sd) == 1) == T & is.numeric(sd) == T) {
      
      # I am setting n as a positive integer and a single value; and if it is not correct, then
      # then the message "ERROR: Invalid Argument will be printed
      if(lenght(n) > 1) stop("ERROR: Invalid Argument \n")
        
        #Calculating a rv x by the algorithm given
        x <- numeric() #setting x as numeric 
      }
  
    }
  }

#Problem no.2: Write an function my.rchisq returning a vector of pseudo-random chi-
#-square distibuted deviates 

#inputs - Number of values to return (n), degrees of freedom of the distribution (df)
my.rchisq <- function(n, df = 1){
  
  #I am setting df as a non-negative integer and a single value and if this is not 
  #correct, then the message " ERROR: Invalid Argument" will be printed 
  h <- df - round(df) #Defining h as a difference between rounded value of df and df itself.
  if(df > 0 & (h == 0) == T  (length(df) == 1) == T & is.numeric(df) == T){
    
    # I am setting n as a positive integer and a single value and if this is not correct then
    #then the message "ERROR: Invalid Argument" will appear 
    if(length (n) > 1) stop ("ERROR: Invalid Argument \n")
      
    #Calculating a random variable x by given algorithm and former function my.rnorm
      x <- c(1:n)
      for (i in 1:n)
        x[i] <- sum (exp(my.rnorm(n)))
        return(x)
    } else {
      cat ( "ERROR: Invalid Argument")
    } 
  } else {
    cat ( "ERROR: Invalid Argument")
  }


#Problem No.3 Write an R function my.rt returning a vector of pseudo-random 
#distributed deviates 

#Inputs: Number of values to return (n) and degrees of freedom(df1 = 1)

#Defining function and inputs
my.rt <- function (n , df1 = 1){
  
  #I am setting df1 as a non-negative integer and a single value and if this is not 
  #correct then the message "ERROR: Invalid Argument" will be printed
  g <- df1 - round(df1)
  if(df > 0 & (g == 0) == T  (length(df1) == 1) == T & is.numeric(df1) == T){
    
    #I am setting n as a positive integer and a single value and if this is not correct,
    # then the message "ERROR: Invalid Argument" will be printed
    if(length(n) > 1) stop ("ERROR: Invalid Argument \n")
      
      #Calculating a random variable y by given algorithm and former functions my.rnorm and my.rchisq
      y <- c(1:n)
      for (i in 1:n)
        y[i] <- rnorm / sqrt(rchisq/df1)
      return(y)
    } else {
      cat ( "ERROR: Invalid Argument")
    }else {
      cat ( "ERROR: Invalid Argument")
    }
}