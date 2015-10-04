# I comfirm that the attached is my own work, except where clearly indicated in the text.

        ############
######## R Function ########
        ############
        
        
######## rnorm Function ########

my.rnorm <- function(n, mean = 0, sd = 1) {     # create a function my.rnorm
  if (n <= 0 | round(n) != n | sd < 0) {        # to make sure n is positive integer
    
      stop("invalid arguments")                 # stop the program with "invalid argument" when the function create incorrect output
    }
    else {
    rnorm.results <- numeric(n)                 # create a numeric object
    
    for (i in 1:n) {                         
      
      w <- 7                                    # set any w which is greater than 1 to get into the loop
      while (w > 1) {                           # start a while loop to make w <= 0
        U <- runif(2)                           # create U with two random values
        U <- 2*U-1
        w <- U[1]^2+U[2]^2
      }                                         
      v <- sqrt(-2*log(w)/w)                    # define v
      X <- U*v                                  # define X for normally distribution
      X <- X*sd+mean                            # transform the algorithm deliver values
      
      rnorm.results[i] <- X[1]                  # get the numbers for that event
      }
  } 
  return(rnorm.results)                         # return the results
 }                                              # end of this event

                                             
#---------------------------------------------------------------------------------------------



######## rchisq Function ########

my.rchisq <- function(n, df=1) {                # create a function my.rchisq
  
  if (n <= 0 | round(n) != n                    # to make sure n and df are positive integers
      | df <= 0 | round(df) != df) {
    stop("invalid arguments")                   # stop the program with "invalid argument" when the function create incorrect output
  }
    else {             
    rchisq.results <- numeric(n)                # create a numeric object
    
    for (i in 1:n) {
      Z <- my.rnorm(df)                         
      rchisq.results[i] <- sum(Z^2)             # get the values
    }
    return(rchisq.results)                      # return the results
  } 
}                                               # end of this event


#--------------------------------------------------------------------------------------------



######## rt Function ########

my.rt <- function(n, df=1) {                    # create a function my.rt
  
  if (n <= 0 | round(n) != n
      | df <= 0 | round(df) != df) {            # to make sure n and df are positive integers
    
      stop("invalid arguments")                 # stop the program with "invalid argument" when the function create incorrect output
    }
    else {
    results <- numeric(n)                       # create a numeric object
    
    for (i in 1:n) {
      Z <- my.rnorm(1)                          # create value Z has a standard normal distribution
      U <- my.rchisq(1, df)                     # create value U has a chi-squared distribution
      
      results[i] <- Z/sqrt(U/df)                # get the values
    }
    return(results)                             # return the results
  }
}                                               # end of this event


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

       ###############
####### Function Test #######
       ###############



####### rnorm Function Testing #######

# set H0 = dataset is normally distribution.
#     H1 = dataset is not normally distribution.

# rnorm.test1 <- my.rnorm(100, mean=0, sd=1)      # create numeric dataset rnorm.test1
# shapiro.test(rnorm.test1)                       # perform Shapiro-Wilk Normality Test

# It is Shapiro-Wilk Normality Test which can perform normality
# test of a dataset with hypothesis. When n=100, the results of this test are
# the value of w and p-value. In this case, p-values is always greater
# than 0.05, H0 can not be rejected. so it is considered that the dataset is normally distributed.


#--------------------------------------------------------------------------------------------



# rnorm.test2 <- my.rnorm(100)                    # create numeric dataset rnorm.test2 with 100 numbers
# qqnorm(rnorm.test2); qqline(rnorm.test2, col=2) # plot using a qqplot

# Observe the plot that the circles all lie close to the line, though there is 
# a little wriggle about the line and there are some outliers at the both ends 
# of the line. It is reasonable to say that the dataset is normally distributed.



#--------------------------------------------------------------------------------------------



# set H0 = dataset is normally distribution.
#     H1 = dataset is not normally distribution.

# rnorm.test3 <- my.rnorm(100)                     # create numeric dataset rnorm.test3
# ks.test(rnorm.test3, "pnorm")                    # perform Kolmogorov-Smirnov Test

# In this case, p-value is always greater than 0.05, H0 can not 
# be rejected. So it is considered that the dataset is normally distributed.



#-----------------------------------------------------------------------------------------------




####### rchisq Function Testing #######

# x <- my.rchisq(10000, df=5)                       # create numeric dataset with 10000 numbers
# hist(x, prob=TRUE)                                # plot a histogram
# curve(dchisq(x, df=5), col='green', add = TRUE)   # draw a trend line with df=5
# curve(dchisq(x, df=10), col='red', add = TRUE)    # draw a trend line with df=10

# It can be seen from the graph that all the values are greater than 0,
# and the distribution is deflective. As the df increases, the distribution trends 
# to be normal. So it is considered that the dataset is chi-square distributed.



#--------------------------------------------------------------------------------------------



# set H0 = dataset is chi-sqaure distributed.
#     H1 = dataset is not chi-sqaure distribued.

# rchisq.test <- my.rchisq(100, 1)                  # create numeric dataset rchisq.test with 100 numbers
# ks.test(rchisq.test, "pchisq", 1)                 # perform Kolmogorov-Smirnov Test

# In this case, p-value is always greater than 0, H0 can not be rejected.
# So it is considered that the dataset is chi-sqaure distributed.



#------------------------------------------------------------------------------------------------



####### rt Function Testing #######

# set H0 = dataset is t distributed.
#     H1 = dataset is not t distribued.

# rt.test <- my.rt(100, 1)                            # create numeric dataset rt.test with 100 numbers
# ks.test(rt.test, "pt", 1)                           # perform Kolmogorov-Smirnov Test

# In this case, p-value is always greater than 0, H0 can not be rejected.
# So it is considered that the dataset is t distributed.



#-------------------------------------------------------------------------------------------------



# x <- my.rt(1000, df=10)                             # create a dataset with 1000 numbers
# hist(x, prob=TRUE)                                  # plot a histogram
# curve(dt(x, df=10), col='blue', add=TRUE)           # draw a trend line with df=10
# curve(dt(x, df=1000), col='red', add=TRUE)          # draw a trend line with df=1000

# It can be seen from the graph that it looks like a normally distributed graph.
# As the df decrease, the trend line is more flat; when df is infinite, it will be
# standard normally distributed. So it is considered that the dataset is t distributed.



#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------


       ####################
####### Function code Test #######
       ####################


####### my.rnorm Function #######

# run my.rnorm function code, then test the function code with following:

# my.rnorm(10)                                        # results come up with 10 numbers
# my.rnorm(100)                                       # results come up with 100 numbers
# my.rnorm(-10)                                       # results come up with "invalid arguments"
# my.rnorm(0)                                         # results come up with "invalid arguments"
# my.rnorm(4.5)                                       # results come up with "invalid arguments"
# my.rnorm(10, 0, -10)                                # results come up with "invalid arguments"

# so my.rnorm works well


####### my.rchisq Function #######

# run my.rchisq function code, then test the function code with following:

# my.rchisq(10)                                       # results come up with 10 numbers
# my.rchisq(100)                                      # results come up with 100 numbers
# my.rchisq(0)                                        # results come up with "invalid arguments"
# my.rchisq(-10)                                      # results come up with "invalid arguments"
# my.rchisq(4.5)                                      # results come up with "invalid arguments"
# my.rchisq(10, 0)                                    # results come up with "invalid arguments"
# my.rchisq(10, -10)                                  # results come up with "invalid arguments"
# my.rchisq(10, 4.5)                                  # results come up with "invalid arguments"

# so my.rchisq function works well


####### my.rt Function #######

# run my.rt function code, then test the function code with following:

# my.rt(10)                                       # results come up with 10 numbers
# my.rt(100)                                      # results come up with 100 numbers
# my.rt(0)                                        # results come up with "invalid arguments"
# my.rt(-10)                                      # results come up with "invalid arguments"
# my.rt(4.5)                                      # results come up with "invalid arguments"
# my.rt(10, 0)                                    # results come up with "invalid arguments"
# my.rt(10, -10)                                  # results come up with "invalid arguments"
# my.rt(10, 4.5)                                  # results come up with "invalid arguments"

# so my.rt function works well







