# I confirm that the attached is my own work, except where clearly indicated in the text

my.rnorm <- function(n,mean=0,sd=1) {
# Purpose: Create n random normal deviates 
# Inputs: n is the number of pairs of deviates you desire, mean is the mean of the deviates with default of zero, sd is the standard deviation of the deviates which has default of one.  
# Outputs: a vector x of n pairs of normally distributed random deviates
    i <- 1
if (((n%%1)==0)=="FALSE") {
  stop("invalid arguments")
}   
# Checks to make sure the value n is an integer, else it stops the function   
if ((sd<=0)=="TRUE") {
  stop("invalid arguments") }
# Checks to make sure the standard deviation is positive    
  x <<- vector(length=n)
# Creates a vector x in the global environment as we need to use it outside of just this function
  while (i < n+1) {
  u <- (runif(2)*2)-1
  w <- (u[1])^2+(u[2])^2
# Calculating the value w based on Marsaglia and Bray's method
  if (w>1) next
# Returns to the top of the while loop without adding to the index count if w > 1
    else {
    v <- sqrt((-2*log(w))/w) 
    x[i] <<- (((u[1]*v)*sd)+mean) 
    x[i+1] <<- (((u[2]*v)*sd)+mean)}
    i <- i+2
# Carries out the required method to create normal deviates and stores these in the vector x in the global environment at the start of the first half and start of the second half of the vector    
  }
  return(x)
}

my.rchisq <- function(n,df=1) {
# Purpose: Create chi-squared random deviates from the previously created normal deviates
# Inputs: n, the number of chi-squared deviates we want to create, df with default one is the number of degrees of freedom of the deviates, the sum of these two values must not exceed the length of the x vector + 1  
# Outputs: a vector chisq which contains n randomly distributed chi-squared variables
if(((n%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to ensure the input n is an integer
if(((df%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to ensure the number of degrees of freedom is an integer
if (((sum(n+df))>(length(x)+1))=="TRUE") {
  stop("invalid arguments") }
# Checks to make sure the user is not asking r to use data which can't be found ie by asking for 40 degrees of freedom if we have less than 40 normal deviates in x  
  if ((df<=0)=="TRUE") {
  stop("invalid arguments") }
# Checks to  make sure the degrees of freedom specified is positive  
  k <- 1
  i <- 1
  sum <- 0
  chisq <<- vector()
# Creates the vector chisq in the global environment as we need to access it outside this function  
  for (i in 1:n) {
    while (k < df+1) {
      sum <- sum + (x[i+k-1])^2
# Sums the squares of the required number of elements of the vector created of normally distributed random variables      
      k <- k+1
    }
    chisq[i] <<- sum
    i <- i+1
    k <- 1
    sum <- 0
# Add to the index count, reset k to one as we need the same number of degrees of freedom for each variable, resets sum to zero as we want to calculate each chi-squared variable individually    
  }
chisq  
}  

my.rt <- function(n,df=1) {
# Purpose: To create randomly distributed t variables
# Inputs: n, user defined, number of t variables to create, this must not exceed the smallest of the two previously created vectors x and chisq, also df is the degrees of freedom which has default one and can be altered as the user wishes  
# Outputs: A vector rt which has n randomly distributed t variables
if (((n%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure that n is an integer
if (((df%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure that the number of degrees of freedom the user has requested is an integer
if ((n<=(length(chisq)))=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure we are not asking R to calculate a number of values larger than the number of chi-squared deviates, else there would be na's in the vector  
if ((df<=0)=="TRUE") {
  stop("invalid arguments") }
# Checks to make sure the number of degrees of freedom is positive  
  i <- 1
  my.t <<- vector(length=(length(chisq)))
  for (i in 1:n) {
  my.t[i] <<- (x[i])/(sqrt((chisq[i])/df))
# Creates each t distributed variable based on the formula provided using the vectors x and chisq as previously defined  
  i <- i+1
  }
  my.t
}

eda <- function(x,...) {
# Purpose: Exploratory Data Analysis to test if the vector x appears to be random normal deviates, as suggested in the lecture on Monday  
# Inputs: The vector x which was created by the earlier function my.rnorm
# Outputs: Returns three plots on the same panel a histogram, a boxplot with rug and a qqnorm plot, these can then by viewed to assess the likelihood of normality  
  par(mfrow=c(1,3))
# Here we are making a change to the global environment as we want to view the three plots at the same time  
  hist(x,prob=TRUE)
  lines(density(x))
# Adds a line to the plot which should appear to be like a normal distribution curve  
  boxplot(x,horizontal=TRUE)
  rug(x)
# Adds a rug to the plot  
  qqnorm(x)
  summary(x)
# Returns a summary of the vector x  
}

is.chisq.linear <- function(chisq,n,df=1) {
# Purpose: Test whether a scatterplot of my.rchisq data and actual rchisq data appears linear, similar to a qq plot
# Inputs: Chisq, this should be exactly the chisq vector from the earlier function, n, this has to be exactly the length of the chisq vector, df this should be the exact same as the number od df used in the my.rchisq  
# Outputs: A scatter plot of points with a fitted regression line on top, we want this to be as linear as possible with a slope close to 1.  
if (((n%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Tests to make sure n is an integer
if (((df%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure the nuber of degrees of freedom is an integer
if ((length(chisq)==n)=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure the length of the chisq vector and the number of trials is the same  
  a <- rchisq(n,df)
  a <- sort(a)
  chisq <- sort(chisq)
# Creating the vector of random chisq then sorting both this and our vector of chisq data  
  plot(a,chisq)
  abline(lm(chisq~a))
# Creating the plot and adding a linear regression line to it  
}

is.t.linear <- function(my.t,n,df=1) {
# Purpose: Test whether our sample of my.rt is similar to the random deviates obtained from the built in function rt
# Inputs: my.t which should be the vector my.t generated earlier, n has to be exactly the length of the vector my.t, df which should be the same as the df used when generating my.t  
# Outputs: A scatterplot with a inear regression line which we would want to be close to going through the origin with slope of 1
if (((n%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to ensure the value n is an integer
if(((df%%1)==0)=="FALSE") {
  stop("invalid arguments") }
# Checks to make sure the value df inputted by the user is an integer  
if ((length(my.t)==n)=="FALSE") {
  stop("invalid arguments") }
# Tests to ensure the length of my.t is the same as the number of random t variables requested  
  b <- rt(n,df)
  b <- sort(b)
  my.t <- sort(my.t)
# Creating a vector of random t variables, then sorting both this and and the vector my.t  
  plot(b,my.t)
  abline(lm(my.t~b))
# Creates the plot and adds a linear regression line to the plot  
}