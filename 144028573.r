# MT4113 Assignment 1  
# NAME: ZHOU ZELONG     STUDENT ID: 150008435
# I confirm that the attached is my own work, except where clearly indicated in the text.




# my.rnorm
my.rnorm <- function (n, mean = 0, sd = 1){  # define the user function with mean 0 and sd 1.
  if (round(n) == n & n > 0 & sd > 0) {      # restrict that n is positive integer and sd is positive.
    vector <- numeric(n)                     # save the results in numbers from every loop into dataset vector.
    
    for(i in 1 : n){              # using the for loop to run n times.
      w <- 2                      # given a initial value for w and make sure that w >= 1.
      while(w>1){                 # using a while loop to restrict the value of w.
        U <- runif(2)             # obtain 2 random values on uniform(0,1).
        U <- U*2 - 1              # make the 2 values on unifor(-1,1).
        w <- U[1]^2 + U[2]^2      # obtain the value of w.
      }
      v <- (-2*log(w)/w)^(1/2)    # obtain the value of v.
      X <- U*v                    # obtain the the value of X.
      X <- X*sd + mean            # make X not only from a standard normal distribution but only a normal ditribution with uncertainly mean and sd.
      vector[i] <- X[1]           # save the value of X[1] in i-th loop into the vector.
    }
    return(vector)                # export the dataset vector with n random values which follow the normal distribution.
  }
  else{
    stop("invalid arguments")     # if n is not a positive integer or sd is not positive the output is invalid argument.
  }
}
# for each loop we got two values of X which follows the normal distribution,however, we hope to get n random values
# by n loops. Thus we only choose to save the value of X[1] otherwise we will get 2n random values by n loops.


# my.rchisq
my.rchisq <- function (n, df=1){  # define the user function with degrees of freedom 1
  if (round(n) == n & n > 0){     # make sure that n is a positive integer
    vector <- numeric(n)          # save the result in number from every loop into dataset vector
    
    for(i in 1:n){                # using the for loop to run n times
      Z <- my.rnorm(df)           # obtain Z which is the value follows the standard normal disribution from the user function my.rnorm
      A <- sum(Z^2)               # obtain A which is the sum of the square of Z in each loop
      vector[i] <- A              # save the value of A in i-th loop into the vector
    }
    return(vector)                # export the dataset vector contained n random values which follows the chi-squared distributon
  }
  else{
    stop("invalid arguments")     # if n is not a positive integer the output is invalid argument
  }
}






# my.rt 
my.rt <- function (n, df=1){      # define the user function with degrees of freedom 1
  if(round(n) == n & n > 0){      # make sure that n is a positive integer
    vector <- numeric(n)          # save the result in number from every loop into dataset vector
    
    for(i in 1:n){                # using the for loop to run n times
      Z <- my.rnorm(df)           # obtain Z which is the value follows the standard normal distribution from the user function my.rnorm
      U <- my.rchisq(df)          # obtain U wichi is the value follows the chi-squared disribution from the user function my.chisq
      t <- Z/(U/df)^(1/2)         # obatin the value of t
      vector[i] <- t              # save the value of t in i-th loop into the vector
    }
    return(vector)                # export the dataset vector contained n random values which follows the student's t distribution
  }
  else{
    stop("invalid arguments")     # if n is not a positive integer the output is invalid argument
  }
}






# test functions
test.1<- function(X){
  hist(my.rnorm(X))
}

test.2<- function(X){
  hist(my.rchisq(X))
}

test.3<- function(X){
  hist(my.rt)
}

# There three functions above shows how to draw histograms for the three functions.For my.rnorm and my.rt
# if the graphs look reasonable symmetrically bell-shaped with a large X,it means that the function are suitable.
# For my.chisq, put X in different numbers to see the change of the graphs and compare with graph of the chi-square distribution.

test.4<- function(n){
  X<- my.rnorm(n)
  pass.test<-(length(X)==n & is.numeric(X))
  return(pass.test)
}

test.5<- function(n){
  X<- my.rchisq(n)
  pass.test<-(length(X)==n & is.numeric(X))
  return(pass.test)
}

test.6<- function(n){
  X<- my.rt(n)
  pass.test<-(length(X)==n & is.numeric(X))
  return(pass.test)
}

# pass.test takes on the value TRUE if the object 'x' contains n numeric values