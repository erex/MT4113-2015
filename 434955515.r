#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n,mean=0,sd=1){
  #error trap to receive only positive integer as input
  if(n%%1!=0 |n <0) stop("invalid arguments") 
  x <- c()
  #generate n/2 pairs for even "n"/ (n+1)/2 pairs for odd "n"
  for(i in 1:ceiling(n/2)){ 
    u <- runif(2)
    u <- 2*u - 1
    w <- u[1]^2 + u[2]^2
    #re-compute u and w for w>1
    while(w>1){ 
      u <- runif(2)
      u <- 2*u - 1
      w <- u[1]^2 + u[2]^2
    } 
    v <- sqrt(((-2)*log(w))/w)
    y <- u*v
    #Add 2 values of y in vector x
    x <- append(x,y)  
    i <- i + 1
  } 
  #check if n is even or odd
  if(n%%2==0) { 
  } else {
    # if  n is odd remove the last element to make the length of x is equal to n
    x <- x[-(n+1)]
  } 
  x <- sd*x + mean
}

my.rchisq <- function(n, df=1) {
  if(n<0) stop("invalid arguments") 
  z <- c()
  for(i in 1:n){
    i <- i + 1
    ss.z <- sum((my.rnorm(df,0,1))^2)
    z <- append(z,ss.z)
  }
  z <- z  
}

my.rt <- function(n,df=1){
  if(n%%1!=0 | n<0) stop("invalid arguments") 
  Z <- my.rnorm(n)   
  U <- my.rchisq(n,df=1) 
  t<- Z/sqrt(U/df)
}

# This function uses qqplot to test normality 
# passes test when the points are close to the line
test.norm <- function(n,mean=0,sd=1){
  Z <- my.rnorm(n,mean,sd)
  qqnorm(Z)
  qqline(Z)
}
#Tests whether the outputs of each function gives 
#correct number of numeric values.
test.general <- function(n,mean=0,sd=1,df=1){
  Z <- my.rnorm(n,mean,sd)
  U <- my.rchisq(n,df)
  t <- my.rt(n,df)
  TorF <- length(Z)==n & length(U)==n & length(t)==n &
          is.numeric(Z) & is.numeric(U) & is.numeric(t)
}


