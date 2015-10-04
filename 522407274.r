my.rnorm <- function (n, m=-0, sd=1){     
  # call function to generate unifrom random number
  # input: n number 
  # output: two pairs of unifrom random numbers 
  u1 <- 1 # declare object u1
  u2 <- 1 # declare object u2
  x1 <- 1 # declare object x1
  x2 <- 1 # declare object x2
  j <- 1
  w= u1^2 + u2^2 #declare object w
  my.list <- list #create a list to store valuse
  k <-0
  while (w<1){ #condition for rejection pair 
    k <- k+1
    u1 <- runif(1) # generate random uniform numbers 
    u1 =2*u1-1 #transform to unit square
    u2 <- runif(1) #generate random uniform numbers
    u2 =2*u2-1 #transform to unit square
    if (n%%2==1){ # check number is odd
    }
    v = sqrt(-2*log(w)/w) # define varable v
    x1 = u1*v # define x1
    x2 = u2*v #define x2
    my.list[k] <-x1
    my.list[k] <-x2
    my.list[k] # store valuse in a list
    j <- j+1
  }
  # return random values 
  return(k)
}





  my.rchisq <- function(n, df=1){ 
    #call function to generate random distributed numbers
    # input: a number 
    # output: random distributed numbers
    n<-10
    nu <- 2
    X <- matrix(rnorm(n*nu),n, nu)^2 # return matrix of squared normals
    y <- rowSums(X) #summ the squared normals across each row
    # return random t-distributed numbers
    return(y)
  }
  # some code was used form the following book
  # Rizzo, M. (2008). Statistical computing with R. Boca Raton: Chapman & Hall/CRC.
  
  t.test(x,y, var.equal = FALSE, conf.level = 0.95)
  # t.test function is used here to calculate the confidence level
  # for the difference of the means for my.rchisq function
  # information for test was used from the following
  # Verzani, J. (2014). Using R for Introductory Statistics. 2nd ed. 
  # Baca Raton: Taylor & Fransis Group,LLC.

  
  my.rt <- function(n, df=1){
    # call function to generaate random t-distributed numbers
    x<- qt(c(.625, .451), df=4)
    # return random t-distributed valuses 
    return(x)
  }

  
  
 