#I confirm that the attached is my own work, except where clearly indicated in the text.
# pseudo-random normal distribution generator
my.rnorm <- function(n, mean = 0, sd = 1) {
  if((!is.numeric(n)) ||    # must be a number
     (!is.numeric(mean)) || # must be a number
     (!is.numeric(sd)) ||   # must be a number
     (length(n) != 1) ||    # must be scalar
     (length(mean) != 1) || # must be scalar
     (length(sd) != 1) ||   # must be scalar
     (n - round(n) != 0) || # n must be an integer
     (n < 1) ||             # n must >= 1
     (sd <= 0))             # sd must > 0
    stop("invalid argument")
  
  # function generates two random values x1, x2 ~ N(0,1)
  generateTwoRnorm <- function() {
    # give w an invalid initial value
    w <- 2
    
    # while w is not valid, repeat generating
    while (w > 1) { 
      # generate u1, u2 ~ uniform(0,1)
      u <- runif(2)
      # transform into [-1, +1]
      u <- u * 2 -1
      # compute w
      w <- sum(u^2)
    }
    
    # compute v
    v <- sqrt((-2 * log(w))/w)
    
    # compute x1, x2 ~ N(0,1)
    x <- u * v
  }
  
  # create empty vector
  r <- numeric()
  # generate requred number of random valus
  while(length(r) < n)
    r <- c(r, generateTwoRnorm())
  
  # correct the length if n is odd
  if(length(r) > n)
    r <- head(r, -1)
  
  # transforme values with mean and sd
  r <- r * sd + mean
  
  #return random values
  r
}

# pseudo-random chi-squared distribution generator
my.rchisq <- function(n, df = 1) {
  if((!is.numeric(n)) ||      # must be a number
     (!is.numeric(df)) ||     # must be a number
     (length(n) != 1) ||      # must be scalar
     (length(df) != 1) ||     # must be scalar
     (n - round(n) != 0) ||   # n must be an integer
     (df - round(df) != 0) || # n must be an integer
     (n < 1) ||               # n must >= 1
     (df < 1))                # sd must >= 1
    stop("invalid argument")
  
  # create empty vector
  q <- numeric()
  
  # generate n values
  for(i in 1:n) {
    # generate df values ~ N(0,1)
    z <- my.rnorm(df)
    
    # get sum of squares
    q <- c(q, sum(z^2))
  }
  
  # return q
  q
}

# pseudo-random student's t-distribution generator
my.rt <- function(n, df = 1) {
  if((!is.numeric(n)) ||      # must be a number
     (!is.numeric(df)) ||     # must be a number
     (length(n) != 1) ||      # must be scalar
     (length(df) != 1) ||     # must be scalar
     (n - round(n) != 0) ||   # n must be an integer
     (df - round(df) != 0) || # n must be an integer
     (n < 1) ||               # n must >= 1
     (df < 1))                # sd must >= 1
    stop("invalid argument")
  
  # generate n values ~ N(0,1)
  z <- my.rnorm(n)
  
  # generate n values have chi-squared distribution with freedom df
  u <- my.rchisq(n, df)
  
  # compute t
  t <- z/sqrt(u/df)
  
  # return t
  t
}

# -------------------- TEST --------------------

# test output of a function is of numeric type
my.test.checkOutputDatatype <- function(f) is.numeric(f(1000))

# test a function has invalid argument error with given arguments 
my.test.invalidArgumentError <- function(f, ...) tryCatch(
    expr <- f(...), 
    # catch the error message and check if it contains "invalid argument"
    error <- function(e) return("invalid argument" %in% e))

# test a function has n number of outputs
my.test.numberOfOutputMatch <- function(f) {
  n <- sample(1:1000, 1)
  length(f(n)) == n
}

# test 1000 outputs of a function are all >= 0 
my.test.allOutputNonNegative <- function(f) all(f(1000) >= 0)

# test 1000 outputs of a function contains nagetive element
my.test.containsNegativeOutput <- function(f) FALSE %in% (f(1000)>=0)

# -------------------- TEST --------------------

# perform the actual test
stopifnot(
  # test output datatype of 3 functions
  my.test.checkOutputDatatype(my.rnorm),
  my.test.checkOutputDatatype(my.rchisq),
  my.test.checkOutputDatatype(my.rt),
  # test detection of invalid input of 3 functions 
  my.test.invalidArgumentError(my.rnorm, 0), # n = 0
  my.test.invalidArgumentError(my.rnorm, -1), # n < 0
  my.test.invalidArgumentError(my.rnorm, 1.1), # n is not integer
  my.test.invalidArgumentError(my.rnorm, "s"), # n is a string
  my.test.invalidArgumentError(my.rnorm, c(1,1)), # n is not scalar
  my.test.invalidArgumentError(my.rnorm, 1, "1"), # mean is a string
  my.test.invalidArgumentError(my.rnorm, 1, c(1,1)), # mean is not a scalar
  my.test.invalidArgumentError(my.rnorm, 1, 0, 0), # sd = 0
  my.test.invalidArgumentError(my.rnorm, 1, 0, -1), # sd < 0
  my.test.invalidArgumentError(my.rnorm, 1, 0, c(1,1)), # sd is not a scalar
  my.test.invalidArgumentError(my.rchisq, 0), # n = 0
  my.test.invalidArgumentError(my.rchisq, -1), # n <0
  my.test.invalidArgumentError(my.rchisq, 1.1), # n is not integer
  my.test.invalidArgumentError(my.rchisq, "s"), # n is a string
  my.test.invalidArgumentError(my.rchisq, 1, 0), # df = 0
  my.test.invalidArgumentError(my.rchisq, 1, -1), # df <0
  my.test.invalidArgumentError(my.rchisq, 1, 1.1), # df is not integer
  my.test.invalidArgumentError(my.rchisq, 1, "1"), # df is a string
  my.test.invalidArgumentError(my.rt, 0), # n = 0
  my.test.invalidArgumentError(my.rt, -1), # n <0
  my.test.invalidArgumentError(my.rt, 1.1), # n is not integer
  my.test.invalidArgumentError(my.rt, "s"), # n is a string
  my.test.invalidArgumentError(my.rt, 1, 0), # df = 0
  my.test.invalidArgumentError(my.rt, 1, -1), # df <0
  my.test.invalidArgumentError(my.rt, 1, 1.1), # df is not integer
  my.test.invalidArgumentError(my.rt, 1, "s"), # df is a string
  # test 3 functions produce desired number of outputs
  my.test.numberOfOutputMatch(my.rnorm),
  my.test.numberOfOutputMatch(my.rchisq),
  my.test.numberOfOutputMatch(my.rt),
  # test my.rchisq have non-negative outputs
  my.test.allOutputNonNegative(my.rchisq),
  # test my.rnorm and my.rt produce negative outputs
  my.test.containsNegativeOutput(my.rnorm),
  my.test.containsNegativeOutput(my.rt)
)
