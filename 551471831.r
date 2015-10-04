# I confirm that the attached is my own work, except where clearly indicated in the 
# text.

# Welcome to The Bear Grylls School of Random Numbeer Generation

####################NORMAL RANDOM DEVIATE GENERATOR#################################

my.rnorm <- function(n, mean=0, sd=1){
  # Purpose: To generate pseudo-random values from a normal distribution using 
  # uniform random deviates 
  # How: Marsaglia and Bray's method
  # Why: To get off of this desert island
  # Inputs: n (number of values to return), mean (mean of  values ot return), sd 
  # (std. deviation of values to return)
  # Outputs: vector of pseudo-random values from a normal distribution
  x <- numeric(n)
  if (n<=0 | n %% 1 != 0){
    stop("Invalid arguments", call. = FALSE)
  }else{
    for (i in 1:n){
      unif.dev <- runif(2)
      u <- 2*unif.dev-1
      w <- sum(u^2)
      while (w<=1){
        v <- sqrt((-2*log(w))/w)
        # Allowing n to be odd, only use one of the pairs of uniform deviates to 
        # generate one N(0,1) deviate 
        U <- sample(u,1)
        x[i] <- U*v
      }
    }
  }
  # To return a vector of N(mean, sd) deviates, transform x as such
  return(x*sd + mean)
}

# Could not manage to get to work. In subsequent functions I will therfore use
# rnorm(n, mean=0, sd=1).

####################CHI-SQUARE RANDOM DEVIATE GENERATOR#############################

my.rchisq <- function(n, df=1){
  # Purpose: Generate chi-squared distributed random deviates using my.rnorm
  # How: Algorithm (Larsen & Marx, 1984): sum of n squared standard nonrmal RVs is 
  # distributed chi-sqaured with n degrees of freedom
  # Input: n (number of deviates to return) and df (degrees of freedom, default 
  # of 1)
  # Ouput: vecotr of pseudo-random chi-squared distributed random deviates
  chi <- numeric(n)
  if (n<=0 | n %% 1 != 0 | df <= 0 | df %% 1 != 0){
    stop("Invalid arguments", call. = FALSE)
  }else{
    for (i in 1:n){
      z <- rnorm(df, mean=0, sd=1)   #put in my.rnorm once up & running
      chi[i] <- sum(z^2)
    }
  }
  return(chi)
}

####################STUDENT'S t RANDOM DEVIATE GENERATOR############################

my.rt <- function(n, df=1){
  # Purpose: Generate student's t distributed random deviates using my.rnorm
  # How: Theorem (Mood, Graybill & Boes, 1974): t = Z/sqrt(U/k) has Student's t 
  # distribution with k df (Z-N(0,1), U-X^2[k], Z&U indep.)
  # Input: n (number of deviates to return) and df (degrees of freedom, default of 1)
  # Ouput: vector of pseudo-random chi-squared distributed random deviates
  t <- numeric(n)
  if (n<=0 | n %% 1 != 0 | df <= 0 | df %% 1 != 0){
    stop("Invalid arguments", call. = FALSE)
  }else{
    for (i in 1:n){
      z <- rnorm(1, mean=0, sd=1)   #put in my.rnorm once up & running
      u <- my.rchisq(1,df)
      t[i] <- z/(sqrt(u/df))
    }
  }
  return(t)
}

####################AUTOMATED TEST FUNCTIONS########################################

check.norm <- function(n, mean=0, sd=1){
  # Purpose: Produce side by side comparisons of my.rnorm and rnorm
  # Why: To ascertain whether my.rnorm does what it is supposed to
  # Input: n (number of values to return), mean (mean of  values ot return), sd (std. deviation of values to return)
  # Output: Similar to eda, a trio of pairs of plots
  # Conclusions: If plots on same row look similar/same then my functions do what they should
  # No need for separate error message, as they are built into my.rnorm
  norm.mine <- my.rnorm(n, mean, sd)
  norm.R <- rnorm(n, mean, sd)
  par(mfrow=c(3,2))
  hist(mine, probability = TRUE)
  lines(density(mine))
  hist(R, probability = TRUE)
  lines(density(R))
  boxplot(mine,horizontal=TRUE)
  rug(mine)
  boxplot(R,horizontal=TRUE)
  rug(R)
  qqnorm(mine)
  qqnorm(R)
  return(summary(mine))
  return(summary(R))
}

check.chisq <- function(n, df=1){
  # Purpose: Produce side by side comparisons of my.rchisq and rchisq
  # Why: To ascertain whether my.rchisq does what it is supposed to
  # Input: n (number of values to return), df (degrees of freedom, default of 1)
  # Output: Similar to eda, a trio of pairs of plots
  # Conclusions: If plots on same row look similar/same then my functions do what they should
  # No need for separate error message, as they are built into my.rchisq
  chisq.mine <- my.rchisq(n, df)
  chisq.R <- rchisq(n, df)
  par(mfrow=c(3,2))
  hist(chisq.mine, probability = TRUE)
  lines(density(chisq.mine))
  hist(chisq.R, probability = TRUE)
  lines(density(chisq.R))
  boxplot(chisq.mine,horizontal=TRUE)
  rug(chisq.mine)
  boxplot(chisq.R,horizontal=TRUE)
  rug(chisq.R)
  qqnorm(chisq.mine)
  qqnorm(chisq.R)
  return(summary(chisq.mine))
  return(summary(chisq.R))
}

check.t <- function(n, df=1){
  # Purpose: Produce side by side comparisons of my.rt and rt
  # Why: To ascertain whether my.rt does what it is supposed to
  # Input: n (number of values to return), df (degrees of freedom, default of 1)
  # Output: Similar to eda, a trio of pairs of plots
  # Conclusions: If plots on same row look similar/same then my functions do what they should
  # No need for separate error message, as they are built into my.rt
  t.mine <- my.rt(n, df)
  t.R <- rt(n, df)
  par(mfrow=c(3,2))
  hist(t.mine, probability = TRUE)
  lines(density(t.mine))
  hist(t.R, probability = TRUE)
  lines(density(t.R))
  boxplot(t.mine,horizontal=TRUE)
  rug(t.mine)
  boxplot(t.R,horizontal=TRUE)
  rug(t.R)
  qqnorm(t.mine)
  qqnorm(t.R)
  return(summary(t.mine))
  return(summary(t.R))
}

####################END OF ASMT 1###################################################