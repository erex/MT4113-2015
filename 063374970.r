#The following three functions are based on the algorithm for normally-distributed deviates of Marsaglia and Bray.

#The my.rnorm function returns n pseudo-random values from the normal distribution.
#It has three arguments: the number of values that returns,n
#the mean of values that returns,mean
#and the standard deviation of values that returns,sd.
#The three arguments of the function must meet their own restrictions otherwise the code breaks and prints an error message.
#The mean and the standard deviation must be positive numbers and the number of values must be a natural number.


my.rnorm<-function(n,mean,sd) {
  i=1:n
  u<-2*runif(i)-1               #it returns i random numbers from the uniform distribution.
  w<-sum(u^2)
  if (mean<0) {
    print("invalid arguments")
  }
  else if (sd<0) {
    print("invalid arguments")
  }
  else if ((n<0 & !(is.integer(n))) & !(is.integer(n))) {
    print("invalid arguments")
  }
  else {                        #the code continues when the above restrictions are met.
    while (w>1) {
      j=1:n                     #this is the rejection step where the code rejects the vector and generates a new one.
      u<-2*runif(j)-1           #the while loop will stop if and only if the criteria w<=1 is met.
      w<-sum(u^2)
    }
    v<-sqrt((-2*log(w))/w)
    x<-u*v
    X<-x*sd+mean                #this is the transormation of the values with mean 0 and sd 1 into values with mean "mean" and standard deviation "sd".
    return(X)                   #it delivers the vector of normally distributed variates.
  }
}



#The my.rchisq function returns n pseudo-random values from the x^2 distribution.
#It has two arguments: the number of values that returns,n
#and the degrees of freedom,df.
#The two arguments of the function must meet their own restrictions otherwise the code breaks and prints an error message.
#The number of values must be a natural number and the degrees of freedom must be equal to the number of values.


my.rchisq<-function(n,df) {
  i=1:n
  u<-2*runif(i)-1               #it returns i random numbers from the uniform distribution.
  w<-sum(u^2)
  if (df!=n) {
    print("invalid arguments")
  }
  else if ((n<0 & !(is.integer(n))) & !(is.integer(n))) {
    print("invalid arguments")
  }
  else {                        #the code continues when the above restrictions are met.
    while (w>1) {
      j=1:n                     #this is the rejection step where the code rejects the vector and generates a new one.
      u<-2*runif(j)-1           #the while loop will stop if and only if the criteria w<=1 is met.
      w<-sum(u^2)
    }
    v<-sqrt((-2*log(w))/w)
    x<-u*v
    y<-sum(x^2)                 #it transforms the independent standard normal random variables into x^2 distributed variables.
    for (k in 1:n) {            #for every value of i, it calculates one y variable.
      y[i]<-y                   #it creates a vector of all the y variables.
    }
    return(y[i])                #it delivers the vector of x^2 distributed variates.
  }
}




#The my.rt function returns n pseudo-random values from the student distribution.
#It has two arguments: the number of values that returns,n
#and the degrees of freedom,df.
#The two arguments of the function must meet their own restrictions otherwise the code breaks and prints an error message.
#The number of values must be a natural number and the degrees of freedom must be equal to the number of values.


my.rt<-function(n,df) {
  i=1:n
  u<-2*runif(i)-1               #it returns i random numbers from the uniform distribution.
  w<-sum(u^2)
  if (df!=n) {
    print("invalid arguments")
  }
  else if ((n<0 & !(is.integer(n))) & !(is.integer(n))) {
    print("invalid arguments")
  }
  else {                        #the code continues when the above restrictions are met.
    while (w>1) {
      j=1:n                     #this is the rejection step where the code rejects the vector and generates a new one.
      u<-2*runif(j)-1           #the while loop will stop if and only if the criteria w<=1 is met.
      w<-sum(u^2)
    }
    v<-sqrt((-2*log(w))/w)
    x<-u*v
    y<-sum(x^2)                 #it transforms the independent standard normal random variables into x^2 distributed variables.
    t<-x/(sqrt(y/df))           #it transforms the standard normal distributed variables and the x^2 distributed variables into t-distributed variables.
    return(t)                   #it delivers the vector of t-distributed variates.
  }
}



#The following tests check if our functions produce a vector of 10 values when asked.

a<-my.rnorm(10,0,1)
pass.test<-(length(a)==10 & is.numeric(a))

b<-my.rchisq(10,10)
pass.test<-(length(b)==10 & is.numeric(b))

c<-my.rt(10,10)
pass.test<-(length(c)==10 & is.numeric(c))




#The following diagrams demonstrate how close our functions are to the normal distribution.

my.rnorm1<-my.rnorm(10,0,1)
hist(my.rnorm1)
qqnorm(my.rnorm1)
qqline(my.rnorm1)


my.rt1<-my.rt(10,10)
hist(my.rt1)
qqnorm(my.rt1)
qqline(my.rt1)


my.rchisq1<-my.rchisq(10,10)
hist(my.rchisq1)
qqnorm(my.rchisq1)
qqline(my.rchisq1)


#Another way of checking if our functions are close to the normal distribution is with the function "expect_that" after installing the "testthat" library.

expect_that(my.rnorm(2,2,1),is_identical_to(rnorm(2,2,1)))
expect_that(my.rchisq(2,2),is_identical_to(rnorm(2,2,1)))
expect_that(my.rt(2,2),is_identical_to(rnorm(2,2,1)))
