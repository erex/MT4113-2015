#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1) {
#Purpose:To generate random normal deviates
#Inputs:natural number n-the number of random normal deviates to be generated
  #mean & sd-The mean and standard deviation of the deviates with defaults 0 and 1 respectively
#Output:A vector of n random normal deviates
  if(n%%1!=0|n<=0|sd<0) {
    stop("invalid arguments")
  }
  num<-ceiling(n/2)
  vars<-vector()
  for(i in 1:num) {
#The following while loop generates random uniformly distributed variables and checks that w is inside the unit circle before moving on in the code
    good.res<-"no"
    while(good.res!="yes") {
      U <- runif(2)
      Sq<-2*U-1
      w<-Sq[1]^2+Sq[2]^2
      if(w<=1) {
        good.res<-"yes"
      }
    }
    v<-sqrt(-2*log(w)/w)
    X<-Sq*v
    vars<-c(vars,X)
  }
#When generating an odd number of values we generate one extra deviate to satisfy the algorithm. The next section of code removes this extra value from the results vector. As the deviates are random we throw away the last value in the vector without loss of generality
  if(length(vars)>=n) {
    vars<-vars[-(n+1)]
  }
  vars<-vars*sd+mean
  return(vars)
}

my.rchisq<-function(n,df=1) {
#Purpose:To generate random chi square values
#Input:Natural number n-The number of values to generate. df-the number of degrees of freedom the values have
#Output:A vector of n random chi square deviates
  if(n%%1!=0|n<=0|df%%1!=0) {
    stop("invalid arguments")
  }
    chi<-vector()
    for(i in 1:n) {
       n.vars<-my.rnorm(df)
       n.vars.sq<-n.vars^2
       chi.val<-sum(n.vars.sq)
       chi<-c(chi,chi.val)
    }
    return(chi)
}

my.rt<-function(n,df=1) {
  #Purpose:To generate random student's t values
  #Input:Natural number n-The number of values to generate. df-the number of degrees of freedom the values have
  #Output:A vector of n random student's t deviates
  if(n%%1!=0|n<=0|df%%1!=0) {
    stop("invalid arguments")
  }
  Z<-my.rnorm(n)
  U<-my.rchisq(n,df)
#Z and U are independent as they are both generated from different pairs of random uniform deviates
  t<-Z/sqrt(U/df)
  return(t)
}

test.length<-function(fun,n=10) {
#Purpose:To check that a function returns the requested number of values
#Input:fun-The function to test, n-The number of values the function should produce
#Output:TRUE if the function gives the correct number of values, FALSE if not
  x<-fun(n)
  if(length(x)==n) {
    return("TRUE")
  } else {
    return("FALSE")
  }
}

test.visual<-function(fun,testfun,x=100) {
#Purpose:To supply plots of two functions and display them simultaneously
  #(used here to examine plots of the user built functions next to their r equivelents)
#Inputs:fun-The first function to be plotted,testfun-The second function to be plotted (The function to be tested against), x-A vector containing the arguments of both functions
#Outputs:Plots of the two functions next to each other
  old.par<-par("mfrow")
  par(mfrow=c(1,2))
  on.exit(par(mfrow=old.par))
  plot(do.call(fun,as.list(x)))
  plot(do.call(testfun,as.list(x)))
}

test.pnorm<-function(fun,testfun,x=100) {
#Purpose:To plot the pnorm values given by the output of two functions (These plots are more likely to have the same range than regular rnorm plots so can be more easily compared)
#Inputs:fun-The first function to be plotted,testfun-The second function to be plotted (The function to be tested against),  x-A vector containing the arguments of both functions
#Outputs:Plots of the pnorm values of the two functions next to each other
  old.par<-par("mfrow")
  par(mfrow=c(1,2))
  on.exit(par(mfrow=old.par))
  a<-do.call(fun,as.list(x))
  b<-do.call(testfun,as.list(x))
  plot(pnorm(a))
  plot(pnorm(b))
}

test.type<-function(fun) {
#Purpose: To test which number types will be accepted by the first argument of a function
#Input:A function which takes one non-default argument
#Output:Statements describing which number types will be accepted by the function
  type<-c("Naturals","Integers","Rationals","Reals")
  num<-c(4,-4,1/3,pi)
  for(i in 1:4) {
    x<-try(fun(num[i]),silent=T)
    if(is(x,"try-error")) {
      cat("The function does not accept",type[i],"\n")
    } else {
      cat("The function accepts",type[i],"\n")
    }
  }
}

test.diff<-function(fun,testfun,n=100,...) {
#Purpose-Tests the difference between the output of two functions (Used in this case to test the mean difference between my.rnorm and rnorm)
#Input-fun-A function which should produce a vector of length n. testfun-another function producing a vector of length n. n-the number of values to generate from each function, ... can be used to supply additional arguments to fun & testfun (df in the case of rt & rchisq)
#Output-The mean difference between the vectors given by the two functions after ordering
  x<-fun(n,...)
  y<-testfun(n,...)
  x<-sort(x)
  y<-sort(y)
  diff<-x-y
  return(mean(diff))
}