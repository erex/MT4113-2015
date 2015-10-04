#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm <- function(n,mean=0,sigma=1){
  Y<-c()
#Check if n is integer
  if (n %% 1 != 0){
    stop("Invalid Arguments")
    }else{
    
#For n is even integer
  if (n %% 2 == 0){
    for (i in 1:(n/2)){
      
#Transform U to range [-1,1]
      U<-c(runif(2))
      U=2*U-1
      w<-U[1]^2+U[2]^2
      
#Reject w=(u_1)^2+(u_2)^2>1
      while (w > 1){
        U=c(runif(2))
        U=2*U-1
        w<-U[1]^2+U[2]^2
      }
      v<-sqrt(-2*log(w)/w)
      X<-U*v
#The function "append" adds the X vector to Y vector
      Y<-append(Y,X)
      i=i+1
    }
#Transform Y_i ~ N(0,1) to N(mean, sigma) for all i
    Y=sigma*Y+mean
  }else{
    
#For n is odd integer#
    {{for (i in 1:((n+1)/2)){
      U<-c(runif(2))
      U=2*U-1
      w<-U[1]^2+U[2]^2
      while (w > 1){
        U=c(runif(2))
        U=2*U-1
        w<-U[1]^2+U[2]^2
      }
      v<-sqrt(-2*log(w)/w)
      X<-U*v
      Y<-append(Y,X)
      i=i+1
      } 
#Remove one variable from the Y vector to make it odd
      Y<-Y[-((n+1)/2)]
      }
      Y=sigma*Y+mean
    }
    }
    }
}


my.rchisq <- function(n,df=1){
  Y=c()
  
#Check if df is integer
  if (df %% 1 != 0){
    stop("Invalid Arguments")
  }else{
    
#Check if n is integer
  if (n %% 1 != 0){
    stop("Invalid Arguments")
  }else{
  for (i in 1:n) {
    Z<-(my.rnorm(df))
    Z2=Z*Z
    chisq<-sum(Z2)
    Y=append(Y,chisq)
    i=i+1
  }
  Y
  }
  }
}

my.rt <- function(n,df=1){
  Y=c()
  
#Check if df is integer
  if (df %% 1 != 0){
    stop("Invalid Arguments")
  }else{
    
#Check if n is integer
  if (n %% 1 != 0){
    stop("Invalid Arguments")
    }else{
  for(i in 1:n){
    Z<-my.rnorm(1)
    U<-my.rchisq(df)
    t<-Z/(sqrt(U/df))
    Y=append(Y,t)
    i=i+1
  }
  Y
  }
  }
}

#For input n, test if the output for all 3 function are length n 
#and check if they are numeric value
test <- function(n){
  N <- my.rnorm(n)
  C <- my.rchisq(n)
  T <- my.rt(n)
  h <-(length(N)==n & length(C)==n & length(T)==n & 
       is.numeric(N) & is.numeric(C) & is.numeric(T))
  h
}

#Gives Q-Q plot of my.rnorm against rnorm to see if my.rnorm is normally distributed or not
test.rnorm <- function(n){
  N <- my.rnorm(n)
  M <- rnorm(n)
  qqplot(N,M)
}

#Test whether reject mean=0 for my.rnorm at 95% C.I. using t-test , if output TRUE then accept
test.rnorm.mean <- function(n){
  N <- my.rnorm(n)
  t<-t.test(N)
  h<-(t$conf.int[1]<0 & t$conf.int[2]>0)
  h
}

#my.rchisq and my.rt depends on my.rnorm and so if rnorm is normally distributed,
#both of the functions should behave as expected.