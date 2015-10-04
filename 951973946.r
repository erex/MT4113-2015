#I accept that the following is my work.


#my.rnorm generates normal random deviates using Marsaglia and Bray's method.
#I create an empty vector the size that the user specifies that will be filled with the normal random deviates.
#A sequence of the odd numbers (loopVect) from 1 to n is used to count the number of cycles used to fill the return vector.
#This is because two normal random deviates are created in each iteration.
#If the user requests an odd number of deviates, the second deviate on the last iteration is disregarded.
#The deviates are assigned to a position in the previously empty vector, which is then returned.

my.rnorm <- function(n,mean=0,sd=1){
  if(n<=0 | n%%1!=0 | sd < 0){
    stop('invalid arguments')
  }
  x<-rep(NA,n)
  loopVect<-seq(1,n,by=2)
  for(i in loopVect){
    u1<-2
    u2<-2
    while(u1^2+u2^2>1){
      u1=runif(1)*2-1
      u2=runif(1)*2-1
    }
    w=u1^2+u2^2
    v=sqrt((-2*log(w))/w)
    x1=u1*v*sd+mean
    x2=u2*v*sd+mean
    x[i]<-x1
    if(i<n){
      x[i+1]<-x2
    }
  } 
  return(x)
}


#my.rchisq function similarly implements an empty vector the size of which is speciied by the user.
#Instead of using nested for loops, I realized that I could simply square a vector and then sum the resulting terms all at once.
#The resulting sums are then integrated into the previously empty vector, which is then returned.

my.rchisq <- function(n,df=1){
  if(n<1 | n%%1!=0 | df<1 | df%%1!=0){
    stop('invalid arguments')
  }
  x<-rep(NA,n)
  for(i in 1:n){
    x[i]<-sum(my.rnorm(df)^2)
  }
  return(x)
}

#I did not use a loop here, as R allowed the calculation to be vectorized.
#I then returned that vector.

my.rt <- function(n,df=1){
  if(n<1 | n%%1!=0 | df<1 | df%%1!=0){
    stop('invalid arguments')
  }
  x<-my.rnorm(n)/(sqrt(my.rchisq(n,df)/df))
  return(x)
}

#This is where I tested my functions. Five pages of histogram comparisons of the functions
#that I created and the functions that are integrated in R. The comparisons are visually
#indistinguishable.

test <- function(){
  pdf('testing.pdf')
  par(mfrow=c(3,2))
  for(i in 1:5){
    hist(my.rnorm(1000),100,freq=FALSE,xlim=c(-3.5,3.5))
    hist(rnorm(1000),100,freq=FALSE,xlim=c(-3.5,3.5))
    hist(my.rchisq(1000,5),100,freq=FALSE,xlim=c(0,25))
    hist(rchisq(1000,5),100,freq=FALSE,xlim=c(0,25))
    hist(my.rt(1000,5),100,freq=FALSE,xlim=c(-5,5))
    hist(rt(1000,5),100,freq=FALSE,xlim=c(-5,5))
  }
  dev.off()
}


