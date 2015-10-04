# I comfirm that the attached is my own work, except where clearly indicated in the text.
# NAME:zhe zhang  ID:120006433


my.rnorm<-function(n,mean=0,sd=1){
  #trapping the error input, the correct input won't be negative, with decimal or non-numeric.
  if(is.numeric(n)==FALSE){stop("invalide arguments")}
  if(n%%1!=0){stop("invalide arguments")}
  if(n<0){stop("invalide arguments")}
  # k is a value to mark the number of loop finished.
  k<-0
  #set U as the null vector, it will combine the result from every loop. 
  U<-c()
  # this loop produce "n" number of value to return.
  while(k<n/2){
    #repeatedly generate a new pair of uniform deviates until met the condition W<1. 
    repeat{
      u<-(runif(2,-1,1))
      W<-sum(u^2);
      if(W<1) break
    }
    #calculate X1 and X2
    v<-sqrt(-2*log(W)/W)
    X1<-u[1]*v*sd+mean
    X2<-u[2]*v*sd+mean
    #combine the outcome from each loop into a vector.
    U<-c(U,X1,X2)
    #mark the number of the loop. 
    k<-k+1
  }
  # if n is an even number, return n number of outcome
  if(n%%2==0){
    return(U)
  }else{
    #n is an odd number,return n-1 number of outcome
    return(U[-1])
  }
}

my.rchisq<-function(n,df=1){
  #trapping the error input, the correct input won't be negative, with decimal or non-numeric.
  if(is.numeric(n)==FALSE){stop("invalide arguments")}
  if(n%%1!=0){stop("invalide arguments")}
  if(n<0){stop("invalide arguments")}
  # k is a value to mark the number of loop finished.
  k<-0
  #set X as the null vector, it will combine the result from every loop. 
  X<-c()
  #this loop produce a random variate with chi-squared distribution.
  while(k<n){
    k<-k+1
    #algorithm of chi-squared distribution
    z<-rnorm(df)
    x<-sum(z^2)
    #Combine result into a vector
    X<-c(X,x)
  }
  #return the vector with n number of random variates with chi-squared distribition.
  return(X)
}

my.rt<-function(n,df=1){
  #trapping the error input, the correct input won't be negative, with decimal or non-numeric.
  if(is.numeric(n)==FALSE){stop("invalide arguments")}
  if(n%%1!=0){stop("invalide arguments")}
  if(n<0){stop("invalide arguments")}
  #get the sample from my.norm
  z<-my.rnorm(n)
  #get the sample from my.rchisq
  u<-my.rchisq(n,df)
  #get the value of t-distribution
  t<-z/sqrt(u/df)
  #return result
  return(t)
}


test.my.rnorm<-function(n){
  #n is the sample size
  # produce n sample from my.rnorm
  x<-my.rnorm(n)
  #1 page 2 plots
  ###par(mfrow=c(1,2))
  #plot the histogram and the density of the sample 
  ###hist(x,probability = TRUE);lines(density((x)),col=2)
  #plot qqnorm and qqline, if it is straight line, it is normally distributed
  ###qqnorm(x);qqline(x, col = 2)
  #The null-hypothesis of this test is that the population is normally distributed, if p-value>0.05,accept the null hyphthesis
  ###shapiro.test(x)
}

test.my.rchisq<-function(n,df=1){
  #n is the sample size,df is degree of freedom
  # the function contains the one sample t-test, the assumption of it is with more than 30 samples 
  if(n<=30){stop("n must > 30")}
  #k used to mark the number of the loop
  k<-0
  #set the vetor 
  ymean<-c()
  yvar<-c()
  ypvalue<-c()
  retu<-c()
  #loop, creating n samples
  while(k<n){
    #this is p-value of ks.test between my.rchisq and rchisq(which is from R), we creat n number of p-value and see if most of them are great than 0.05
    xpvalue<-ks.test(my.rchisq(n,df),rchisq(n,df))$p.value
    #this is the mean of the sample from 1000 my.rchisq  
    xmean<-mean(my.rchisq(1000,df))
    #this is the variance of the sample from 1000 my.rchisq   
    xvar<-var(my.rchisq(1000,df))
    #combine all the result into a vector
    ymean<-c(xmean,ymean)
    yvar<-c(yvar,xvar)
    ypvalue<-c(ypvalue,xpvalue)
    #mark the number of loop
    k<-k+1
  }
  #in chi-square distribution, the mean of it is degree of freedom and the variance of it is the double of degree of freedom.
  #we are going to do the hypothesis test, setting null hypotthesis as mean of chi-square equal to df, variance of chi-square equal to 2*df
  #we use one sample t-test, they produce the t of mean
  t.mean<-(mean(ymean)-df)/(sd(ymean)/sqrt(length(ymean)))
  #after we got t of mean, we put it into pt() to find out if it is greater than 0.05, if so, the null hypothesis can not be rejected
  #and mean of rchisq is equal to its degree of freedom,fitting the chi-square property. 
  #but it doesn't mean it is chi-square distribution,we will do the variance hypothesis test as well
  ttmean<-pt(t.mean,(length(ymean)-1))
  # similar as the mean, we calculate the p-value
  t.var<-(mean(yvar)-2*df)/(sd(yvar)/sqrt(length(yvar)))
  ttvar<-pt(t.var,(length(yvar)-1))
  #plot the histogram and density curve of p-value of ks.test to see if most of them are greater than 0.05
  
  ###hist(ypvalue,probability = TRUE);lines(density(ypvalue),col=2)
  
  retu<-c(ttmean,ttvar)
  #return the P-value of 2 hypothesis test.
  
  ###return(retu)
}

test.my.rt<-function(n,df=3){
  #trap,the function contains the one sample t-test, the assumption of it is with more than 30 samples 
  if(n<=30){stop("n must > 30")}
  # the variance of t-distribution equal to df/(df-2) if df>2, we are going to do the hypothesis test, so here we 
  #set df >2
  if(df<=2){stop("df must be geater than 2")}
  # k used to mark the number of the loop
  k<-0
  #set the vector
  y.var<-c()
  y.pvalue<-c()
  #loop, creating n samples
  while(k<n){
    #mark the number of loop
    k<-k+1
    #draw n sample of varianve from my.rt of size 1000.
    rt.var<-var(my.rt(1000,df))
    #get the mean
    x<-mean(rt.var)
    #this is p-value of ks.test between my.rt and rt(which is from R),
    #we creat n number of p-value and see if most of them are great than 0.05.
    xpvalue<-ks.test(my.rt(1000,df),rt(1000,df))$p.value
    #combine the result into the vector
    y.var<-c(x,y.var)
    y.pvalue<-c(y.pvalue,xpvalue)
  }
  #in t-distribution, the variance of it is df/(df-2) if df>2, we do the one sample t-test to see if variance=df/(df-2) by p-value.
  #we see the null hypothesis is that variance is equal to df/(df-2)
  #here we get t value
  t.var<-(mean(y.var)-(df/(df-2)))/(sd(y.var)/sqrt(length(y.var)))
  #by using pt, we get p-value
  tt.var<-pt(t.var,(length(y.var)-1))
  #plot the histogram and dinsity curve of p-value of ks.test to see if most of them are grater than 0.05
  
  ###hist(y.pvalue,probability = TRUE);lines(density(y.pvalue),col=2)
  
  #return the p value
  
  ###return(tt.var)
}



