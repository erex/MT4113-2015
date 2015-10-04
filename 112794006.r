#I confirm that the attached is my own work, except where clearly indicated in the text.

my.rnorm<-function(n,mean=0,sd=1){
  # check if n is an odd number.if it is, k=n+1.
  # remove the final normal deviate from the result to give an odd number.
  test(m=n)
  if(n%%2 != 0){
    k<-n+1
  }else{
    k<-n
  }
  # generate n/2 pair(s) of uniformly distributed numbers,
  # and turn them into the range of (-1,1).
  # store the normal pair in a list.
  result<-list()
  counter<-1
  if(k==0){
    return (unlist(result))
  }else{
    for(i in 1:(k/2)){
      u1<-1;u2<-1
      while(u1^2+u2^2>1){
        u1<-runif(1,min=0,max=1)*2-1;u2<-runif(1,min=0,max=1)*2-1
      }
      w<-u1^2+u2^2
      v<-sqrt(-2*log(w)/w)
      n1<-(u1*v)*sd+mean;n2<-(u2*v)*sd+mean
      result[counter]<-n1
      result[counter+1]<-n2
      counter=counter+2
    }
    # check the number of values in the output based on n(initial).
    a<-unlist(result)
    if((n)%%2==0){
      final<-a
    }else{
      final<-a[1:n]
    }
    return (final)
  }
}

my.rchisq<-function(n,df=1){
  # check if df is a valid value.
  # this is a test funciton described below.
  test(df=df)
  result<-list()
  if(n==0){
    return (unlist(result))
  }else{
    for(i in 1:n){
      chi<-0
      # generate df standard normal deviates from my.rnorm.
      norm_d<-my.rnorm(n=df,mean=0,sd=1)
      for(j in norm_d){
          chi=chi+j^2
      }
      result[i]<-chi
    }
    return (unlist(result))
  }
}

my.rt<-function(n,df=1){
  # check if df is a valid value.
  test(m=n,df=df)
  result<-list()
  if(n==0){
    return (unlist(result))
  }else{
    for(i in 1:n){
      # generate a pair of standard normal deviate from my.rnorm.
      n1<-my.rnorm(1,mean=0,sd=1)
      # generate df standard normal deviates to form a chisqr deviate.
      n2<-my.rnorm(df,mean=0,sd=1)
      chi<-0
      for(j in n2){
        chi=chi+j^2
      }
      t<-n1/sqrt(chi/df)
      result[i]<-t
    }
    return(unlist(result))
  }
}

# test function.
# constraint that df can't be less than 1.
test<-function(m,df=1){
  if(df<1){
    stop(('please re-enter the degree of freedom!'),call. = FALSE)
  }
}
