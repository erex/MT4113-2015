my.rnorm<-function( n, mean, sd)
{
  #DESCRIPTION:Function that returns a vector of pseudo-random values from a normal distribution using the Marsaglia and Bray algorithm
  #INPUT:n=number if random normal values, mean=mean of normal distribution, sd= standard deviation of the normal distribution
  #OUTPUT:vector of pseudo-random values following a normal distribution
  if(length(n)>1 | length(mean)>1 |length(sd)>1){
    print("********************************************************************")
    print("invalid arguments")
    print("********************************************************************")
    break
  }
  vec<-vector()
  for(i in 1:ceiling(n/2))
  { 
    #Intialy sets w to value greater than 1 to stat num geneartion
    w=2
    while(w>1){
      rand1=2*(runif(1, min = 0, max = 1))-1
      rand2=2*(runif(1, min = 0, max = 1))-1 
      w=rand1^2+rand2^2
    }
    v=sqrt((-2*log(w))/w)
    if(mean>0){
      val1=(rand1*v)*sd + mean
      val2=(rand2*v)*sd + mean
    }else{
      val1=rand1*v
      val2=rand2*v
    }
    vec <- c(vec,val1, val2);
  }
  #In the case where n is an odd number removes the last value from the vector 
  if(n%% 2>0){
    vec=vec[1:length(vec)-1]
  }
  return (vec)     
}


my.rchisq<-function(n,df,..)
{
  #DESCRIPTION:Write an function returning a vector of pseudo-random χ2-distributed deviates
  #INPUT:n=number of random normal values to be generated and returned,df=degrees of freedom 
  #OUTPUT:a vector of pseudo-random χ2-distributed deviates
  if(length(n)>1 | length(df)>1 ){
    print("********************************************************************")
    print("invalid arguments")
    print("********************************************************************")
    break
  }
  sum=0
  vec<-vector()
  for(i in 1:n){
    norm_vec<-my.rnorm(df,0,1)
    for(j in 1:df)
    {
      sum=sum + norm_vec[j]^2
    }
    vec<-c(vec,sum);
    sum=0
  }
  return(vec)
}


my.rt<-function(n,df)
{
  #DESCRIPTION:Function returning a vector of pseudo-random t-distributed deviates
  #INPUT: n=number of random normal values to be generated and returned,df=degrees of freedom 
  #OUTPUT: a vector of pseudo-random t-distributed deviates
  if(length(n)>1 | length(df)>1 ){
    print("********************************************************************")
    print("invalid arguments")
    print("********************************************************************")
    break
  }
  vec<-vector() 
  for(i in 1:n){
    #generate value with standand normal distribution 
    z<-my.rnorm(1,0,1)
    #generate value with chi square
    u<-my.rchisq(1, df)
    #Compute t distribution 
    t<-(z/sqrt(u/df))
    #add the generated t value to the vec
    vec<-c(vec,t)
  }
  return(vec)
}


test.my.rnorm<-function(n, mean, sd){
  #DESCRIPTION:Function that test my.rnom function
  #INPUT:n=number if random normal values, mean=mean of normal distribution, sd= standard deviation of the normal distribution
  #OUTPUT:A q plot of my.rnorm vs rnorm funvtion in R
  val<-my.rnorm(n,mean,sd)
  qqnorm(val)
  qqline(val)
}


test.my.rchisq<-function(n,df){
  #DESCRIPTION:Function testing my.rchisq function
  #INPUT: n=number of random normal values to be generated and returned,df=degrees of freedom 
  #OUTPUT:A qqplot of my.rchisq versus rchisq funtion in r 
  qqplot(my.rchiq(n,df),rchisq(n,df, ncp = 0))
  qqline(my.rchiq(n,df), distribution = function(p) qchisq(p, df))
}


test.my.rt-function(n,df){
  #DESCRIPTION:Function testing my.rt function
  #INPUT: n=number of random normal values to be generated and returned,df=degrees of freedom 
  #OUTPUT:A qqplot of my.rt versus rt funtion in r
  qqplot(my.rt(n,df),rt(n, df) 
}