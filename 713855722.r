
#I confirm that the attached is my own work, except where clearly indicated in the text. 

# Basic Function
my.rnorm<-function(n,mean=0,sd=1) {
  
  # Stop function
  if(n<0)
    stop ("invalid arguments") 
  # Generate n uniform variates, transform them to unit square and sum them up
    n<-1
    for (i in (1:n)) {
    # Introduction of the rejection step
      while (w>1) {
        n<-n+2
        u[i]<-runif(n,min=-1,max=1)
        u[i]<-2*u-1
        w<-sum((u)^2)
    }
        # Define v and return the odd number of normal deviates
        v<-sqrt((-2*log(w))/w)
        z<-numeric(n)
        x<-0
        z[i]<-u[i]*v
        x[i]<-mean(u)+z[i]*sd(u)
    }
    return(z[i])
}



# Normality test of the produced variates
  pass.test<-function (x){
    while (length(x)>=3){
      if (shapiro.test(x)$p.value>0.05){
        cat("The deviates are normally distributed\n")
      } else {
        cat("The deviates do not follow a normal distribution\n")
      }
    }
  }

  
  
  
  
  # Transformation into chisq distributed deviates
  z<-numeric(n)
  my.rchisq<-function(n,df=1,ncp=0) {
    z<-((rnorm(n))^2)
    return(z)
  }

  # Function testing if the deviates are chisquared distributed
  ki.test<- function(z) {
    while(length(x)>=3) {
      if (chis.test(z)$p.value>0.05){
        cat ("The deviates are chisquared distributed\n")
      } else {
        ("The deviates are not chisquared distributed\n")
      }
    } 
  }

  


  
  
  
  # Transformation into pseudo-random t-distributed deviates
  t<-numeric(n)
  my.rt<-function(n,df=1,ncp=0) {
    t<- (rnorm(n)) / (sqrt(my.rchisq(n)/n))
    return(t)
  }
 
 # Test function of distributed t deviates
  kol.test<- function(x){
    if (ks.test(x)$p.value>0.05) {
      cat("The deviates follow a t-distribution\n")
    } else {
      cat("The deviates do not follow a t-distribution\n")
    }
  }
  
  