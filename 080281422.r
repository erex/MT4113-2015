# I confirm that the attached is my own work where clearly indicated in the text.


my.rnorm <-function (n, mean=0, sd=1){
   
   #Purpose: This function is design to use the runif function to produce random deviates that conform 
   # to the standard noral distribution
   #Output:returns a 
   #Input: 
   # n: number of deviates to be produced
   
   
   if(missing(n)||is.numeric(n)==FALSE||n<1||(n-round(n))!=0)
     stop("invalid argument")
   
   if(is.numeric(mean)==FALSE)
     stop("invalid argument")
   
   if(is.numeric(sd)==FALSE||sd<1)
     stop("invalid argument")
   
   
   norm.deviates <- numeric(n)
   U <- numeric(n)
   X <- numeric(n)
   x.2 <- numeric(n)
   
   
 for (i in 1:n){
     
    repeat  {                # generate a pair of uniform deviates and transform it to normal so that the 
                              # sum of its square is less than or equal to 1 using Marsaglia and Bray algorithm
       U<- (((runif(n))*2)-1)
       
       W<- (sum(U^2))
       
       if(W <= 1) break
       
          }
     
     v <- sqrt((-2*log(W))/W)
     
     X <- U*v                    # vector containing pair of normally distributed variates
     
     X.2<- (X*sd) + mean
        
    norm.deviates[i]<- X.2
      }
   
    return(norm.deviates)      # returns a vector of n normal random variates
    }
    
     
  
#---------------------------------------------------------------------------------------------------------------------------  
  
  my.rchisq <- function(n, df=1) {
 
     
 #Purpose of function: This function is design to produce random deviates that conform to the Chi-square distribution
 #Output: a vector of Chi square random variates
 #Input:
 #n:a vector of n random Chi square random variates
 #df: degrees of freedom
    
   
       if(missing(n)||is.numeric(n)==FALSE||n<1||(n-round(n))!=0)
       stop("invalid argument")
   
       if(is.numeric(df)==FALSE||df<1)
       stop("invalid argument")
   
   
       chisq.deviates2 <- numeric(n)  # vector to hold random variates
       
       for (i in 1:n) {
       
           Z<- my.rnorm(df)^2             # using normal function to generate Chi square deviates
      
           chisq.deviates2[i]<- sum(Z) 
                       }
         
       return(chisq.deviates2)
  
                 }
  
#-----------------------------------------------------------------------------------------------------------------------------

 my.rt <- function(n,df) { 
   
 #Purpose of function: This function is design to produce random deviates that conform to the Student's t distribution
 #Output: a vector of Student's T random variables
 #Input:    
 #n : vector of n Student's T random variates
 #df: degrees pf freedom
  
   
       if(missing(n)||is.numeric(n)==FALSE||n<1||(n-round(n))!=0)
       stop("invalid argument")
   
       if(is.numeric(df)==FALSE||df<1)
       stop("invalid argument")
   
   
       t.deviates <- numeric(n)
       
       for(i in 1:n){
         
           A<- my.rchisq(n)
           B<- sqrt(my.rnorm(n)/df)
           t.deviates[i]<- A/B
         
                    }
       return(t.deviates)
    
 }
 
#----------------------------------------------------------------------------------------------------------------------------- 
 
 test.my.rnorm <- function(i) {
   
# Purpose: to test my.rnorm function
# Input: 
# Output: TRUE/FALSE
  
   
   pass.test <- character("3")
   
   norm.l<- (my.rnorm(10))
   pass.test[1]<- (length(norm.l)==10 & is.numeric(norm.l))
   
   norm.m<- (my.rnorm(5,50,9))                    
   pass.test[2]<- (length(norm.m)==5 & is.numeric(norm.m))
   
   norm.o<- (my.rnorm(15))
   pass.test[3]<- (length(norm.o)==15 & is.numeric(norm.o))
   
   return(pass.test)
   
   
   
 }
    
   
#----------------------------------------------------------------------------------------------------------------------------   

 test.my.rchisq <- function(i) {
   
   # Purpose: to test my.rchisq function
   # Input: 
   # Output: TRUE/FALSE
   
   pass.test2 <- character("3")
   
   
   l<- (my.rchisq(10,3))
   pass.test2[1]<- (length(l)==10 & is.numeric(l))
   
   m<- (my.rchisq(5))
   pass.test2[2]<- (length(m)==5 & is.numeric(m))
   
   o<- (my.rchisq(15))
   pass.test2[3]<- (length(o)==15 & is.numeric(o))
   
   return(pass.test2)
   
 }
 
#----------------------------------------------------------------------------------------------------------------------------- 
 
test.my.rt <- function(i) {
   
   # Purpose: to test my.rt function
   # Input: 
   # Output: TRUE/FALSE
   
   
   pass.test3 <- character("3")
   
   t.l<- (my.rt(10))
   pass.test3[1]<- (length(t.l)==10 & is.numeric(t.l))
   
   t.m<- (my.rt(5,3))
   pass.test3[2]<- (length(t.m)==5 & is.numeric(t.m))
   
   t.o<- (my.rt(15))
   pass.test3[3]<- (length(t.o)==15 & is.numeric(t.o))
   
   return(pass.test3)
   
 }     
 
 