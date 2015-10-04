#-----------------------------------------my.rnorm----------------------------------------------------------


my.rnorm<-function(n,mean=0,sd=1){
  #Purpose: return a vector of pseudo-random values from a normal distribution using the Marsaglia and Bray algorithm
  #Inputs: U1 and U2, where Ui~U(0,1),i=1,2.
  #Outputs: the gnerated normal distribution from the uniform distribution with n values.
  
  #combine the results of x
  x<-c()
  #start a for loop from 1 to ceiling(n/2)
  for(i in 1:ceiling(n/2)){
    if(n<=0|sd<0)stop(print("argument invalid")
    )else{
  #set the Ui~U(0,1),i=1,2
  U<-runif(2,min=0,max=1)
  #Now Ui~U(-1,1),i=1,2
  Ui<-U*2-1
  #define w
  w<-Ui[1]^2+Ui[2]^2
  #reject the values outside the unit circle(centred on 0, with radius 1)
  while(w>1){
    U<-runif(2,min=0,max=1)
    Ui<-U*2-1
    w<-Ui[1]^2+Ui[2]^2
  }#end of while loop
  #define v
    v<-sqrt(((-2)*log(w))/w)
  #define a pair of normally distrbuted variates Xi=(Ui*v)~N(0,1),i=1,2
    x<-append(x,Ui*v)
  #repeat until i=ceiling(n/2)
    i<-i+1  
    }
  }#end of for loop
  
  #print the answer if n is even number
   if(n%%2==0){
     Xi<-sd*x+mean
     print(Xi)
  #print the answer if n is odd number
   }else{
     x<-x[-n+1]
     Xi<-sd*x+mean
     print(Xi)
   }
     }#end of function



#--------------------------------------------------my.rchisq-----------------------------------------------------
my.rchisq<-function(n,df=1){
  #purpose: to return a vector of pseudo-random chi-squared distributed deviates.
  #input:Zi~N(0,1), i=1,...,n.
  #output: the generated chi-squared distribution with n values.
  if (n<0|df<0) stop (print(" arguments invalid")
  ) else{
  myChisq<-numeric()
  #Zi~N(0,1),i=1,...,n
  #Chi-squared~sum of Zi's
    for(i in 1:n){
      Z<-my.rnorm(df,0,1)
      myChisq[i]=sum(Z^2)
      i<-i+1
    }#end of for loop
    return(myChisq)
  }
}#end of function





#-----------------------------------------------------------my.rt--------------------------------------------------------------------
my.rt<-function(n,df=1){
  #puepose: to return a vector of pseudo-random t-distribution deviates.
  #input:Z~N(0,1)and U~Chi-squared distribution with k degrees of freedom, where Z and U are independent.
  #output: the generated student t-distribution deviates with n values.
  if (n<0|df<0) stop (print("arguments invalid")
  )else{
    t<-numeric()
  for(i in 1:n){
    #Z~N(0,1)
    #U~chi-squared distribution with k freedom
    #student t-distribution = Z/sqrt(U/k)
    Zt<-my.rnorm(n=1,mean=0,sd=1)
    Ut<-my.rchisq(1,df)
    t[i]=Zt/sqrt(Ut/df)
    i<-i+1
  }#end of for loop
  
  return(t)
  }
}#end of function





#--------------------------------------------test for my.rnorm--------------------------------------------------
#to test if the function my.rnorm meet the condition of the normal distribution
#Set n=10 to generate the result for the test
testmy.rnorm<-function(n){
x <- my.rnorm(n)
Rnorm<-rnorm(n,0,1)


#the normal distribution will giev 10 numeric answers so we check if the my.rnorm function meet this condition.
pass.test<-(length(x)==length(Rnorm)&is.numeric(x))
#If the function my.rnorm produces the same number of vector of n numbers as rnorm does, R shows TRUE for the pass.test, otherwise a False.
pass.test
par(mfrow=c(3,2))
hist(x,main="histogram of my.rnorm",probability = TRUE)
lines(density(x))
hist(Rnorm,main="histogram of rnorm",probability = TRUE)
lines(density(Rnorm))
boxplot(x,main="boxplot for my.rnorm",horizontal = TRUE)
boxplot(Rnorm,main="boxplot for rnorm",horizontal = TRUE)

qqnorm(x,main = "qq plot for my.rnorm")
qqnorm(Rnorm,main = "qq plot for rnorm")

#summarize both distributions to see the scale of the data.
summary(x)
summary(Rnorm)
}#end of function


#-------------------------------------------test for my.rchisq---------------------------------------------------------
#to test if the function my.rchiq meet the condition of the chi-squared distribution rchiq.
#we are going to use the plot to produce both rchisq and my.rchiq graph when n=10

testmy.rchisq<-function(n,df){
B<- my.rchisq(n,df)
y<-rchisq(n,df)
#summarize both distributions to see the scale of the data.
summary(B)
summary(y)
#create a matrix of 2 rows 1 ncols plots that are filled in by row.
par(mfrow=c(3,2))
#To compare the histogram boxplot and qq plot for my.rchisq and rchisq to see if the overall situation quite fit.
hist(B,main="histogram of my.rchisq",probability = TRUE)
lines(density(B))
hist(y,main="histogram of rchisq",probability = TRUE)
lines(density(y))
boxplot(B,main="boxplot for my.rchisq",horizontal = TRUE)
boxplot(y,main="boxplot for rchisq",horizontal = TRUE)

qqnorm(B,main = "qq plot for my.rchisq")
qqnorm(y,main = "qq plot for rchisq")



#Test if all vectors in my.rchisq is numeric
pass.test<-(is.numeric(B))
pass.test
}#end of function


#----------------------------------------------test for my.rt-----------------------------------------
#to test if the function my.rt meet the condition of the student-t.
#as the studnet t-distribution with 1:10 has the mean 5.5 so we test our my.rt to see if the mean meets the one with t.test.




testmy.rt<-function(n,df){
C<-rt(n,df)
A<-my.rt(n,df)
#summarize both distributions to see the scale of the data.
summary(A)
summary(C)
#Test if all vectors in my.rt is numeric
pass.test <- (is.numeric(A))
pass.test
par(mfrow=c(3,2))
#To compare the histogram boxplot and qq plot for my.rt and rt to see if the overall situation quite fit.
hist(A,main="histogram of my.rt",probability = TRUE)
lines(density(A))
hist(C,main="histogram of rt",probability = TRUE)
lines(density(C))
boxplot(A,main="boxplot for my.rt",horizontal = TRUE)
boxplot(C,main="boxplot for rt",horizontal = TRUE)

qqnorm(A,main = "qq plot for my.rt")
qqnorm(C,main = "qq plot for rt")


summary(A)
summary(C)


}#end of function


