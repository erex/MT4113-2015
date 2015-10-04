#The first function to form pseudo-random normal deviates
my.rnorm <- function(n, mean=0, sd=1){

#Purpose:Call this function to get normal r.v.s formed by uniform r.v.s
#Input:number of r.v.s need to be get
#Output:normal r.v.s formed by uniform r.v.s

my.runif <- function(n){

#Purpose:Call this function to form suitable pairs of uniform deviates
#Input:number of r.v.s need to be get
#Output:suitable pairs of uniform deviates

a <- runif(1,-1,1)
b <- runif(1,-1,1)
w <- a^2+b^2

while(w>1) {
w <-a^2+runif(1,-1,1)^2
}
output <- list(a = a, w = w)
return(output)
}

l<-replicate(n, my.runif(n))
m<-matrix(l,nrow=2)
#Pick up elements for final calculations
A<-m[1,1:n]
W<-m[2,1:n]
a<-unlist(A)
w<-unlist(W)

v <- sqrt((-2*log(w))/w)

for(i in 1:n) {
   x <- a*v
}
return(x)
}




#The second function to form pseudo-random chisquare deviates
my.rchisq <- function(n, df=1){

#Purpose:Call this function to get pseudo-random chisquare deviates
#Input:number of pseudo-random deviates need to be get
#Output:chisquare distributed pseudo-random deviates obtained

z <- my.rnorm(n,mean=0, sd=1)
for (i in 1:n){
    u<-z^2
}
return(u)
}



#The third function to form pseudo-random student's t deviates
my.rt <- function(n, df=1){

#Purpose:Call this function to get Student't distributed pseudo-random deviates
#Input:number of pseudo-random deviates need to be get
#Output:Student's t distributed pseudo-random deviates obtained

z <- my.rnorm(n, mean-0, sd=1)
u <- my.rchisq(n, df=1)
for (i in 1:n){
    t = z/sqrt(u)
}
return(t)
}





#First method for test (functional)
pass.test <- function(n){
s <- replicate(n,shapiro.test(my.rnorm(1000)))
l <- matrix(s, nrow=4)
t <- l[2,1:n]
m <- mean(unlist(t))
#The significance level is set to be 5% generally
if(m>0.05){
   x<- 1
}else{
   x<- 0
}
return(x)
}
# pass.test(n) returns 1 if the mean of p-value is over 0.05, namely, samples 
#from my.rnorm(n, mean=0, sd=1) are standard normally distributed. Then, samples from 
#my.rchisq(n, df=1) are chisquare distributed and samples from my.rt(n, df=1) are Student's t
#distributed.






#Second method for test (non-functional)
s <- replicate(50,shapiro.test(my.rnorm(1000)))
l <- matrix(s, nrow=4)
#Pick up p-values
t <- l[2,1:50] 
m <- mean(unlist(t))
#The significance level is set to be 5% generally
past.test <- (m>0.05) 
# past.test takes on the value TRUE if the mean of p-value is over 0.05, namely, samples 
#from my.rnorm(n, mean=0, sd=1) are standard normally distributed. Then, samples from 
#my.rchisq(n, df=1) are chisquare distributed and samples from my.rt(n, df=1) are Student's t
#distributed.
