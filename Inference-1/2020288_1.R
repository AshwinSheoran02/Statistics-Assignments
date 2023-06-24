## Ans1
#a)
set.seed(123) # set seed for reproducibility
sample_size <- 1000 # specify the sample size

data1 = rexp(1000,1)
plot(data1, main="Exp dist , lambda =1", ylab="Y",xlab="X",)

data2 = rexp(1000,2)
plot(data2, main="Exp dist , lambda =2", ylab="Y",xlab="X",)

data3 = rexp(1000,3)
plot(data3, main="Exp dist , lambda =3", ylab="Y",xlab="X",)

data4 = rexp(1000,4)
plot(data4, main="Exp dist , lambda =4", ylab="Y",xlab="X",)




# b)

le.exp<-function(lambda,data){
  data_sum <- sum(data)
  val = 1000*(log(lambda)) - lambda*data_sum
  return(val)
}

le.exp_nlminb<-function(lambda,data){
  data_sum <- sum(data)
  val = 1000*(log(lambda)) - lambda*data_sum
  return((-1)*val)
}



## Lambda = 1
lambda_mom = 1/mean(data1)
cat("Parameter using Method of Moments (MOM) when lambda = 1 is " , lambda_mom)


mlle.exp <-nlminb(1,objective=le.exp_nlminb,hessian=T,lower=0,upper=1, data=data1)

# Print the MLE estimate 
cat("MLE estimate: for value-> 1 and lambda=1 is  ", (mlle.exp)$par, "\n")

mlle.exp <-nlminb(0.5,objective=le.exp_nlminb,hessian=T,lower=0,upper=1, data=data1)
cat("MLE estimate: for value-> 0.5 and lambda=1 is  ", (mlle.exp)$par, "\n" )

mlle.exp <-nlminb(lambda_mom,objective=le.exp_nlminb,hessian=T,lower=0,upper=1, data=data1)
cat("MLE estimate: for value-> MOM and lambda=1 is  ", (mlle.exp)$par, "\n")



#### Lambda = 2
lambda_mom = 1/mean(data2)
cat("Parameter using Method of Moments (MOM) when lambda = 2 is " , lambda_mom)

mlle.exp <-nlminb(1,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data2)

# Print the MLE estimate 
cat("MLE estimate: for value-> 1 and lambda=2 is  ", (mlle.exp)$par, "\n")

mlle.exp <-nlminb(0.5,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data2)
cat("MLE estimate: for value-> 0.5 and lambda=2 is  ", (mlle.exp)$par, "\n" )

mlle.exp <-nlminb(lambda_mom,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data2)
cat("MLE estimate: for value-> MOM and lambda=2 is  ", (mlle.exp)$par, "\n")


#### Lambda = 3
lambda_mom = 1/mean(data3)
cat("Parameter using Method of Moments (MOM) when lambda = 3 is " , lambda_mom)

mlle.exp <-nlminb(1,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data3)

# Print the MLE estimate 
cat("MLE estimate: for value-> 1 and lambda=3 is  ", (mlle.exp)$par, "\n")

mlle.exp <-nlminb(0.5,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data3)
cat("MLE estimate: for value-> 0.5 and lambda=3 is  ", (mlle.exp)$par, "\n" )

mlle.exp <-nlminb(lambda_mom,objective=le.exp_nlminb,hessian=T,lower=0,upper=4, data=data3)
cat("MLE estimate: for value-> MOM and lambda=3 is  ", (mlle.exp)$par, "\n")


#### Lambda = 4
lambda_mom = 1/mean(data4)
cat("Parameter using Method of Moments (MOM) when lambda = 4 is " , lambda_mom)

mlle.exp <-nlminb(1,objective=le.exp_nlminb,hessian=T,lower=0,upper=5, data=data4)

# Print the MLE estimate 
cat("MLE estimate: for value-> 1 and lambda=4 is  ", (mlle.exp)$par, "\n")

mlle.exp <-nlminb(0.5,objective=le.exp_nlminb,hessian=T,lower=0,upper=5, data=data4)
cat("MLE estimate: for value-> 0.5 and lambda=4 is  ", (mlle.exp)$par, "\n" )

mlle.exp <-nlminb(lambda_mom,objective=le.exp_nlminb,hessian=T,lower=0,upper=5, data=data4)
cat("MLE estimate: for value-> MOM and lambda=4 is  ", (mlle.exp)$par, "\n")


############
## C)

le11 <- le.exp(1,data1)
le12 <- le.exp(2,data1)
le13 <- le.exp(3,data1)
le14 <- le.exp(4,data1)
le15 <- le.exp(5,data1)
le16 <- le.exp(6,data1)
le17 <- le.exp(7,data1)
le18 <- le.exp(8,data1)
le19 <- le.exp(9,data1)


le_es <- c(le11 , le12 , le13 ,le14 , le15 , le16 , le17 , le18 , le19)
plot(le_es, type="o", col="blue", ylab="Log Likelihood estimate for data-1",xlab="Lambda " )



le21 <- le.exp(1,data2)
le22 <- le.exp(2,data2)
le23 <- le.exp(3,data2)
le24 <- le.exp(4,data2)
le25 <- le.exp(5,data2)
le26 <- le.exp(6,data2)
le27 <- le.exp(7,data2)
le28 <- le.exp(8,data2)
le29 <- le.exp(9,data2)


le_es <- c(le21 , le22 , le23 ,le24 , le25 , le26 , le27 , le28 , le29)
plot(le_es, type="o", col="blue",  ylab="Log Likelihood estimate for data-2",xlab="Lambda " )




le31 <- le.exp(1,data3)
le32 <- le.exp(2,data3)
le33 <- le.exp(3,data3)
le34 <- le.exp(4,data3)
le35 <- le.exp(5,data3)
le36 <- le.exp(6,data3)
le37 <- le.exp(7,data3)
le38 <- le.exp(8,data3)
le39 <- le.exp(9,data3)


le_es <- c(le31 , le32 , le33 ,le34 , le35 , le36 , le37 , le38 , le39)
plot(le_es, type="o", col="blue",  ylab="Log Likelihood estimate for data-3",xlab="Lambda " )


le41 <- le.exp(1,data4)
le42 <- le.exp(2,data4)
le43 <- le.exp(3,data4)
le44 <- le.exp(4,data4)
le45 <- le.exp(5,data4)
le46 <- le.exp(6,data4)
le47 <- le.exp(7,data4)
le48 <- le.exp(8,data4)
le49 <- le.exp(9,data4)


le_es <- c(le41 , le42 , le43 ,le44 , le45 , le46 , le47 , le48 , le49)
plot(le_es, type="o", col="blue",  ylab="Log Likelihood estimate for data-4",xlab="Lambda " )







