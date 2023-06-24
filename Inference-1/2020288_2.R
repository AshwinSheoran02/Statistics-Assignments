rm(list=ls())
# read csv file

data = read.csv("data.csv")
data = data[,c(2)]

##########
#A

le.normal <- function(theta_param, data){
  mean <- theta_param[1]
  var <- theta_param[2]
  ll <- -(0.5)*length(data)*log(2*pi*var) - (0.5/var)*sum((data - mean)^2)
  return(-ll)
}

le.normal_pos <- function(theta_param, data){
  mean <- theta_param[1]
  var <- theta_param[2]
  ll <- -(0.5)*length(data)*log(2*pi*var) - (0.5/var)*sum((data - mean)^2)
  return(ll)
}

theta_param <- c(mean(data),var(data))
levalue.normal <- optim(theta_param,le.normal,data = data)

mean_data <- theta_param[1]
sigma2 <- theta_param[2]


cat( "Mean is " , mean_data )
cat ("variance is " , sigma2 )

##########
#B

le11 <- le.normal_pos(c(0,1) , data )
le12 <- le.normal_pos(c(500,1), data )
le13 <- le.normal_pos(c(1000,1), data )
le14 <- le.normal_pos(c(1500,1), data )
le15 <- le.normal_pos(c(2000,1), data )
le16 <- le.normal_pos(c(2500,1), data )
le17 <- le.normal_pos(c(3000,1), data )
le18 <- le.normal_pos(c(3500,1), data )
le19 <- le.normal_pos(c(4000.044,1), data )
le20 <- le.normal_pos(c(4500,1), data )
le21 <- le.normal_pos(c(5000,1), data )
le22 <- le.normal_pos(c(5500,1), data )
le23 <- le.normal_pos(c(6000,1), data )
le24 <- le.normal_pos(c(6500,1), data )



le <- c(le11 , le12 , le13 , le14 , le15 , le16 , le17 , le18  , le19 , le20 , le21 , le22 , le23 , le24)
xval <- c(0,500,1000,1500,2000,2500,3000, 3500,4000.043 , 4500 , 5000 , 5500, 6000 , 6500)
plot(xval , le ,type="o", ylab = "log-likelihood estimate", xlab = "Changing values of Mean")

