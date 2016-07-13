# Regression Naratip page 91-92 
#-----------------------------------------------
# chingchai humhong
# 12/07/2016 - 13/07/2016
# chingchai.h@gmail.com
# GISTNU @ Naresuan University
# MIT@License
# github : https://github.com/sanchangon
#-----------------------------------------------
# set work directory my project
setwd("C:/Users/sanchangon/Documents/R_NU/regression")
getwd()
read.table("examreg.txt",h=T) -> m
m
attach(m)     # attach the data frame
cor(m)[,1:3]
cor(m$x,m$z)
cor(m$x,m$y)

#reg.lm <- lm(y~x)
reg.lm <- lm(m$y~m$x)
attributes(reg.lm)
ls(reg.lm)
summary(reg.lm)
anova(reg.lm)
reg.lm$fitted.values
m$y

#tcrit <- qt(0.975,df)   #  qt(0.975,n-2) 

newdata <- data.frame(x = 158)  
predict(reg.lm,newdata,interval="prediction", se.fit=TRUE)
plot(m$x,m$y, xlim=c(150,190), ylim=c(50,100),pch=16, las = 1)
lines(m$x,reg.lm$fitted.values, col= "red")

detach(m)      # clean up 

##############################################################################################

# Regression Naratip page 91-92 
mean <- 
  CIU
CIL

setwd("C:/Users/sanchangon/Documents/R_NU/regression")
getwd()
read.table("examreg.txt",h=T) -> m1

#attach(m1)     # attach the data frame 
cor(m1)[,1:3]
cor(m1$x,m1$z)
reg.lm1 <- lm(m1$y~m1$x+m1$z)
attributes(reg.lm1)
summary(reg.lm1)
anova(reg.lm1)
reg.lm1$fitted.values
step(reg.lm1)
step(lm(m1$y~m1$z+m1$x))
summary(step(lm(m1$y~m1$z+m1$x)))

newdata <- data.frame(x=158, z =23)  
predict(reg.lm1,newdata,interval="prediction", se.fit=TRUE)
plot(m1$x,m1$y, xlim=c(150,190), ylim=c(50,100),pch=16, las = 1)
lines(m1$x,reg.lm1$fitted.values, col= "red")



#### page 179-180 Aj. Narathip
x1 <- seq(min(x),max(x),length = 30)
z1 <- seq(min(z),max(z),length = 30)
f <- function(x1,x2) { r <- -0.294+0.037*x1+0.913*x2}
x1
y1 <- outer(x1,z1,f)
y1[is.na(y1)] <- 1
persp(x1,z1,y1, theta=30, phi=30, expand=0.5, col = "red", ticktype="detailed", xlab="x1", ylab="y1",zlab="z1")
title(main="y1 = -24.47415+0.62746*x1-0.09390*z1")

### plot 3D
library(rgl)
plot3d(x1,y1,z1, col="red", size=3)


##############################################################################################
detach(m1)      # clean up 
m1$y-reg.lm1$fitted.values
reg.lm1$residuals
