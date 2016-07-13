#-----------------------------------------------------------------------------------------
# chingchai humhong
# 12/07/2016 - 13/07/2016
# chingchai.h@gmail.com
# GISTNU @ Naresuan University
# MIT@License
# github : https://github.com/sanchangon
#-----------------------------------------------------------------------------------------
#Folow form อ.ดร.เอกพันธ์ ไกรจักร์
3+5
3*4^2
log(24)
x <- 10
x^2
x*x
my.height <- 171
My_STAT <- 4.00
my.id <- 57063039
weight <- c(60, 72, 57, 90, 95, 72)
weight * 2.2
height <- c(1.75, 1.80, 1.65, 1.90, 1.75, 1.91)
height
bmi <- weight/height^2
bmi
mean(weight)
sd(weight)
sum(weight)
mode(height)
score <- c(10,12,15,17,18)
sd(score)
plot(height, weight)
plot(height, weight, cex = 2)
plot(height, weight, cex = 2, col = "red")
plot(height, weight, cex = 2, col = "#FFBF00")
plot(height, weight, cex = 2, col = "red",pch = 20)
#-----------------------------------------------------------------------------------------
# set work directory my project
setwd("C:/Users/sanchangon/Documents/GitHub/R-Spatial-training")
getwd()
sample <- read.csv("iris.csv")
sample
head(sample)
tail(sample)
plot(Sepal.Length ~ Sepal.Width, data = sample,cex = 1,xlab = "Width(cm.)",ylab = "Length(cm.)",col = "blue",pch = 7)
plot(Petal.Length ~ Petal.Width, data = sample,cex = 1,xlab = "Width(cm.)",ylab = "Length(cm.)",col = "blue",pch = 5)
plot(Petal.Length ~ Petal.Width, data = sample,cex = 1,xlab = "Width(cm.)",ylab = "Length(cm.)",col = "red",pch = 5)


