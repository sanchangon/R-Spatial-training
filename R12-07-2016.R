#-----------------------------------------------
# chingchai humhong
# 12/07/2016 - 13/07/2016
# chingchai.h@gmail.com
# GISTNU @ Naresuan University
# MIT@License
# github : https://github.com/sanchangon
#-----------------------------------------------
# set work directory my project
setwd("C:/Users/sanchangon/Documents/R_NU")
getwd()

# read csv data gistnu pop
read.csv("d2.csv", row.names=1) -> d
str(d)

# read csv pop data gistnu pop
read.csv("dataPop.csv",h=T,as.is = T) -> pop
dim(pop)
View(pop)
str(pop)
labels(pop$CODEY)
table(pop$CODEY)
addmargins(table(pop$CODEY))

# taaply for gistnu pop
tapply(pop$TOTALTOT, pop$CODEY, sum)
tapply(pop$MALETOT, pop$CODEY, sum) -> popM
tapply(pop$FEMALETOT, pop$CODEY, sum) ->  popF
popM+popF
addmargins(tapply(pop$TOTALTOT, pop$CODEY, length))
unique(pop$CODEY)
length(unique(pop$CODEY))
length(unique(pop$POV))

# snippet code enhanced gistnu pop
tapply(pop$TOTALTOT, list(pop$POV,pop$CODEY), sum)
addmargins(tapply(pop$TOTALTOT, list(pop$POV,pop$CODEY), sum),1) #view sum column
addmargins(tapply(pop$TOTALTOT, list(pop$POV,pop$CODEY), sum),2) #view sum row

# sum & create variables gistnu pop
pop$m0 <- pop$maleage0+pop$maleage1+pop$maleage2+pop$maleage3+pop$maleage4
pop$m5 <- pop$maleage5+pop$maleage6+pop$maleage7+pop$maleage8+pop$maleage9

# read text file pop example
read.table("popReg45_58.txt",h=T,as.is = T) -> p
dim(p)
View(p)
str(p)

# write txt to csv file
write.csv(p, file="pop45_48.csv")

# taaply pop example
tapply(p$pop, list(p$year,p$prov), sum)
addmargins(tapply(p$pop, list(p$year,p$prov), sum),2)
addmargins(tapply(p$pop, list(p$year,p$prov), sum),1)
addmargins(tapply(p$pop, list(p$year,p$ageGrp,p$prov), sum),1)

# summary data using tapply
tapply(p$pop, p$year, summary) # summary only one variable!! 
tapply(p$pop, p$ageGrp, summary) # summary only one variable!!


# Generating random number from a normal distribution - and +
n100 <- rnorm(100)
n100
n100 <- rnorm(100, mean=0, sd=1)
hist(n100) # view histogram of n100
mean(n100); sd(n100)

n1000 <- rnorm(1000, mean=0, sd=1)
hist(n1000)
mean(n1000); sd(n1000)
n1000 <- rnorm(1000, mean=25, sd=2.31)
hist(n1000) # view histogram of n1000
mean(n1000); sd(n1000)

# Generating random number from Poisson distribution 
rpois100 <- rpois(100, lambda=3) # lambda = average of poisson
rpois100
hist(rpois100)

# Generating random number from Binomial distribution
# สุ่มทำ 100 ครั้ง แต่ละครั้งทดลอง 20 ครั้ง แล้วดูว่า 20 ครั้ง สำเร็จกี่ครั้ง (0.2 = โอกาสที่จะสำเร็จ)
rbinom100 <- rbinom(100, 20, 0.2)
rbinom100

# Point probability for specific value of standard normal distribution
dnorm(-1.96); dnorm(-1.96,0,1)
dnorm(1.96); dnorm(1.96,0,1)

# Calculate the quantiles for the standard normal
qnorm(0.05) ; qnorm(0.05,0,1)
qnorm(0.025)
qnorm(0.975)
pnorm(2)-pnorm(1)

# Plotting the density function of normal distributionN(0,1)
x <- seq(-3,3,0.1)
plot(x,dnorm(x,0,1),type = "l",col="#FF0990",lwd = 2)
points(x[1],dnorm(x[1]),pch= 21,col="blue")
points(x[2],dnorm(x[2]),pch= 21,col="blue")
points(x[3],dnorm(x[3]),pch= 21,col="blue")
points(x[5],dnorm(x[5]),pch= 21,col="blue")
points(x[50],dnorm(x[50]),pch= 21,col="blue")
points(x,dnorm(x),pch=7 ,col="blue", cex=1)

# plot graph
plot(p$ageGrp, p$pop, col="blue", cex=1)
barplot(p$pop, p$ageGrp)

# Plotting the density function of binomial distribution
y <- 0:30
plot(y,dbinom(y, 30, 0.25),type = "h")

z <- 0:50
plot(z,dpois(z, 10),type = "h")

# boxplot
read.csv("d2.csv", row.names=1) -> d
boxplot(d$sbp,col="blue")
boxplot(d$sbp,col="red",horizontal = T)
boxplot(d$dbp,col="red",horizontal = T)

# component boxplot basic
# note: mtext(out box) and text(in box) to custumization plot
boxplot(d$dbp~d$sex,col="green", horizontal=F,las=1, main="Title: Sex~dbp", xlab="sex(m/f)", ylab="dbp(...)")
med <- tapply(d$dbp,d$sex, median)
table(med)
points(1,med[1], col="red",pch=16, cex=2)
points(2,med[2], col="red",pch=16, cex=2)
mtext(side=3,adj=0,line=0.5,"Box plot of DBP(Sex~dbp)", font=2)
mtext(side=1,adj=0.5,line=2.5,"Sex(m/f)" ,font=2)
#boxplot(d$dbp~d$sex,col="red",horizontal = T)

# Boxplot, Histogram
hist(d$wt, ylim=c(0,300), col = "yellow", xlab= "weight(Kgs)", las=1)
boxplot(d$wt, horizontal = T, col= "pink", add=T, at=250, boxwex=60)

hist(d$sbp, ylim=c(0,650), col = "yellow", xlab= "SBP")
boxplot(d$sbp, horizontal = T, col= "pink", add=T, at=500, boxwex=80)
mean(d$sbp)-> averageSBP
median(d$sbp) -> medSBP
points(medSBP,500,pch=15,col= "red",cex=2)



# Test Barplot 
# Define the cars vector with 5 values
cars <- c(1, 3, 6, 4, 9)

# Graph cars
barplot(cars)

# Read values from tab-delimited autos.dat 
autos_data <- read.table("autos.dat", header=T, sep="\t")
autos_data
str(autos_data)

# Graph cars with specified labels for axes.  Use blue 
# borders and diagnal lines in bars.
barplot(autos_data$cars, main="Cars", xlab="Days",  
        ylab="Total", names.arg=c("Mon","Tue","Wed","Thu","Fri"), 
        border="blue", density=c(10,20,30,40,50))

# Graph autos with adjacent bars using rainbow colors
barplot(as.matrix(autos_data), main="Autos", ylab= "Total",
        beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("Mon","Tue","Wed","Thu","Fri"), cex=0.6, 
       bty="n", fill=rainbow(5))

# Expand right side of clipping rect to make room for the legend
par(xpd=T, mar=par()$mar+c(0,0,0,4))

# Graph autos (transposing the matrix) using heat colors,  
# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
barplot(t(autos_data), main="Autos", ylab="Total", 
        col=heat.colors(3), space=0.1, cex.axis=0.8, las=1,
        names.arg=c("Mon","Tue","Wed","Thu","Fri"), cex=0.8) 

# Place the legend at (6,30) using heat colors
legend(6, 30, names(autos_data), cex=0.8, fill=heat.colors(3));

# Restore default clipping rect
par(mar=c(5, 4, 4, 2) + 0.1)
#text(4, 1, "chai",  adj = c(2,3))

# stem, qqnorm, qqline
stem(d$age)
qqnorm(d$age)
qqline(d$age,col="red",lwd="3")

# One sample t-test
#-------------------
# H0 : u=100
# H1 : u!= 100
#-------------------
t.test(d$wt, mu=100)
print("hello R")

#-------------------
# H0 : u= 62.5
# H1 : u!= 62.5
#-------------------
t.test(d$wt, mu=62.5)
t.test(d$wt, mu=62.5) -> ttest
ls(ttest)
ttest$p.value
ttest$estimate
ttest$statistic

# Concatenate numerical values in a string 
paste(c("Result of p.value:", ttest$p.value), collapse = " ")
capture.output(cat("Result of p.value:", ttest$p.value))
print("End")

# One sample paired t-test
before <- c(12,14,11,15,12,14,13,12,15,14)
after <- c(14,15,15,15,13,13,16,14,16,14)
t.test(after,before, paired=T,    data=cbind(before,after))
t.test(after,before, paired=T,    data=cbind(before,after))  -> pttest
ls(pttest)
pttest$p.value
pttest$statistic
pttest$conf.int

# Writing a simple for loop in R
print(paste("The year is", 2010))
print(paste("The year is", 2011))
print(paste("The year is", 2012))
print(paste("The year is", 2013))
print(paste("The year is", 2014))
print(paste("The year is", 2015))

for (year in c(2010,2011,2012,2013,2014,2015)){
  print(paste("The year is", year))
}

for (year in 2010:2015){
  print(paste("The year is", year))
}

for (i in 2010:2015){
  print(paste("The year is", i))
}

for (i in 1:10) {
  if (!i %% 2){
    next
  }
  print(i)
}

# two sample t-test
#read.csv("d2.csv", row.names=1) -> d
t.test(d$wt~d$sex,data=d,alternative = "less")  -> ttest
t.test(d$wt~d$sex,data=d,alternative = "less")
t.test(d$wt~d$sex,data=d,alternative = "greater")
ls(ttest)
ttest$p.value
by(d$wt, d$sex, summary)
tapply(d$wt, d$sex, summary)

# Analysis of Variance
pain = c(4, 5, 4, 3, 2, 4, 3, 4, 4, 6, 8, 4, 5, 4, 6, 5, 8, 6, 6, 7, 6, 6, 7, 5, 6, 5, 5)
drug = c(rep("A",9), rep("B",9), rep("C",9))
migraine = data.frame(pain,drug)
migraine
plot(pain ~ drug, data=migraine)
results = aov(pain ~ drug, data=migraine)
summary(results)
anova(results)

# Multiple comparisons
pairwise.t.test(pain, drug, p.adjust="bonferroni")
results = aov(pain ~ drug, data=migraine)
TukeyHSD(results, conf.level = 0.95) 

# Simple Linear Regression and Multiple Regression
#read.csv("d2.csv", row.names=1) -> d
getwd()
ls()
str(d)
cor(d$age,d$sbp) #correlatoin
lm(d$sbp~d$age) -> m1
summary(m1)

cor(d$sbp,d$dbp)
lm(d$sbp~d$dbp) -> m2
summary(m2)

lm.r=lm(d$sbp~d$dbp)
summary(lm.r)
coef(lm.r)
resid(lm.r)
fitted(lm.r)
layout(matrix(1:4,2,2))
layout(matrix(c(1,2,3,4),2,2)) #optional 4 graphs/page
plot(lm.r)

# Pie
par(mfrow=c(3,3),mar=c(1.0,2.0,1.5,1.0),mgp=c(1.1,0.2,0),oma=c(0,0,0,0),las=1,tcl=0.2,cex=0.7)
color <- topo.colors(10)
color <- color[c(1,5,8)]
color
b <- c(21.1, 48.4, 30.5)
b <- c(150, 332, 125)
lb <- c("15-24","25-34","35-49")
pct <- round(b/sum(b)*100,2)
pct
sum(pct)
lbAG <- paste(pct)
lbAG
lbAG <- paste(lbAG,"%",sep="") # ad % to labels
lbAG
pie(b,labels=lbAG, col=color, main="Pie Chart of Age group", clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

pie(b,labels = lbAG, col=color,main="Pie Chart of Age group",clockwise=90)
legend("bottomleft", lb, cex=0.7, fill=color, bty="n")

# Customize colors pie
n <- 12
pie(rep(1,n), col=rainbow(n), clockwise=90)
pie(rep(1,n),col=heat.colors(n), clockwise=90)
pie(rep(1,n),col=terrain.colors(n), clockwise=90)
pie(rep(1,n),col=topo.colors(n), clockwise=90)
pie(rep(1,n),col=cm.colors(n), clockwise=90)

# Barplot
max.temp <- c(33, 36, 34, 37, 35, 32, 34)
barplot(max.temp)
barplot(max.temp, main="Temperatures in a Week", xlab="Degree Celsius", ylab="Day", names.arg=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), col="darkred", horiz=TRUE, las= 1)

averageTemp <- c(33, 36, 34, 37, 35, 32, 34)
barplot(averageTemp)

barplot(averageTemp, main="Temperatures in a Week", xlab="Degree Celsius", ylab="", names.arg=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), col="darkred", horiz=TRUE, las= 1)
mtext(side=3,adj= -0.1,line=0.1,"Day",font=2)

# reverse day in a week
barplot(rev(averageTemp), main="Temperatures in a Week", xlab="Degree Celsius", ylab="", names.arg=rev(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")), col="darkred", horiz=TRUE, las= 1)
mtext(side=3,adj= -0.1,line=0.1,"Day",font=2)

averageTemp <- c(33, 36, 34, 37, 35, 32, 34)
windows(7,7)
par(mar=c(2.8,5.5,2.5,3),mgp=c(1.1,0.2,0),oma=c(0,0,0,0),las=1,tcl=0.2)
barplot(averageTemp, main="Temperatures in a Week",cex.axis = 0.8, cex.names=0.8, xlab="Degree Celsius", ylab="", names.arg=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), col="darkred", horiz=TRUE, las= 1)
mtext(side=3,adj= -0.1,line=0.1,"Day",font=1)




