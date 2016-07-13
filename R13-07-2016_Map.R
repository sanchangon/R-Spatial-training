# chingchai humhong
# 13/07/2016
# chingchai.h@gmail.com
# GISTNU @ Naresuan University
# MIT@License
# github : https://github.com/sanchangon
# set work directory my project
setwd("C:/Users/sanchangon/Documents/R_NU/rMap")
getwd()

# read.table("rxySubdistDPC9.xy",h=T,as.is=T) -> rxy
read.table("rxyworld.xy",h=T,as.is=T) -> rxy
dim(rxy)
head(rxy)
tail(rxy)
class(rxy)
str(rxy)

rxy$x <- as.numeric(rxy$x, na.rm = T)
rxy$y <- as.numeric(rxy$y, na.rm = T)
str(rxy)


# rxy <- subset(rxy,rxy$prov == 64)
# Plot map
rxy$x <- rxy$x/1000
rxy$y <- rxy$y/1000
rid <- unique(rxy[,1])
rid <- rid[order(rid)]
table(rid)

# plot thematic map
g <- 15
# and shift to fit legend
h <- -13

xmin <- min(rxy$x[is.finite(rxy$x)])-h
ymax <- max(rxy$y[is.finite(rxy$y)])+g
ymin <- min(rxy$y[is.finite(rxy$y)])-g
xmax <- max(rxy$x[is.finite(rxy$x)])+h
xmid <- (xmin+xmax)/2

windows(height=6,width=10)
# windows(6.5,10)
# layout(rbind(c(1,1,1,1),c(1,2,1,1),c(1,1,1,1)),respect=T,widths=c(0.3,2.2,3.9,3.9),heights=c(4.2,3.8,0.3))
par(mar=c(2.8,2.4,2.0,0.8),oma=c(0.1,0.4,0.2,0.2),las=1,tcl=0.3,mgp=c(1.1,0.06,0))   ### Good
plot(xmin,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")
mtext("UTM(km)-East",side=1,adj=0.5,cex=0.8,line=1)
mtext("UTM(km)-North",side=3,adj=-0.03,cex=0.8,line=0.3)
points(xmid,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")

# colour the districts modeled in our study with red (high), orange (average) or light blue (low)

i <- 0

# bolder tambon

for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  polygon(rj$x,rj$y,col="grey",)
  #lines(rj$x,rj$y,col="darkred", lwd=1)     # no line
}


# bolder line tambon

for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  # polygon(rj$x,rj$y,col="grey",)
  lines(rj$x,rj$y,col="brown", lwd=1)     # no line
}


# bolder province

for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  #  polygon(rj$x,rj$y,col="red")                # no polygon
  lines(rj$x,rj$y,col="red", lwd=2)         # bolder province
}

#---------------------------------------------------------------------------------------------------------
#------------------------#
#    plot thematic map   #
#------------------------#
windows(height=7.7,width=9)
layout(rbind(c(1,1,1,1),c(1,2,1,1),c(1,1,1,1)),respect=T,widths=c(0.3,2.2,3.9,3.9),heights=c(4.2,3.8,0.3))
par(mar=c(2.8,2.4,2.0,0.8),oma=c(0.1,0.4,0.2,0.2),las=1,tcl=0.3,mgp=c(1.1,0.06,0))   ### Good
### par(mfrow=c(1,1),mar=c(2.2,2,2.5,0.5),mgp=c(1.2,0.1,0),oma=c(0,0,0,0),las=1,tcl=0.2)  ### old
## par(mfrow=c(1,1),mar=c(2.2,2,2.5,0.5),mgp=c(1.2,0.1,0),oma=c(0,0,0,0),las=1,tcl=0.2)  ### old

plot(xmin,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")
mtext("UTM(km)-East",side=1,adj=0.5,cex=0.8,line=1)
mtext("UTM(km)-North",side=3,adj=-0.03,cex=0.8,line=0.3)

# ----------------------- other provinces-----------------------------#

read.table("b1.xy",h=T,as.is=T) -> rxy1
read.table("b1.txt",h=T,as.is=T) -> rdata
# select district boundaries for which data are availabale
distID <- rdata$plotID

rxy <- c(0,0,0,0)
for (j in distID) {
  r <- subset(rxy1,rxy1$distID==j)
  rxy <- rbind(rxy,r)
}
rxy <- subset(rxy,rxy$distID>0)

# Plot map
rdata$x <- rdata$x/1000 # convert UTM from metres to km
rdata$y <- rdata$y/1000
rdata$xtext <- rdata$xtext/1000
rdata$ytext <- rdata$ytext/1000
rxy$x <- rxy$x/1000
rxy$y <- rxy$y/1000
rid <- unique(rxy[,1])
rid <- rid[order(rid)]

g <- 15
# and shift to fit legend
h <- -13

xmin <- min(rxy$x[is.finite(rxy$x)])-h
ymax <- max(rxy$y[is.finite(rxy$y)])+g
ymin <- min(rxy$y[is.finite(rxy$y)])-g
xmax <- max(rxy$x[is.finite(rxy$x)])+h
xmid <- (xmin+xmax)/2

windows(height=7.7,width=9)
layout(rbind(c(1,1,1,1),c(1,2,1,1),c(1,1,1,1)),respect=T,widths=c(0.3,2.2,3.9,3.9),heights=c(4.2,3.8,0.3))
par(mar=c(2.8,2.4,2.0,0.8),oma=c(0.1,0.4,0.2,0.2),las=1,tcl=0.3,mgp=c(1.1,0.06,0))   ### Good
plot(xmin,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")
mtext("UTM(km)-East",side=1,adj=0.5,cex=0.8,line=1)
mtext("UTM(km)-North",side=3,adj=-0.03,cex=0.8,line=0.3)
i <- 0
for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  cj <- colour[i]
  polygon(rj$x,rj$y,col=cj,)
  # lines(rj$x,rj$y,col="black", lwd=2)     # no line
}



# --------------DHF Incidence ----------------#
read.table("rxy9n.txt",h=T, as.is=T,) -> rxy1
read.table("rxy9map.txt",h=T, as.is=T) -> rdata
# select district boundaries for which data are availabale
distID <- rdata$plotID
rxy <- c(0,0,0,0)
for (j in distID) {
  r <- subset(rxy1,rxy1$distID==j)
  rxy <- rbind(rxy,r)
}
rxy <- subset(rxy,rxy$distID>0)

abline(v=200+100*(1:9),col="dimgrey",lty="13")
abline(h=1600+100*(1:9),col="dimgrey",lty="13")
lab1 <- c(1600+100*(0:8))
lab2 <- c(200+100*(0:12))

# Plot map
rdata$x <- rdata$x/1000 # convert UTM from metres to km
rdata$y <- rdata$y/1000

rdata$xtext <- rdata$xtext/1000
rdata$ytext <- rdata$ytext/1000

rxy$x <- rxy$x/1000
rxy$y <- rxy$y/1000

rid <- unique(rxy[,1])
rid <- rid[order(rid)]

points(xmid,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")


# colour the districts modeled in our study with red (high), orange (average) or blue (low)   ??????????
colour <- colours$colour

high <- CILBsd>meanInc
low <- CIUBsd< meanInc
xlab <-c(1:47)
colour[xlab] <- "orange"
colour[xlab[high]] <- "red"
colour[xlab[low]] <- "blue"

i <- 0
for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  cj <- colour[i]
  polygon(rj$x,rj$y,col=cj)
  lines(rj$x,rj$y,col="snow") 
}

# Plot labelled circles indentifying regions (districts)
lab <- c(1:47)
x <- rdata$x
y <- rdata$y

y[28] <- y[28]+2
y[38] <- y[38]-8
y[43] <- y[43]-8
y[45] <- y[45]-5
x[39] <- x[39]+7

xcoord <- x
ycoord <- y
for (i in 1:47) {
  points(xcoord[i],ycoord[i],cex=3.0,pch=21,bg="snow")
  text(xcoord[i],ycoord[i],lab[i],cex=0.9)
}

# Labels for Provinces  Lao and Myanmar

text(760,1991,paste("Lao PDR"),cex=1.2, col="darkgrey")
text(410,1850,paste("Myanmar"),cex=1.2, col="darkgrey")
text(550,1810,"Kamphangphet",cex=1.0, col="darkgrey")
text(635,1810,"Phichit",cex=1.0, col="darkgrey")
text(625,1755,"Nakornsawan",cex=1.0, col="darkgrey")
text(475,1685,"Kan",cex=1.0, col="darkgrey")
text(472,1675,"chanaBuri",cex=1.0, col="darkgrey")
text(765,1920,"Loei",cex=1.0, col="darkgrey")
text(780,1790,"Chaiya",cex=1.0, col="darkgrey")
text(782,1770,"phum",cex=1.0, col="darkgrey")
text(780,1680,"Korat",cex=1.0, col="darkgrey")
text(720,1685,"LopBuri",cex=1.0, col="darkgrey")
text(680,2030,"Nan",cex=1.0, col="darkgrey")
text(600,1991,"Phrae",cex=1.0, col="darkgrey")
text(527,1960,"Lam",cex=1.0, col="darkgrey")
text(527,1940,"phang",cex=1.0, col="darkgrey")
text(495,2010,"Lam",cex=1.0, col="darkgrey")
text(495,1990,"phun",cex=1.0, col="darkgrey")
text(440,1980,"Chiangmai",cex=1.0, col="darkgrey")
text(390,2010,"Mae",cex=1.0, col="darkgrey")
text(390,1991,"hongsorn",cex=1.0, col="darkgrey")


axis(side=2,labels=lab1,at=lab1,cex.axis=1.0)
axis(side=1,labels=lab2,at=lab2,cex.axis=1.0)


distLegend <- rdata$name
distLegend <- paste(1:47,distLegend)
mtext(" Morbidity of dengue hemorrhagic fever in Public Health Area17th : 2001-2010",side=3,adj=0.5,cex=1.0,line=2)
box()


####### Line border of porvince ########

read.table("b1.xy",h=T,as.is=T) -> rxy1
read.table("b1.txt",h=T,as.is=T) -> rdata
# select district boundaries for which data are availabale
distID <- rdata$plotID

rxy <- c(0,0,0,0)
for (j in distID) {
  r <- subset(rxy1,rxy1$distID==j)
  rxy <- rbind(rxy,r)
}
rxy <- subset(rxy,rxy$distID>0)

# Plot map
rdata$x <- rdata$x/1000 # convert UTM from metres to km
rdata$y <- rdata$y/1000
rdata$xtext <- rdata$xtext/1000
rdata$ytext <- rdata$ytext/1000
rxy$x <- rxy$x/1000
rxy$y <- rxy$y/1000
rid <- unique(rxy[,1])
rid <- rid[order(rid)]

# plot thematic map
points(xmid,ymin,type="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),yaxp=c(600,2200,8),las=1,tcl=0.2,mgp=c(1.1,0.2,0),xlab="",ylab="",yaxs="i",cex.axis=0.9,yaxt="n",xaxt="n")

# colour the districts modeled in our study with red (high), orange (average) or light blue (low)
colour <- rdata$colour
i <- 0
for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  i <- i+1
  cj <- colour[i]
  # polygon(rj$x,rj$y,col=cj,)                # no polygon
  lines(rj$x,rj$y,col="brown", lwd=2)         # bolder province
}

legend(517,1743,c("  1-9      Uttaradit","10-18    Tak","19-27    Sukhothai","28-36    Phisanulok","37-47    Phetchabun"),cex=1.0,bg="bisque",inset=0.01,title =" Districts of Province")
legend(606.5,1720.5,c("Above Average","Average","Below average"),cex=1.0,fill=c("red","orange","blue"),bg="bisque",inset=0.01,title =" Incidence/100000",x.intersp=0.7)
box()

#################  END ####################
# Add picture on top 
# plot inset
xmin1 <- 375
xmax1 <- 448
ymin1 <- 1670
ymax1 <- 1831
polygon(c(xmin1,xmax1,xmax1,xmin1,xmin1),c(ymin1,ymin1,ymax1,ymax1,ymin1),col="white")

# function to recolour and merge regions
combineP <- function(indexPlots, colourNew) {
  n <- length(indexPlots) # number of plots to combine
  for (i in 1:n) {
    di <- subset(rxy,plotID==indexPlots[i])
    rid <- unique(di[,1])
    rj <- subset(di,di[,1]==rid)
    polygon(rj[,3:4],col=colourNew,border="black")
  }
}

# Ning's common boundary merge program (slow but easy to use)
mergeN <- function(indexPlots, colourP, typeP) {
  n <- length(indexPlots) # number of plots to merge
  for (k in 1:n) {
    for (p in k+1:n) {
      d1 <- subset(rxy,plotID==indexPlots[k])
      d2 <- subset(rxy,plotID==indexPlots[p])
      merge(d1,d2,by.x=c("x","y"),by.y=c("x","y")) -> d12
      # if border is not empty redraw it
      if (dim(d12)[1]>1) {
        d12[order(d12$pointID.x),c(4,1:2)] -> border
        # if border is not a single segment, insert a blank
        nBorderPoints <- dim(border)[1]
        if (nBorderPoints < border$pointID.x[nBorderPoints]) {
          for (i in 1:nBorderPoints) {
            if (border$pointID.x[i]>i) break
          }
          border <- rbind(border[1:(i-1),],c(NaN,NaN,NaN),border[(i:nBorderPoints),])
        }
        lines(border$x,border$y,lwd=1,col=colourP)
        lines(border$x,border$y,lwd=1,col=typeP)
      }
    }
  }
}

# get data

read.table("thai.xy",h=T,as.is=T) -> rxy
str(rxy)
read.table("thai.txt",h=T,as.is=T) -> rdata
str(rdata)

# convert UTM from metres to km
rdata$x <- rdata$x/1000
rdata$y <- rdata$y/1000
rxy$x <- rxy$x/1000
rxy$y <- rxy$y/1000

# specify window for map

xmin2 <- min(rxy[,3],na.rm=T)
xmin2 <- xmin2-30
xmax2 <- max(rxy[,3],na.rm=T)
xmax2 <- xmax2+30
ymin2 <- min(rxy[,4],na.rm=T)
ymin2 <- ymin2-30
ymax2 <- max(rxy[,4],na.rm=T)
ymax2 <- ymax2+30

box()
# plot inset

plot(xmin2,ymin2,type="n",xlim=c(xmin2,xmax2),ylim=c(ymin2,ymax2),xlab="",ylab="",xaxs="i",yaxs="i",xaxt="n",yaxt="n")

rid <- unique(rxy[,1])
rid <- rid[order(rid)]

for (j in rid) {
  rj <- subset(rxy,rxy[,1]==j)
  polygon(rj[,3:4],border="dimgrey",col="honeydew2")
}

polygon(c(350,350,350,820,820),c(1650,1650,2070,2070,1650),density=0,col="red")

# colour of five provinces
colour <- c("blue","red","tan3","yellow","pink")
c <- c(41,50,51,52,54)
for(j in 1:5) {
  cj <- colour[j]	
  Pj <- c[j]
  combineP(Pj,cj)
}

box(col="white")
# box()
text(980,2150,paste("Laos"),cex=0.8,col="orange")
text(1065,1480,paste("Cambodia"),cex=0.6,col="red")
text(450,2275,paste("Myanmar"),cex=0.6,col="blue")
text(900,610,paste("Malaysia"),cex=0.6,col="violet")
##########################################################



