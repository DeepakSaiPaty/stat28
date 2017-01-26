## ----defineNumber,error=FALSE,message=FALSE, echo=FALSE, warning=FALSE,results='hide'----
####################################
######Reset these for each lecture!
####################################
lectureTitle<-"Data Distributions" #title of the lectures. 
semester<-"Spring 2017, STAT 28" #define what semester gets printed
teacherClass<-"Guntuboyina \\& Purdom"  
####################################
########## Leave the rest alone
####################################
currentFile<-current_input() #get file name and grab number automatically from it
currentFile<-gsub(".Rnw","",currentFile)
library(stringr)
lectureNumber<-str_extract(pattern="[0-9]+",currentFile) #define what lecture number this is
temp<-str_replace(pattern="\\A[0]+",replacement="",lectureNumber)
if(temp!="") lectureNumber<-temp
library(knitr)
outdir<-paste(currentFile,"Output/",sep="")
#########Set defaults here for how code is shown, etc. For stat 28, probably default is show code (echo=TRUE)
#Note have turned off showing errors and warnings, so if want that for some R code need to turn back on.
figHeight<-6
figWidth<-6
doubleWidth<-"\\textwidth"
knitr::opts_chunk$set(fig.align="center", background="white",prompt=TRUE, dev='pdf',cache=FALSE,cache.path = outdir, error=FALSE,message=FALSE, echo=TRUE, warning=FALSE,results="markup", fig.width=figHeight,fig.height=figWidth, fig.path=outdir,dev="png",fig.env="center",out.width=".5\\textwidth",
tidy.opts=list(width.cutoff=50), tidy=TRUE,prompt=FALSE)
#options(continue=" ") #so don't get the '+' when code wraps, but still keep prompt (prompt=FALSE in knitr options turns off both)

## ----readInData----------------------------------------------------------
dataDir<-"../finalDataSets"
flightSF<-read.table(file.path(dataDir,"SFO.txt"),sep="\t",header=TRUE)
dim(flightSF)
names(flightSF)

## ----fivenumsummary------------------------------------------------------
summary(flightSF$DepDelay)

## ----headOfNAs-----------------------------------------------------------
naDepDf<-subset(flightSF,is.na(DepDelay))
head(naDepDf[,c("Carrier","DepDelay","DepTime","ArrTime","Cancelled")])
summary(naDepDf[,c("Carrier","DepDelay","DepTime","ArrTime","Cancelled")])

## ----hist----------------------------------------------------------------
hist(flightSF$DepDelay,main="Departure Delay", xlab="Time (in minutes)")

## ----histWNAs------------------------------------------------------------
flightSF$DepDelayWithCancel<-flightSF$DepDelay
flightSF$DepDelayWithCancel[is.na(flightSF$DepDelay)]<-1200
hist(flightSF$DepDelayWithCancel,xlab="Time (in minutes)", main="Departure delay, with cancellations=5000")

## ----histSmallBreaks,out.width=doubleWidth-------------------------------
par(mfrow=c(2,2))
hist(flightSF$DepDelay,main="Departure Delay, default breaks", xlab="Time (in minutes)")
hist(flightSF$DepDelay,main="Departure Delay, breaks=100", xlab="Time (in minutes)",breaks=100)
hist(flightSF$DepDelay,main="Departure Delay, breaks=1000", xlab="Time (in minutes)",breaks=1000)

## ----boxplot-------------------------------------------------------------
boxplot(flightSF$DepDelay,main="Departure Delay", ylab="Time (in minutes)")

## ----boxplotByCarrier_noOutlier------------------------------------------
boxplot(flightSF$DepDelay~flightSF$Carrier,
	main="Departure Delay, by airline carrier", 
	ylab="Time (in minutes)")

## ----boxplotByCarrier----------------------------------------------------
boxplot(flightSF$DepDelay~flightSF$Carrier,
	main="Departure Delay, by airline carrier", 
	ylab="Time (in minutes)",outline=FALSE)

## ----transformFunctions--------------------------------------------------
ylim<-c(-3,3)
curve(log,0,10,ylim=ylim,ylab="Transformed",xlab="Original")
curve(sqrt,0,10,add=TRUE,col="red")
legend("bottomright",c("log","sqrt"),fill=c("black","red"))

## ----transformFakeData,fig.width=figWidth*2,out.width=doubleWidth--------
y<-rgamma(1000,scale=1,shape=4)
par(mfrow=c(1,2))
hist(y,main="Original data",xlab="original scale",breaks=30)
hist(log(y),main="Log of data",xlab="log-scale",breaks=30)

## ----TransformedBoxplotByCarrier,fig.height=figHeight*3,fig.width=figWidth*3,out.width=doubleWidth----
addValue<-abs(min(flightSF$DepDelay,na.rm=TRUE))+1
par(mfrow=c(3,1))
boxplot(flightSF$DepDelay+addValue~flightSF$Carrier,main="Departure Delay, original", ylab="Time")
boxplot(log(flightSF$DepDelay+addValue)~flightSF$Carrier,main="Departure Delay, log transformed", ylab=paste("log(Time+",addValue,")"))
boxplot(sqrt(flightSF$DepDelay+addValue)~flightSF$Carrier,main="Departure Delay, sqrt-transformed", ylab=paste("sqrt(Time+",addValue,")"))


## ----makeNonCancelledDf--------------------------------------------------
flightSF_nc<-subset(flightSF,flightSF$Cancelled!=1)

## ----histBinProbVsDensity, echo=FALSE------------------------------------
histBinProb<-function(x,ylab="Bin Probabilities",breaks = "Sturges",...){
	#plot=FALSE: doesn't plot, just returns the calculated values
    h<-hist(x,plot=FALSE,breaks=breaks) 
    h$counts=h$counts/sum(h$counts)
    plot(h,ylab=ylab,...)
}
par(mfrow=c(1,2),fig.width=figWidth*2,out.width=doubleWidth)
ylim<-c(0,0.6)
histBinProb(flightSF$DepDelay,main="Departure Delay, Probability of Bin", xlab="Time (in minutes)",ylim=ylim)
histBinProb(flightSF$DepDelay,main="Departure Delay, Probability of Bin, more breaks", xlab="Time (in minutes)",breaks=10000,ylim=ylim)
hist(flightSF$DepDelay,main="Departure Delay, Density", xlab="Time (in minutes)",freq=FALSE,ylim=ylim)
hist(flightSF$DepDelay,main="Departure Delay, Density, more breaks", xlab="Time (in minutes)",breaks=10000,freq=FALSE,ylim=ylim)

## ----flightSRS-----------------------------------------------------------
flightSRS<-sample(x=flightSF_nc$DepDelay, size= 100, replace=TRUE) #notice replace=TRUE gives SRS

## ----flightSRSHist-------------------------------------------------------
ylim<-c(0,1)
breaks<-seq(min(flightSF_nc$DepDelay),max(flightSF_nc$DepDelay),length=10)
histBinProb(flightSRS,main="Departure Delay", xlab="Time (in minutes)",border=NA,breaks=breaks,ylim=ylim,col="red",add=FALSE)
histBinProb(flightSF_nc$DepDelay,main="Departure Delay", xlab="Time (in minutes)",col=NULL,border="black",breaks=breaks,ylim=ylim,lwd=2,add=TRUE,lwd=3)
legend("topright",c("SRS","Truth"),fill=c("red","white"))

## ----flightSRSHistSmallBreaks--------------------------------------------
ylim<-c(0,0.6)
breaks<-seq(min(flightSF_nc$DepDelay),max(flightSF_nc$DepDelay),length=100)
histBinProb(flightSRS,main="Departure Delay", xlab="Time (in minutes)",border=NA,breaks=breaks,ylim=ylim,col="red",add=FALSE)
histBinProb(flightSF_nc$DepDelay,main="Departure Delay", xlab="Time (in minutes)",col=NULL,border="black",breaks=breaks,ylim=ylim,lwd=2,add=TRUE,lwd=3)
legend("topright",c("SRS","Truth"),fill=c("red","white"))

## ----compareHist,out.width=doubleWidth-----------------------------------
flightSFOSRS<-read.table(file.path(dataDir,"SFO_SRS.txt"),
                         sep="\t",header=TRUE, stringsAsFactors = FALSE)
flightSFOStratified<-read.table(file.path(dataDir,"SFO_Stratified.txt"),
                                sep="\t",header=TRUE, stringsAsFactors = FALSE)
par(mfrow=c(2,2))
xlim<-c(-20,400)
hist(flightSF$DepDelay,breaks=100,xlim=xlim,freq=FALSE)
hist(flightSFOSRS$DepDelay,breaks=100,xlim=xlim,freq=FALSE)
hist(flightSFOStratified$DepDelay,breaks=100,xlim=xlim,freq=FALSE)

## ----toyPdf--------------------------------------------------------------
curve(x*exp(-x/2)/4,xlim=c(0,30),ylab="p(x)",xlab="x")

## ----toyPdfInterval------------------------------------------------------
plotUnderCurve<-function(x1, x2,p,...){
####Don't worry too much about understanding this code!
####It just shades in under the curve
    x=seq(x1,x2,len=100) 
	y=p(x)
	polygon(c(x,tail(x,1),x[1]), c(y, 0,0), ...)
}
p<-function(x){x*exp(-x/2)/4} #define as function
curve(p,xlim=c(0,30),ylab="p(x)",xlab="x")
plotUnderCurve(5,10,p,col="red")

## ----distMean------------------------------------------------------------
sampleSize=1000
sampleMean<-replicate(n=10000,expr=mean(sample(flightSF_nc$DepDelay,size=sampleSize,replace=TRUE)))
hist(sampleMean,xlab="Mean Departure Delay",main=paste("Mean of SRS of size",sampleSize))

## ----distMeanWCurve,echo=FALSE-------------------------------------------
hist(sampleMean,xlab="Mean Departure Delay",main=paste("Mean of SRS of size",sampleSize),freq=FALSE)
m<-mean(flightSF_nc$DepDelay)
s<-sqrt(var(flightSF_nc$DepDelay)/sampleSize)
p<-function(x){dnorm(x, mean=m,sd=s)}
curve(p,add=TRUE,col="red",lwd=3)

## ----unifPdf,echo=FALSE--------------------------------------------------
#This is the 'Uniform' distribution
curve(dunif,xlim=c(-1,2),ylab="p(x)",xlab="x") 

## ----unifPdfSmall,echo=FALSE---------------------------------------------
xvalues<-seq(-1,2,length=100)
plot(xvalues,dunif(xvalues,min=1/4,max=1/2),type="l",xlim=c(-1,2),ylab="p(x)",xlab="x")

## ----distMeanWCurveRedo--------------------------------------------------
hist(sampleMean,xlab="Mean Departure Delay",main=paste("Mean of SRS of size",sampleSize),freq=FALSE)
m<-mean(flightSF_nc$DepDelay)
s<-sqrt(var(flightSF_nc$DepDelay)/sampleSize)
p<-function(x){dnorm(x, mean=m,sd=s)}
curve(p,add=TRUE,col="red",lwd=3)

## ----histDensityMinutesHours,out.width=doubleWidth,fig.width=2*figWidth,echo=FALSE----
par(mfrow=c(1,2))
hist(flightSF$DepDelay,main="Departure Delay, minutes", xlab="Time (in minutes)",freq=FALSE,ylim=ylim)
hist(flightSF$DepDelay/60,main="Departure Delay, hours", xlab="Time (in in hours)",freq=FALSE,ylim=ylim)

## ----histDensityBreaks,out.width=doubleWidth,fig.width=2*figWidth,echo=FALSE----
par(mfrow=c(1,2))
ylim<-c(0,0.8)
hist(flightSF$DepDelay,main="Departure Delay, default breaks", xlab="Time (in minutes)",freq=FALSE,ylim=ylim)
hist(flightSF$DepDelay,main="Departure Delay, 10,000 breaks", xlab="Time (in minutes)",breaks=10000,freq=FALSE,ylim=ylim)

## ----distMeanWCurveBreaks------------------------------------------------
m<-mean(flightSF_nc$DepDelay)
s<-sqrt(var(flightSF_nc$DepDelay)/sampleSize)
p<-function(x){dnorm(x, mean=m,sd=s)}
par(mfrow=c(2,2))
hist(sampleMean,xlab="Mean Departure Delay",main=expression(paste(bar(X),", default breaks")),freq=FALSE)
curve(p,add=TRUE,col="red",lwd=3)
hist(sampleMean,xlab="Mean Departure Delay",main=expression(paste(bar(X),", 10 breaks")),breaks=10,freq=FALSE)
curve(p,add=TRUE,col="red",lwd=3)
hist(sampleMean,xlab="Mean Departure Delay",main=expression(paste(bar(X),", 1000 breaks")),freq=FALSE,breaks=1000)
curve(p,add=TRUE,col="red",lwd=3)
hist(sampleMean,xlab="Mean Departure Delay",main=expression(paste(bar(X),", 10000 breaks")),freq=FALSE,breaks=10000)
curve(p,add=TRUE,col="red",lwd=3)

## ----densityExamples,out.width=doubleWidth-------------------------------
par(mfrow=c(2,2))
f<-function(x){dgamma(x,shape=5,scale=1)}
curve(f,from=0,to=20,ylab="p(x)",main="Gamma(5,1) distribution")
f<-function(x){dgamma(x,shape=1,scale=5)}
curve(f,from=0,to=20,ylab="p(x)",main="Gamma(1,5) distribution")
f<-function(x){dbeta(x,.5,.5)}
curve(f,from=0,to=1,ylab="p(x)",main="Beta(.5,.5) distribution")
f<-function(x){dbeta(x,2,5)}
curve(f,from=0,to=1,ylab="p(x)",main="Beta(2,5) distribution")

## ----notdensityExamples,echo=FALSE---------------------------------------
par(mfrow=c(1,2))
f<-function(x){x^3}
curve(f,from=-10,to=10,ylab="p(x)",main=expression(x^3))
f<-function(x){x^2}
curve(f,from=-10,to=10,ylab="p(x)",main=expression(x^2))

## ----densityExponential,echo=FALSE---------------------------------------
curve(dexp,from=0,to=10,ylab="p(x)",main="Exponential distribution")

## ----histSRS,echo=FALSE--------------------------------------------------
binWidth<-20
breaks<-seq(min(flightSRS),max(flightSRS)+binWidth,by=binWidth)
xlim<-range(breaks)
histBin<-hist(flightSRS,xlab="Departure Delay (in Minutes)",breaks=breaks,freq=TRUE,xlim=xlim)
ylim<-range(histBin$density)

## ----stepHist,echo=FALSE-------------------------------------------------
p<-stepfun(histBin$breaks,c(0,histBin$density,0),right=TRUE)
plot(p,do.points=FALSE,xlab="Departure Delay (in Minutes)",main="pdf estimated by histogram",verticals=FALSE,xlim=xlim,ylim=ylim)
x<-15
mtext(side=1,text="x",at=x)
points(x=x,y=p(x),pch=19)
text(x=x,y=p(x),labels=expression(paste(hat(p),"(x)")),pos=3)

## ----rectKernelDensity,echo=FALSE----------------------------------------
plot(density(flightSRS,bw=binWidth/sqrt(12),kernel="rectangular"))
lines(p,do.points=FALSE,xlab="Departure Delay (in Minutes)",verticals=FALSE,ylim=ylim,xlim=xlim)

## ----plotRectKernelShapes, echo=FALSE------------------------------------
plot(stepfun(c(-1,1),c(0,1,0)),do.points=FALSE,xaxt="n",yaxt="n",ylab="",xlab="",main="",bty="l",xlim=c(-2,2))
mtext(c("x-w/2","x","x-w/2"),side=1,at=c(-1,0,1))
#mtext(expression(frac(1,w)),side=2,at=1,las=2,line=.5)

## ----plotGaussianKernelShapes, echo=FALSE--------------------------------
curve(dnorm,xaxt="n",yaxt="n",ylab="",xlab="",main="",bty="l",xlim=c(-2,2))
mtext(c("x"),side=1,at=c(0))
segments(x0=0,x1=1,y0=dnorm(1),y1=dnorm(1))
text(label=expression(w/2),x=0.5,y=dnorm(1),pos=1)


## ----gaussianKernelDensity-----------------------------------------------
plot(density(flightSRS,kernel="gaussian"),
	main="Density estimate, both kernels")
lines(density(flightSRS,kernel="rectangular"),col="red")
lines(p,do.points=FALSE,xlab="Departure Delay (in Minutes)",verticals=FALSE,ylim=ylim,xlim=xlim,col="blue")
legend("topright",c("Normal","Rectangle","Histogram"),fill=c("black","red","blue"))

## ----bandwidthKernelDensity,echo=FALSE,out.width=doubleWidth-------------
par(mfrow=c(2,2),out.width=doubleWidth)
ylim<-c(0,0.08)
for(adjust in c(1,0.5,2,10)){
  plot(density(flightSRS,kernel="gaussian",adjust=adjust),main="Density estimate, different bandwidths",
  sub=paste("Bandwith multiplied by",adjust),ylim=ylim,xlim=xlim)
lines(density(flightSRS,kernel="rectangular",adjust=adjust),col="red")
lines(p,do.points=FALSE,xlab="Departure Delay (in Minutes)",verticals=FALSE,col="blue")
legend("topright",c("Normal","Rectangle","Histogram"),fill=c("black","red","blue"))
}



## ----densityMultipleGroups-----------------------------------------------
perGroupDensity<-tapply(flightSF_nc$DepDelay,flightSF_nc$Carrier,density)
ylim<-range(sapply(perGroupDensity,function(x){range(x$y)}))
par(mfrow=c(1,1))
plot(density(flightSF_nc$DepDelay),main="All non-cancelled flight, by carrier",sub=paste("Bandwith multiplied by",adjust),lwd=2,lty=2,xlim=c(-20,50),ylim=ylim)
cols<-rainbow(length(perGroupDensity))
nullOut<-mapply(perGroupDensity,cols,FUN=function(x,col){lines(x,col=col)})


## ----singleViolinPlot----------------------------------------------------
# have to install the package 'vioplot'
library(vioplot)
vioplot(flightSF_nc$DepDelay)

## ----multiViolinPlot-----------------------------------------------------
source("http://www.stat.berkeley.edu/~epurdom/RcodeForClasses/myvioplot.R")
vioplot2(flightSF_nc$DepDelay,flightSF_nc$Carrier,col=palette())

