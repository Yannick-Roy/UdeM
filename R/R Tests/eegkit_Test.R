library(eegkit)

##########   EXAMPLE 1   ##########
# plot 10-10 system (default):
# plot full cap 3d (NOT RUN)
# eegcap()
# plot full cap 2d (default options)
eegcap(type="2d")
# plot full cap 2d (different color for ears and nose)
data(eegcoord)
mycols <- rep("white",87)
enames <- rownames(eegcoord)
mycols[enames=="A1"] <- "green"
mycols[enames=="A2"] <- "light blue"
mycols[enames=="NZ"] <- "pink"
eegcap(type="2d",col.point=mycols)


##########   EXAMPLE 2   ##########
# plot 10-20 system:
# plot 3d cap with labels (NOT RUN)
# eegcap(electrodes="10-20")
# plot 3d cap without labels (NOT RUN)
# eegcap("10-20",plotlabels=FALSE)
# plot 2d cap with labels
eegcap("10-20","2d")
# plot 2d cap without labels
eegcap("10-20","2d",plotlabels=FALSE)


eegcapdense(type="2d")

##########   EXAMPLE 2   ##########
# plot 10-20 system:
# plot 3d cap with labels (NOT RUN)
# eegcapdense(electrodes="10-20")
# plot 2d cap without labels
eegcapdense("10-20","2d",plotlabels=FALSE)

##########   EXAMPLE 3   ##########
# plot custom subset of electrodes
myelectrodes <- c("FP1","FP2","FPZ","F7","F3","FZ",
                  "F4","F8","T7","C3","CZ","C4","T8",
                  "P7","P3","PZ","P4","P8","O1","O2")
eegcapdense(myelectrodes,"2d")


##########   EXAMPLE   ##########
data(eegcoord)
enames <- rownames(eegcoord)
# plot3d(eegcoord[,1],eegcoord[,2],eegcoord[,3],size=10,col="green")
# text3d(eegcoord[,1],eegcoord[,2],eegcoord[,3],texts=enames,col="blue")
plot(eegcoord[,4],eegcoord[,5],cex=2,col="green",pch=19)
text(eegcoord[,4],eegcoord[,5],labels=enames,col="blue")

##########   EXAMPLE   ##########
data(eegdense)
# plot3d(eegdense[,1],eegdense[,2],eegdense[,3],size=10,col="green")
plot(eegdense[,4],eegdense[,5],cex=1,col="green",pch=19)

##################################################################
####### 3D Mesh !
##################################################################
##########   EXAMPLE   ##########
data(eeghead)
shade3d(eeghead)
eeghead$material$color <- rep("black",length(eeghead$material$color))
wire3d(eeghead)

##################################################################
####### 
##################################################################
##########   EXAMPLE   ##########
# get "c" subjects of "eegdata" data
data(eegdata)
idx <- which(eegdata$group=="c")
eegdata <- eegdata[idx,]
# get average data (across subjects)
eegmean <- tapply(eegdata$voltage,list(eegdata$channel,eegdata$time),mean)
# remove ears and nose
acnames <- rownames(eegmean)
idx <- c(which(acnames=="X"),which(acnames=="Y"),which(acnames=="nd"))
eegmean <- eegmean[-idx,]
# get spatial coordinates (for plotting)
data(eegcoord)
cidx <- match(rownames(eegmean),rownames(eegcoord))
# temporal ICA with 4 components
icatime <- eegica(eegmean,4)
icatime$vafs
# quartz()
# par(mfrow=c(4,2))
# tseq <- (0:255)*1000/255
# for(j in 1:4){
#   par(mar=c(5.1,4.6,4.1,2.1))
#   sptitle <- bquote("VAF:  "*.(round(icatime$vafs[j],4)))
#   eegtime(tseq,icatime$S[,j],main=bquote("Component  "*.(j)),cex.main=1.5)
#   eegspace(eegcoord[cidx,4:5],icatime$M[,j],main=sptitle)
# }
# spatial ICA with 4 components
icaspace <- eegica(eegmean,4,type="space")
icaspace$vafs
# quartz()
# par(mfrow=c(4,2))
# tseq <- (0:255)*1000/255
# for(j in 1:4){
#   par(mar=c(5.1,4.6,4.1,2.1))
#   sptitle <- bquote("VAF:  "*.(round(icaspace$vafs[j],4)))
#   eegtime(tseq,icaspace$M[,j],main=bquote("Component  "*.(j)),cex.main=1.5)
#   eegspace(eegcoord[cidx,4:5],icaspace$S[,j],main=sptitle)
# }



##########   EXAMPLE   ##########
data(eegmesh)
wire3d(eegmesh)
eegmesh$material$color <- rep("red",length(eegmesh$material$color))
shade3d(eegmesh)




##########   EXAMPLE   ##########
## plot spatiotemporal component functions
data(eegcoord)
chnames <- rownames(eegcoord)
tseq <- seq(0,1,length.out=200)
quartz(width=18,height=6)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,11), 2, 6, byrow = TRUE))
eegspace(eegcoord[,4:5],p1s(chnames),cex.point=1,main=expression(psi[p1]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p1]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],p2s(chnames),cex.point=1,main=expression(psi[p2]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p2]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],p3s(chnames),cex.point=1,main=expression(psi[p3]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p3t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p3]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],n1s(chnames),cex.point=1,main=expression(psi[n1]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,n1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n1]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],n2s(chnames),cex.point=1,main=expression(psi[n2]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,n2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n2]),cex.main=2,
        xlab="Time After Stimulus (sec)")
plot(seq(-10,10),seq(-10,10),type="n",axes=FALSE,xlab="",ylab="")
text(0,8,labels=expression(omega[p1]*" = "*psi[p1]*tau[p1]),cex=2)
text(0,4,labels=expression(omega[n1]*" = "*psi[n1]*tau[n1]),cex=2)
text(0,0,labels=expression(omega[p2]*" = "*psi[p2]*tau[p2]),cex=2)
text(0,-4,labels=expression(omega[n2]*" = "*psi[n2]*tau[n2]),cex=2)
text(0,-8,labels=expression(omega[p3]*" = "*psi[p3]*tau[p3]),cex=2)
## plot simulated data at various time points
quartz(width=15,height=3)
tseq <- c(50,150,250,350,450)/1000
par(mfrow=c(1,5))
for(j in 1:5){
  eegspace(eegcoord[,4:5],eegsim(chnames,rep(tseq[j],87)),vlim=c(-6.8,5.5),
           main=paste(tseq[j]*1000," ms"),cex.main=2)
}


########## EXAMPLE ##########
data(eegmesh)
wire3d(eegmesh)
eegmesh$material$color <- rep("red",length(eegmesh$material$color))
shade3d(eegmesh)


########## EXAMPLE ##########
# get "c" subjects of "eegdata" data
data(eegdata)
idx <- which(eegdata$group=="c")
eegdata <- eegdata[idx,]
# get average data (across subjects)
eegmean <- tapply(eegdata$voltage,list(eegdata$channel,eegdata$time),mean)
# remove ears and nose
acnames <- rownames(eegmean)
idx <- c(which(acnames=="X"),which(acnames=="Y"),which(acnames=="nd"))
eegmean <- eegmean[-idx,]
# get spatial coordinates (for plotting)
data(eegcoord)
cidx <- match(rownames(eegmean),rownames(eegcoord))
# temporal ICA with 4 components
icatime <- eegica(eegmean,4)
icatime$vafs
quartz()
par(mfrow=c(4,2))
tseq <- (0:255)*1000/255
for(j in 1:4){
  par(mar=c(5.1,4.6,4.1,2.1))
  sptitle <- bquote("VAF: "*.(round(icatime$vafs[j],4)))
  eegtime(tseq,icatime$S[,j],main=bquote("Component "*.(j)),cex.main=1.5)
  eegspace(eegcoord[cidx,4:5],icatime$M[,j],main=sptitle)
}
# spatial ICA with 4 components
icaspace <- eegica(eegmean,4,type="space")
icaspace$vafs
quartz()
par(mfrow=c(4,2))
tseq <- (0:255)*1000/255
for(j in 1:4){
  par(mar=c(5.1,4.6,4.1,2.1))
  sptitle <- bquote("VAF: "*.(round(icaspace$vafs[j],4)))
  eegtime(tseq,icaspace$M[,j],main=bquote("Component "*.(j)),cex.main=1.5)
  eegspace(eegcoord[cidx,4:5],icaspace$S[,j],main=sptitle)
}

######### EXAMPLE ##########
## plot spatiotemporal component functions
data(eegcoord)
chnames <- rownames(eegcoord)
tseq <- seq(0,1,length.out=200)
quartz(width=18,height=6)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,11), 2, 6, byrow = TRUE))
eegspace(eegcoord[,4:5],p1s(chnames),cex.point=1,main=expression(psi[p1]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p1]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],p2s(chnames),cex.point=1,main=expression(psi[p2]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p2]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],p3s(chnames),cex.point=1,main=expression(psi[p3]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,p3t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[p3]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],n1s(chnames),cex.point=1,main=expression(psi[n1]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,n1t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n1]),cex.main=2,
        xlab="Time After Stimulus (sec)")
eegspace(eegcoord[,4:5],n2s(chnames),cex.point=1,main=expression(psi[n2]),cex.main=2,vlim=c(-3,9))
eegtime(tseq,n2t(tseq),ylim=c(-1,1),asp=1/2,main=expression(tau[n2]),cex.main=2,
        xlab="Time After Stimulus (sec)")
plot(seq(-10,10),seq(-10,10),type="n",axes=FALSE,xlab="",ylab="")
text(0,8,labels=expression(omega[p1]*" = "*psi[p1]*tau[p1]),cex=2)
text(0,4,labels=expression(omega[n1]*" = "*psi[n1]*tau[n1]),cex=2)
text(0,0,labels=expression(omega[p2]*" = "*psi[p2]*tau[p2]),cex=2)
text(0,-4,labels=expression(omega[n2]*" = "*psi[n2]*tau[n2]),cex=2)
text(0,-8,labels=expression(omega[p3]*" = "*psi[p3]*tau[p3]),cex=2)

## plot simulated data at various time points
quartz(width=15,height=3)
tseq <- c(50,150,250,350,450)/1000
par(mfrow=c(1,5))
for(j in 1:5){
  eegspace(eegcoord[,4:5],eegsim(chnames,rep(tseq[j],87)),vlim=c(-6.8,5.5),
           main=paste(tseq[j]*1000," ms"),cex.main=2)
}


########## EXAMPLE 1: Temporal ##########
# get "PZ" electrode of "c" subjects in "eegdata" data
data(eegdata)
idx <- which(eegdata$channel=="PZ" & eegdata$group=="c")
eegdata <- eegdata[idx,]
# temporal smoothing
eegmod <- eegsmooth(eegdata$voltage,time=eegdata$time)
# define data for prediction
time <- seq(min(eegdata$time),max(eegdata$time),length.out=100)
yhat <- predict(eegmod,newdata=time,se.fit=TRUE)
# plot results using eegtime
eegtime(time*1000/255,yhat$fit,voltageSE=yhat$se.fit,ylim=c(-4,4),main="Pz")


########## EXAMPLE 2: Spatial ##########
# get time point 65 (approx 250 ms) of "c" subjects in "eegdata" data
data(eegdata)
idx <- which(eegdata$time==65L & eegdata$group=="c")
eegdata <- eegdata[idx,]
# remove ears, nose, and reference (Cz)
idx <- c(which(eegdata$channel=="X"),which(eegdata$channel=="Y"),
         which(eegdata$channel=="nd"),which(eegdata$channel=="Cz"))
eegdata <- eegdata[-idx,]
# match to eeg coordinates
data(eegcoord)
cidx <- match(eegdata$channel,rownames(eegcoord))
# spatial smoothing
eegmod <- eegsmooth(eegdata$voltage,space=eegcoord[cidx,1:3])
# use dense cap for prediction
mycap <- levels(factor(eegdata$channel))
ix <- eegcapdense(mycap,type="2d",index=TRUE)
data(eegdense)
space <- eegdense[ix,1:3]
yhat <- predict(eegmod,newdata=space)
# plot results using eegspace
#eegspace(space,yhat)
eegspace(eegdense[ix,4:5],yhat)



######### EXAMPLE 3: Spatial-Temporal (not run) ##########
# get "c" subjects of "eegdata" data
data(eegdata)
idx <- which(eegdata$group=="c")
eegdata <- eegdata[idx,]
# remove ears, nose, and reference (Cz)
idx <- c(which(eegdata$channel=="X"),which(eegdata$channel=="Y"),
         which(eegdata$channel=="nd"),which(eegdata$channel=="Cz"))
eegdata <- eegdata[-idx,]
# match to eeg coordinates
data(eegcoord)
cidx <- match(eegdata$channel,rownames(eegcoord))
# spatial-temporal smoothing
eegmod <- eegsmooth(eegdata$voltage,space=eegcoord[cidx,1:3],time=eegdata$time)
# time main effect
newdata <- list(time=seq(min(eegdata$time),max(eegdata$time),length.out=100))
yhat <- predict(eegmod,newdata=newdata,se.fit=TRUE,include="time")
eegtime(newdata$time,yhat$fit,voltageSE=yhat$se.fit,ylim=c(-2,4),main="Time Main Effect")
# space main effect
mycap <- levels(factor(eegdata$channel))
ix <- eegcapdense(mycap,type="2d",index=TRUE)
data(eegdense)
newdata <- list(space=eegdense[ix,1:3])
yhat <- predict(eegmod,newdata=newdata,include="space")
eegspace(newdata$space,yhat)
# interaction effect (spatial map at time point 65)
newdata <- list(space=eegdense[ix,1:3],time=rep(65,nrow(eegdense[ix,])))
yhat <- predict(eegmod,newdata=newdata,include="space:time")
eegspace(newdata$space,yhat)
# full prediction (spatial map at time point 65)
newdata <- list(space=eegdense[ix,1:3],time=rep(65,nrow(eegdense[ix,])))
yhat <- predict(eegmod,newdata=newdata)
eegspace(newdata$space,yhat)




########## EXAMPLE ##########
# get time point 65 (approx 250 ms) from "eegdata" data
data(eegdata)
idx <- which(eegdata$time==65L)
eegdata <- eegdata[idx,]
# get average spatial map
eegmean <- tapply(eegdata$voltage,list(eegdata$channel,eegdata$group),mean)
# remove ears and nose
acnames <- rownames(eegmean)
idx <- c(which(acnames=="X"),which(acnames=="Y"),which(acnames=="nd"),which(acnames=="Cz"))
eegmean <- eegmean[-idx,]
# match to eeg coordinates
data(eegcoord)
cidx <- match(rownames(eegmean),rownames(eegcoord))
# plot average control voltage in 3d
open3d()
eegspace(eegcoord[cidx,1:3],eegmean[,2])
# plot average control voltage in 2d
eegspace(eegcoord[cidx,4:5],eegmean[,2])
# change 3d bar location and use play3d to rotate (not run)
open3d()
par3d(windowRect=c(0,0,600,600))
eegspace(eegcoord[cidx,1:3],eegmean[,2],barloc="frontleft")
play3d(spin3d(axis=c(0,0,1),rpm=5),duration=20)
# change 2d bar location
eegspace(eegcoord[cidx,4:5],eegmean[,2],barloc="left")



########## EXAMPLE ##########
# get "PZ" electrode from "eegdata" data
data(eegdata)
idx <- which(eegdata$channel=="PZ")
eegdata <- eegdata[idx,]
# get average and standard error (note se=sd/sqrt(n))
eegmean <- tapply(eegdata$voltage,list(eegdata$time,eegdata$group),mean)
eegse <- tapply(eegdata$voltage,list(eegdata$time,eegdata$group),sd)/sqrt(50)
# plot results with legend
tseq <- seq(0,1000,length.out=256)
eegtime(tseq,eegmean[,2],voltageSE=eegse[,2],ylim=c(-10,6),main="Pz")
eegtime(tseq,eegmean[,1],vlty=2,vcol="red",voltageSE=eegse[,1],scol="pink",add=TRUE)
legend("bottomright",c("controls","alcoholics"),lty=c(1,2),
       lwd=c(2,2),col=c("blue","red"),bty="n")


######### EXAMPLE ##########
# get control ("c") data from "eegdata" data
data(eegdata)
idx <- which(eegdata$group=="c")
eegdata <- eegdata[idx,]
# get average
eegmean <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),mean)
eegse <- tapply(eegdata$voltage,list(eegdata$time,eegdata$channel),sd)/sqrt(50)
# plot time course for all electrodes
quartz(height=15,width=15)
tseq <- seq(0,1000,length.out=256)
eegtimemc(tseq,eegmean,colnames(eegmean),ylim=c(-11,14),voltSE=eegse)
