####################################################################
## Source : https://cran.r-project.org/web/packages/ERP/ERP.pdf
####################################################################

library(ERP)
### simulated ERPs for 20 subjects (rows) and 251 ERPs measured at
### every 4 milliseconds plus a behavior score (columns)
data(simerp)

### Plot raw ERP curves
erpplot(simerp[,1:251],frames=seq(0,1001,4),xlab="Time (ms)", ylab=expression(ERP),col="black",main="Simulated ERP")

### Test of averaged ERPs over a pre-determined number of equal intervals
frames = seq(0,1001,4)
tests = erpavetest(simerp[,1:251],design=model.matrix(~y,data=simerp))
plot(frames,sign(tests$signal)*sqrt(tests$r2),type="l", xlab="Time (ms)",ylab="Correlation",ylim=c(-1,1))
points(frames[tests$significant],rep(-1,length(tests$significant)), pch=16,col="blue")
abline(v=frames[tests$breaks],lty=2,col="darkgray")
title("Test of averaged ERPs")

### Guthrie-Buchwald test
tests = gbtest(simerp[,1:251],design=model.matrix(~y,data=simerp),nbsamples=500)
plot(frames,sign(tests$signal)*sqrt(tests$r2),type="l",xlab="Time (ms)", ylab="Correlation",ylim=c(-1,1))
points(frames[tests$significant],rep(-1,length(tests$significant)), pch=16,col="blue")
title("Guthrie-Buchwald test")

### Benjamini-Hochberg Significance testing
tests = erptest(simerp[,1:251],design=model.matrix(~y,data=simerp))
plot(frames,sign(tests$signal)*sqrt(tests$r2),type="l",xlab="Time (ms)", ylab="Correlation",ylim=c(-1,1))
points(frames[tests$significant],rep(-1,length(tests$significant)), pch=16,col="blue")
title("Benjamini-Hochberg Significance testing")

### AFA significance testing
#tests = erpfatest(simerp[,1:251],design=model.matrix(~y,data=simerp),nbf=5,min.err=1e-01,maxiter=10, wantplot=TRUE)
tests = erpfatest(simerp[,1:251],design=model.matrix(~y,data=simerp),nbf=NULL,min.err=1e-01,maxiter=10, wantplot=TRUE)
plot(frames,sign(tests$signal)*sqrt(tests$r2),type="l",xlab="Time (ms)", ylab="Correlation",ylim=c(-1,1))
points(frames[tests$significant],rep(-1,length(tests$significant)), pch=16,col="blue")
title("AFA significance testing")


####################################

data(erpcz)
erpplot(erpcz[,1:251],frames=seq(0,1001,4),xlab="Time (ms)", ylab="ERP",main="ERP at electrode CZ")
data(simerp)
erpplot(simerp[,1:251],frames=seq(0,1001,4),xlab="Time (ms)", ylab="ERP",main="Simulated ERP")