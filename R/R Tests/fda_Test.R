scriptsDir <- system.file('scripts', package='fda')
Rscripts <- dir(scriptsDir, full.names=TRUE, pattern='R$')
fdarm <- grep('fdarm', Rscripts, value=TRUE)
chapters <- length(fdarm)
# NOTE: If R fails in any of these scripts,
# this for loop will not end normally,
# and the abnormal termination will be displayed:
for(ch in 1:chapters){
  cat('Running', fdarm[ch], '\n')
  invisible(source(fdarm[ch]))
}


## Simple smoothing
##
girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf, lambda=0.1))
plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
     main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd), xlab="age", ylab="growth rate (cm / year)",
     main="Girls in Berkeley Growth Study" )
plot(deriv(girlGrowthSm$fd, 2), xlab="age",
     ylab="growth acceleration (cm / year^2)",
     main="Girls in Berkeley Growth Study" )
##
## Simple basis
##
bspl1.2 <- create.bspline.basis(norder=1, breaks=c(0,.5, 1))
plot(bspl1.2)
# 2 bases, order 1 = degree 0 = step functions:
# (1) constant 1 between 0 and 0.5 and 0 otherwise
# (2) constant 1 between 0.5 and 1 and 0 otherwise.
fd1.2 <- Data2fd(0:1, basisobj=bspl1.2)
op <- par(mfrow=c(2,1))
plot(bspl1.2, main='bases')
plot(fd1.2, main='fit')
par(op)
# A step function: 0 to time=0.5, then 1 after