library(foreach)
library(doMC)
registerDoMC(10)

print(paste("Doing - anova on Full & Restricted"))
tic()
mmFits_lmer.anovas <- foreach(i=1:nbPoints, .combine=rbind) %do%
{ anova(mmFull[[i]], mmRestricted[[i]]) }
toc()

print(paste("Doing - KRmodcomp on Full & Restricted"))
tic()
mmFits_lmer.kr <- foreach(i=1:nbPoints, .combine=rbind) %do%
{ KRmodcomp(mmFull[[i]], mmRestricted[[i]]) }
toc()

save(mmFits_lmer.kr,mmFits_lmer.anovas, file="Results.RData")