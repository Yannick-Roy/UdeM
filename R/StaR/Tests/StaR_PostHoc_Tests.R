## ==== Post Hoc Tests =====

library(multcomp)
com=glht(your model,linfct=mcp(you fixed factor="Tukey"))
cld(com)

K1 <- glht(modF, mcp(groups = "Tukey"))$linfct
K2 <- glht(modF, mcp(motions = "Tukey"))$linfct
summary(glht(modF, linfct = rbind(K1, K2)))

tmp <- expand.grid(groups = unique(fD$groups), + motions = unique(fD$motions))
X <- model.matrix(~ groups * motions, data = tmp)
glht(mod, linfct = X)


lsms = lsmeans(modF, ~ groups:sessions:orders)
org.sum <- summary(lsms, infer = c(TRUE,TRUE), level = .90, adjust = "tukey")



## ==========================================
## =========== WORKING STEPS ! ==============
## ==========================================
library("lsmeans")
mixedmodels.lsm <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x,  ~ groups:sessions:motions:orders)})
mixedmodels.lsm.sum <- lapply(mixedmodels.lsm, FUN = function(x) {summary(x, infer = c(TRUE,TRUE), level = .90, adjust = "fdr")})

mixedmodels.lsm.pVals = lapply(mixedmodels.lsm.sum, FUN = function(x) {x$p.value})
mixedmodels.lsm.pVals1 = lapply(mixedmodels.lsm.pVals, FUN = function(x) {x[[9]]})
plot(unlist(mixedmodels.lsm.pVals1))

mixedmodels.lsm <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x,  ~ groups:orders)})
mixedmodels.lsm.sum <- lapply(mixedmodels.lsm, FUN = function(x) {summary(x, infer = c(TRUE,TRUE), level = .90, adjust = "bon")})

mixedmodels.lsm.pVals = lapply(mixedmodels.lsm.sum, FUN = function(x) {x$p.value})
mixedmodels.lsm.pVals1 = lapply(mixedmodels.lsm.pVals, FUN = function(x) {x[[1]]})
plot(unlist(mixedmodels.lsm.pVals1))

staR_lsmeans_pvals(mixedmodels.lsm.sum[1536])

staR_lsmeans_pvals <- function(modSum)
{
  lsMeanPos = match("lsmean", names(modSum[[1]]))
  nbVars = lsMeanPos - 1
  rows <- list()
  for (i in 1:length(modSum[[1]][[1]])){
    curTitle <- NULL
    for(j in 1:nbVars){
      tmp <- paste(names(modSum[[1]])[[j]], "=", modSum[[1]][[j]][[i]], sep="")
      
      if(is.null(curTitle)) {
        curTitle <- tmp
      }else {
        curTitle <- paste(curTitle, tmp, sep=" | ")
      }
      
      #Get pValue + Title
      rows[[i]] <- c(curTitle, modSum[[1]]$p.value[[i]])
    }
  }
  
  return(rows)
}


# Get all the pVals & Titles.
tmp <- staR_lsmeans_pvals(mixedmodels.lsm.sum[1]) # test for size.
pVals <- list()
pTitles <- list()
for (v in 1:length(tmp)){
  pVals[[v]] <- list()
  pTitles[[v]] <- tmp[[v]][[1]]
}

for (p in 1:length(mixedmodels.lsm.sum)){
  tmp <- staR_lsmeans_pvals(mixedmodels.lsm.sum[p])
  for (v in 1:length(tmp)){
    pVals[[v]][[p]] <- tmp[[v]][[2]]
  }
}

# Get all the combinaisons
a <- c("domains","groups","sessions","orders","motions")
effects <- list()
for(i in 1:length(a)){
  effects[[i]] = combn(a, i)
}

## ==================================================
## ============ FULL WORKING SEQUENCE ! =============
## ==================================================

library("lsmeans")

#a <- c("domains","groups","sessions","orders","motions")
a <- c("groups","sessions","orders","motions")

# Get all the combinaisons
effects <- list()
for(i in 1:length(a)){
  effects[[i]] = combn(a, i)
}

# For all the level of effects (Main effects, Interaction, Triple Interaction, ...)
for(i in 1:length(effects)){
  #i = 2  
  print(paste("Effect: ", i, sep=""))
  
  # For each level of effects, for each of the combinaisons...
  for(j in 1:dim(effects[[i]])[[2]]){
    mixedmodels.lsm <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x,  effects[[i]][,j])})
    mixedmodels.lsm.sum <- lapply(mixedmodels.lsm, FUN = function(x) {summary(x, infer = c(TRUE,TRUE), level = .90, adjust = "bon")})
    
    pValsAndTitles <- staR_lsmeans_pValsAndTitles(mixedmodels.lsm.sum)
    
    writeMat(paste(dirPlots, "/", paste("Effects", i, sep="") ,"_", paste(effects[[i]][,j], collapse="_"), ".mat", sep=""), pValsSub=unlist(pValsAndTitles[[1]]), pTitlesSub=unlist(pValsAndTitles[[2]]))
  }
}

staR_lsmeans_pvals <- function(modSum)
{
  lsMeanPos = match("lsmean", names(modSum[[1]]))
  nbVars = lsMeanPos - 1
  rows <- list()
  for (i in 1:length(modSum[[1]][[1]])){
    curTitle <- NULL
    for(j in 1:nbVars){
      tmp <- paste(names(modSum[[1]])[[j]], "=", modSum[[1]][[j]][[i]], sep="")
      
      if(is.null(curTitle)) {
        curTitle <- tmp
      }else {
        curTitle <- paste(curTitle, tmp, sep=" | ")
      }
      
      #Get pValue + Title
      rows[[i]] <- c(curTitle, modSum[[1]]$p.value[[i]])
    }
  }
  
  return(rows)
}

staR_lsmeans_pValsAndTitles <- function(lmssum)
{
  # Get all the pVals & Titles.
  tmp <- staR_lsmeans_pvals(lmssum[1]) # Get size (with 1st item).
  pVals <- list()
  pTitles <- list()
  for (v in 1:length(tmp)){
    pVals[[v]] <- list()
    pTitles[[v]] <- tmp[[v]][[1]]
  }
  
  for (p in 1:length(lmssum)){
    tmp <- staR_lsmeans_pvals(lmssum[p])
    for (v in 1:length(tmp)){
      pVals[[v]][[p]] <- tmp[[v]][[2]]
    }
  }
  
  return(list(pVals, pTitles))
}
