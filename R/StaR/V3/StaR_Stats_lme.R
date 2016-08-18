library(lme4)
library(nlme)
library(pbkrtest)
library(multcomp)
library("lsmeans")

source("StaR_Tools.R")

STATS_DESIGNS_RND = ~1|subjects

staR_lme2 <- function(fullData, vars, fixedVars)
{
  # Do LME -> Get Model.
  pValsAndTitles_Mod <- staR_lme2_mod(fullData, fixedVars)
  
  # Do Post Hoc(s). All of them. (You can't unseen, so don't look!)
  pValsAndTitles_PostHoc <- staR_lme2_PostHoc(pValsAndTitles_Mod[[1]], vars, fixedVars)
  
  return(list(pValsAndTitles_Mod, pValsAndTitles_PostHoc))
}

staR_lme2_mod <- function(fullData, fixedVars)
{
  print(fixedVars)
  cl <- makeCluster(4) 
  clusterExport(cl, list("lme", "STATS_DESIGNS_RND", "fixedVars"), envir = environment() )
  
  print(paste("Doing - lme (fullData) : ", fixedVars, " random=", format(STATS_DESIGNS_RND), ", na.action = na.omit", ", method=ML"))
  tic()
  mixedmodels.full <- parLapply(cl = cl, fullData, fun = function(x) {lme(fixed=formula(fixedVars), random=STATS_DESIGNS_RND, data=x, method="ML", na.action = na.omit)})
  toc()
  
  stopCluster(cl)
  
  return (list(mixedmodels.full, staR_lme2_pvals(mixedmodels.full)))
}

staR_lme2_pvals <- function(summary)
{
  mixedmodels.summary <- lapply(summary, anova)   
  #mixedmodels.lsm <- lapply(summary, lsmeans(x,  ~ groups:sessions:motions:order))
  
  # -- pVals --    
  print("Doing lme pVals!")
  
  pVs <- list()
  pNames <- list()
  
  n = length(mixedmodels.summary[[1]]$'p-value')
  for(i in 1:(n - 1))
  {
    pVs[[i]] <- lapply(mixedmodels.summary, FUN = function(x) {x$'p-value'[[i+1]]})
    pNames[[i]] <- row.names(mixedmodels.summary[[1]])[i+1]#[2:n]
  }
  print("Done!")
  
  return(list(pVs, pNames))
}

staR_lme2_PostHoc <- function(lmeMod, vars, fixedVars)
{
   print(paste("Post Hoc with: ", fixedVars))
   
   # Get all the combinaisons
   effects <- list()
   for(i in 1:length(vars)){
    effects[[i]] = combn(vars, i)
   }
   
   pVals <- list()
   pTitles <- list()
   
   # For all the level of effects (Main effects, Interaction, Triple Interaction, ...)
   for(i in 1:length(effects)){
    print(paste("Effect: ", i, sep=""))
    
    pVals[[i]]  <- list()
    pTitles[[i]]  <- list()
    
    # For each level of effects, for each of the combinaisons...
    for(j in 1:dim(effects[[i]])[[2]]){
      print(paste("Effect: ", effects[[i]][,j]))
      mixedmodels.lsm <- lapply(lmeMod, FUN = function(x) {lsmeans(x,  effects[[i]][,j])})
      mixedmodels.lsm.sum <- lapply(mixedmodels.lsm, FUN = function(x) {summary(x, infer = c(TRUE,TRUE), level = .90, adjust = "bon")})
      
      pValsAndTitles <- staR_lsmeans_pValsAndTitles(mixedmodels.lsm.sum)
      pVals[[i]][[j]] <- pValsAndTitles[[1]]
      pTitles[[i]][[j]] <- pValsAndTitles[[2]]
      
      #writeMat(paste(dirPlots, "/", paste("Effects", i, sep="") ,"_", paste(effects[[i]][,j], collapse="_"), ".mat", sep=""), pValsSub=unlist(pValsAndTitles[[i]][[1]]), pTitlesSub=unlist(pValsAndTitles[[i]][[2]]))
    }
   }
   
   return(list(pVals, pTitles))
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
        curTitle <- paste(curTitle, tmp, sep=";")
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
      pVals[[v]][[p]] <- as.numeric(tmp[[v]][[2]])
    }
  }
  
  return(list(pVals, pTitles))
}

