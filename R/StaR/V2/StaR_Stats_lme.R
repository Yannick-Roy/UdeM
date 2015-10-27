library(lme4)
library(nlme)
library(pbkrtest)

source("StaR_Tools.R")

staR_lme <- function(fullData, iDesign, bSubAnalysis = FALSE)
{
  cl <- makeCluster(4) 
  clusterExport(cl, list("lme", "STATS_DESIGNS", "STATS_DESIGNS_RND", "iDesign"))
  
  print(paste("Doing - lme (fullData) : ", format(STATS_DESIGNS[[iDesign]]), " random=", format(STATS_DESIGNS_RND)))  
  tic()
  mixedmodels.full <- parLapply(cl = cl, fullData, fun = function(x) {lme(fixed=STATS_DESIGNS[[iDesign]], random=STATS_DESIGNS_RND, data=x)})
  toc()
  
  stopCluster(cl)
  
  #mixedmodels.full <- lapply(fullData, FUN = function(x) {lme(STATS_DESIGNS[[iDesign]], random=STATS_DESIGNS_RND, x)})
  
  return (staR_lme_pvals(mixedmodels.full))
}

staR_lme_pvals <- function(summary)
{
    mixedmodels.summary <- lapply(summary, anova)   
    
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

# staR_lme_sub() <- function()
# {
#   
#   mixedmodels.subH <- list()
#   mixedmodels.subV <- list()
#   
#   print("Done!")
#   
#   mixedmodels.all <- list(mixedmodels.subH, mixedmodels.subV, mixedmodels.full)
#   
#   # -- Summary --
#   mixedmodels.summary <- list()
#   mixedmodels.summary[[1]] <- list()
#   mixedmodels.summary[[2]] <- list()
#   mixedmodels.summary[[3]] <- list()
#   # mixedmodels.summary = 3D; H | V | Full
#   mixedmodels.summaryTemp <- lapply(mixedmodels.all[[3]], anova)   
#   mixedmodels.summary[[3]][[1]] <- mixedmodels.summaryTemp
#   
#   # -- pVals --    
#   print("... lme ...")
#   mixedmodels.pVals <- list()
#   mixedmodels.pValsTitle <- list()
#   mixedmodels.pVals[[3]] <- list()
#   mixedmodels.pValsTitle[[3]] <- list()
#   n = length(mixedmodels.summary[[3]][[1]][[1]]$`p-value`)
#   for(i in 1:(n - 1))
#   {
#     mixedmodels.pVals[[3]][[i]] <- lapply(mixedmodels.summary[[3]][[1]], FUN = function(x) {x$'p-value'[[i+1]]})
#     mixedmodels.pValsTitle[[3]] <- row.names(mixedmodels.summary[[3]][[1]][[1]])[2:n]
#   }
#   print("... done ...")
# }


###################################################################################
######  SUB !
###################################################################################
staR_lme_sub <- function(subData, iDesign)
{
  #stats.fullAnalysis.aov.retVal <- staR_aov(fullData, iDesign)
  hDataset <- list()
  vDataset <- list()
  lDataset <- list()
  for(p in 1:nbPoints)  # Points
  {
    ## -----------------------
    ## -- Horizontal (Rows) --
    ## -----------------------
    hDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(i in 1:length(subData)) # Layers
      {
        hDataset[[p]][[i]] <- list()
        if(length(subData[[i]]) > 0)
        {
          for(j in 1:length(subData[[i]])) # Rows
          {
            hDataset[[p]][[i]][[j]] <- list()
            if(length(subData[[i]][[j]]) > 0)
            {
              for(k in 1:length(subData[[i]][[j]])) # Cols
              { 
                #print(paste(p, " (h) - ", i, j, k))
                # -- Horizontal --
                #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
                if(k == 1) {hDataset[[p]][[i]][[j]] <- subData[[i]][[j]][[k]][[p]]}
                else {hDataset[[p]][[i]][[j]] <- rbind(hDataset[[p]][[i]][[j]], subData[[i]][[j]][[k]][[p]])}
              }
            }
          }
        }
      }
    }
    
    ## -----------------------
    ## -- Vertival (Cols) --
    ## -----------------------
    vDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(i in 1:length(subData)) # Layers
      {
        vDataset[[p]][[i]] <- list()
        if(length(subData[[i]]) > 0)
        {
          for(k in 1:length(subData[[i]][[1]])) # Cols
          {
            vDataset[[p]][[i]][[k]] <- list()
            for(j in 1:length(subData[[i]])) # Rows
            {
              #print(paste(p, " (v) - ", i, j, k))
              # -- Vertical --
              if(j == 1) {vDataset[[p]][[i]][[k]] <- subData[[i]][[j]][[k]][[p]]}
              else {vDataset[[p]][[i]][[k]] <- rbind(vDataset[[p]][[i]][[k]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    }
    
    ## -----------------------
    ## -- 3D (Layers) --
    ## -----------------------
    lDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(j in 1:length(subData[[1]])) # Rows
      {
        lDataset[[p]][[j]] <- list()
        if(length(subData[[1]][[j]]) > 0)
        {
          for(k in 1:length(subData[[1]][[j]])) # Cols
          {
            lDataset[[p]][[j]][[k]] <- list()
            for(i in 1:length(subData)) # Layers
            {
              #print(paste(p, " (v) - ", i, j, k))
              # -- Vertical --
              if(j == 1) {lDataset[[p]][[j]][[k]] <- subData[[i]][[j]][[k]][[p]]}
              else {lDataset[[p]][[j]][[k]] <- rbind(lDataset[[p]][[j]][[k]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    }   
  }
  
  stats.subAnalysis.hData <- staR_InvertDimensions3D(hDataset)
  stats.subAnalysis.vData <- staR_InvertDimensions3D(vDataset)
  stats.subAnalysis.lData <- staR_InvertDimensions3D(lDataset)
  
  stats.subAnalysis.combinedData <- list()
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 0) {stats.subAnalysis.combinedData[[1]] <- stats.subAnalysis.hData} # Rows.
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 1) {stats.subAnalysis.combinedData[[2]] <- stats.subAnalysis.vData} # Cols.
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 2) {stats.subAnalysis.combinedData[[3]] <- stats.subAnalysis.lData} # Layers.
  
  #stats.subAnalysis.aov.retVal <- staR_aov(stats.subAnalysis.hData[[1]][[1]], iDesign)
  
  stats.subAnalysis.pVals <- list()
  stats.subAnalysis.pTitles <- list()
  if(length(stats.subAnalysis.combinedData) > 0)
  {
    for(curDim in 1:length(stats.subAnalysis.combinedData)) # Row & Cols & Layers (1 - Horizontal | 2 - Vertical)
    {
      if(length(stats.subAnalysis.combinedData[[curDim]]) > 0)
      {
        stats.subAnalysis.pVals[[curDim]] <- list()
        stats.subAnalysis.pTitles[[curDim]] <- list()
        for(i in 1:length(stats.subAnalysis.combinedData[[curDim]])) # Layers
        {
          stats.subAnalysis.pVals[[curDim]][[i]] <- list()
          stats.subAnalysis.pTitles[[curDim]][[i]] <- list()
          for(j in 1:length(stats.subAnalysis.combinedData[[curDim]][[i]])) # Rows
          {
            print(paste("Doing - Mixed Models (subData - [", curDim, ",", i, ",", j, "]) : fixed = ", format(STATS_SUB_DESIGNS[[iDesign]][[curDim]]), " random = ", format(STATS_DESIGNS_RND)))
            cl <- makeCluster(4) 
            clusterExport(cl, list("lme", "STATS_SUB_DESIGNS", "STATS_DESIGNS_RND", "iDesign"))
            tic()
            
            ###############
            # TODO : Fix me with envir...
            ###############
            if(curDim == 1) # TODO : Fix me with envir...
            {
              mixedmodels.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[1]][[i]][[j]], fun = function(x) {lme(fixed=STATS_SUB_DESIGNS[[iDesign]][[1]], random=STATS_DESIGNS_RND, data=x)})
            }
            if(curDim == 2) # TODO : Fix me with envir...
            {
              mixedmodels.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[2]][[i]][[j]], fun = function(x) {lme(fixed=STATS_SUB_DESIGNS[[iDesign]][[2]], random=STATS_DESIGNS_RND, data=x)})
            }
            if(curDim == 3) # TODO : Fix me with envir...
            {
              mixedmodels.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[3]][[i]][[j]], fun = function(x) {lme(fixed=STATS_SUB_DESIGNS[[iDesign]][[3]], random=STATS_DESIGNS_RND, data=x)})
            }
            
            toc()
            stopCluster(cl)
            
            mixedmodels.full.titles = paste("lme", " : fixed = ",  format(STATS_SUB_DESIGNS[[iDesign]][[curDim]]), " random = ", format(STATS_DESIGNS_RND))
            print("Done!")
            
            print(paste("Doing - Summary (subData - [",curDim, ",", i, ",", j, "]) : fixed = ", format(STATS_SUB_DESIGNS[[iDesign]][[curDim]]), " random = ", format(STATS_DESIGNS_RND)))
            mixedmodels.full.summary <- lapply(mixedmodels.full, FUN = function(x) {summary(x)})
            
            stats.subAnalysis.lme.retVal <- staR_lme_pvals(mixedmodels.full.summary)
            
            stats.subAnalysis.pVals[[curDim]][[i]][[j]] <- unlist(stats.subAnalysis.lme.retVal[[1]])
            stats.subAnalysis.pTitles[[curDim]][[i]][[j]]  <- unique(stats.subAnalysis.lme.retVal[[2]])
          }
        }
      }
    }
  }
  
  return(list(stats.subAnalysis.pVals, stats.subAnalysis.pTitles))
}