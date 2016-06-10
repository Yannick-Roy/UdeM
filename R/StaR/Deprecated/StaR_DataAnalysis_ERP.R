# ERP Stuff...
library(R.matlab)
library(ggplot2)
library(gridExtra)
library(zoo)
require(timeSeries)

stde <- function(x) sd(x)/sqrt(length(x))

###########################################################################
#########################       Sequence !     ############################
###########################################################################
source("StaR_Designs.R")
source("StaR_LoadData.R")
source("StaR_MixedModels.R")
source("StaR_Anovas.R")
source("StaR_PlotStats.R")

designs = c(1,2,3,4,5,11,12,13,14,15)


#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
iDesign = 12
bReloadFile = FALSE
bReprepData = FALSE
bSaveOnDisk = FALSE

bAnova = TRUE
bMixedModels = FALSE

{
  # Formatting the "Wide structure" for stats.
  if(bReprepData)
  {
    fullData <- staR_prepData()
  }
  
  # Fill "Wide structure" with real data.
  if(bReloadFile == TRUE)
  {
    matlabData <- staR_fillFromMatlab("RData/export_mpt_erp_d1.mat", "MPT", fullData)
    fullData = matlabData[[1]]
    timeData = matlabData[[2]][1,]
  }
  
  # Sub select, according to current stats.
  retVal <- staR_selectData(fullData, iDesign)
  subDataset = retVal[[1]]
  subData = retVal[[2]]
  
  # Get mean, max, min, etc.
  paramsList <- staR_getDistParams(subData, timeData, iDesign)
  
  if(bAnova)
  {
    # -- Anova --
    # anovas.all = 3D; H | V | Full
    anovas.all <- staR_Anova(fullData = fullData, subData = subDataset, iDesign = iDesign)
    #save() # Save on disk.
    
    # -- Summary --
    # anovas.summaries = 3D; H | V | Full
    anovas.summaries <- staR_Summary(anovas.all, iDesign)
    #anovas <- NULL # Free memory.
    
    # -- pVals --
    anovas.ps <- staR_PVals(anovas.summaries, iDesign, 0.05)
    anovas.pVals <- anovas.ps[[1]]
    anovas.pSignificants <- anovas.ps[[2]]
    
    # -- Plot Stats --
    hStats <- plotStats(anovas.pSignificants, timeData)
    
    # -- pVals Correction --
    #anovas.cps <- staR_FDR(anovas.ps)
  }  
  
  if(bMixedModels)
  {
    # -- Mixed Models --
    # mixedmodels.all = 3D; H | V | Full
    mixedmodels.all <- staR_MMlmer(fullData = fullData, subData = subDataset, iDesign = iDesign)
    mixedmodels.all <- staR_MMlme(fullData = fullData, subData = subDataset, iDesign = iDesign)
    #save() # Save on disk.
    
    # -- Summary --
    # mixedmodels.summary = 3D; H | V | Full
    mixedmodels.summary <- staR_MM_Summary(mixedmodels.all, iDesign)
    #anovas <- NULL # Free memory.
    
    # -- pVals --
    mixedmodels.ps <- staR_MM_PVals(mixedmodels.summaries, iDesign, 0.05)
    mixedmodels.pVals <- mixedmodels.ps[[1]]
    mixedmodels.pSignificants <- mixedmodels.ps[[2]]
    
    # -- Plot Stats --
    hStats <- plotStats(mixedmodels.pSignificants, timeData)
    
    # -- pVals Correction --
    #anovas.cps <- staR_FDR(anovas.ps)
  }
  
  # -- Plot Data --
  hData <- plotData(subData, paramsList, timeData)
  
  # -- Plot All --
  designMatrix <- staR_getDesignMatrix(iDesign)
  
  hRows <- list()
  for(i in 1:designMatrix$nbRow)
  {
    for(j in 1:designMatrix$nbCol)
    {
      if(j > 1)
      {
        hRows[[i]] <- cbind(hRows[[i]], ggplotGrob(hData[[i]][[j]]), size = "last")
      }
      else
      {
        hRows[[i]] <- ggplotGrob(hData[[i]][[j]])
      }
    }
    hRows[[i]] <- cbind(hRows[[i]], ggplotGrob(hStats[[1]][[i]]), size = "last")
  }
  
  if(designMatrix$nbRow > 1)
  {
    nbRows <- length(hRows)
    hRows[[nbRows + 1]] <- ggplotGrob(hStats[[2]][[1]])
    for(j in 2:designMatrix$nbCol)
    {
      hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[2]][[j]]), size = "last")
    }
    if(iDesign >= 11 && iDesign <= 16)
    {
      hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[3]][[3]]), size = "last")
    }
  }
  
  if(length(hRows) == 1) 
  {
    grid.arrange(hRows[[1]], main=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
  
  if(length(hRows) == 2) 
  {
    grid.arrange(hRows[[1]], hRows[[2]], main=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
  
  if(length(hRows) == 3) 
  {
    grid.arrange(hRows[[1]], hRows[[2]], hRows[[3]], main=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
}