# ERP Stuff...
library(R.matlab)
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(lattice)
library(pbkrtest)
library(lme4)
library(nlme)
library(parallel)
require(timeSeries)
require(reshape2)

#require(fdrtool)
#require(tictoc)

stde <- function(x) sd(x)/sqrt(length(x))

###########################################################################
#########################       Sequence !     ############################
###########################################################################
source("StaR_Designs.R")
source("StaR_LoadData.R")
source("StaR_MixedModels.R")
source("StaR_Anovas.R")
source("StaR_PlotStats.R")


##### PARALLEL Library ! Or Snow !



#designs = c(1,2,3,4,5,11,12,13,14,15,16)
designs = 3

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = TRUE
bReloadMatlabFile = FALSE
bReprepMatlabData = FALSE
bSaveOnDiskImages = FALSE
bSaveOnDiskData = FALSE

bAnova = FALSE
bMixedModels = TRUE

# Support only 1 at the time for now... Please "FALSE" others.
bERSP = TRUE #TRUE
bERP = FALSE #FALSE

nbPoints = 0
if(bERSP)
{
 nbPoints = 54000
}
if(bERP)
{
  nbPoints = 1536
}

# Clear Plots.
dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
dirPlotsPath <- "~/Documents/Playground/RMatlab_Data/StaR_Images/"
dirPlots <- paste(dirPlotsPath, dirPlotsName, sep = "")
dir.create(dirPlots)
  
#save(fullData, timeData, freqData, subDataset, subData, paramsList, anovas.summaries, anovas.pVals, anovas.pSignificants,  file = "RWorkspaceVariables.RData")
for(i in designs)
{
  iDesign = i
  
  # Prep Plot Series !
  grid.arrange(textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=30)))
  if(bSaveOnDiskImages)
  {
    dev.copy2pdf(file = paste(dirPlots, "/Title_", i, ".pdf", sep = ""))
    dev.off()
  }
  
  ##################################
  ############## DATA ##############
  ##################################
  # Formatting the "Wide structure" for stats.
  if(bReprepMatlabData)
  {
    fullData <- staR_prepData()
  }
  
  # Fill "Wide structure" with real data.
  if(bReloadMatlabFile)
  {
    if(bERP)
    {
      matlabData <- staR_fillFromMatlab("~/Documents/Playground/RMatlab_Data/export_mpt_erp_d1.mat", "MPT", fullData, nbPoints)
    }
    if(bERSP)
    {
      matlabData <- staR_fillFromMatlab("~/Documents/Playground/RMatlab_Data/export_mpt.mat", "MPT", fullData, nbPoints)
    }
    
    fullData = matlabData[[1]]
    
    if(length(matlabData) >= 3)
      {if(length(matlabData[[2]]) >= 1)
        {timeData = matlabData[[2]][1,]}}
    
    if(length(matlabData) >= 3)
      {if(length(matlabData[[3]]) >= 1)
        {freqData = matlabData[[3]][1,]}}
  }
  
  # Sub select, according to current stats.
  if(bReloadRData)
  {
    print("Select Data from File...")
    #load
  }
  else
  {
    print("Select Data...")
    retVal <- staR_selectData(fullData, iDesign)
    subDataset = retVal[[1]]
    subData = retVal[[2]]
    if(bSaveOnDiskData)
    {
      #save(file = "RSelectedData")
    }
  }
  
  # Get mean, max, min, etc.
  print("Get Params (mean, min, max, ...)")
  #paramsList <- staR_getDistParams(subData, timeData, iDesign)
  if(bERSP) {timeVals <- seq(1, nbPoints)}
  if(bERP) {timeVals <- timeData}
  paramsList <- staR_getDistParams(subData, timeVals, iDesign)
  
  ##################################
  ############# ANOVAS #############
  ##################################
  if(bAnova)
  {
    # -- Anova --
    # anovas.all = 3D; H | V | Full
    anovas.all <- staR_Anova(fullData = fullData, subData = subDataset, iDesign = iDesign, nbPoints, func = 'anova')
    #save(anovas.all, file = "RAnovas.RData") # Save on disk.
    
    # -- Summary --
    # anovas.summaries = 3D; H | V | Full
    anovas.summaries <- staR_Summary(anovas.all, iDesign)
    #anovas.all <- NULL # Free memory.
    
    # -- pVals --
    anovas.ps <- staR_PVals(anovas.summaries, iDesign, 0.05)
    anovas.pVals <- anovas.ps[[1]]
    anovas.pSignificants <- anovas.ps[[2]]
  
    # -- Plot Stats --
    if(bERP) { hStats <- plotStats(anovas.pSignificants, timeData) }
    if(bERSP) { hStats <- plotStats_ERSP(anovas.pSignificants, timeData) }
        
    # -- pVals Correction --
    #anovas.cps <- staR_FDR(anovas.ps)
    
    # -- post-hoc Comparaison --
    #TukeyHSD(anovas.all)
  }  
  
  ##################################
  ########## Mixed Models ##########
  ##################################
  if(bMixedModels)
  {
    # -- Mixed Models --
    # mixedmodels.all = 3D; H | V | Full
    mixedmodels.all <- staR_MMlmer(fullData = fullData, subData = subDataset, iDesign = iDesign, cluster = cl)
    
    
    cl <- makeCluster(8) 
    clusterExport(cl, list("lmer", "STATS_DESIGNS_MM", "STATS_DESIGNS_MM_RESTRICTED", "iDesign"))
    
    tic()
    print(paste("Doing - lmer (fullData) : ", format(STATS_DESIGNS_MM[[iDesign]])))
    #mixedmodels.full <- lapply(fullData, FUN = function(x) {lmer(STATS_DESIGNS_MM[[iDesign]], x)})
    mixedmodels.full <- parLapply(cl = cl, fullData, fun = function(x) {lmer(STATS_DESIGNS_MM[[iDesign]], x)})
    toc()
    
    tic()
    print(paste("Doing - lmer (fullData) : ", format(STATS_DESIGNS_MM_RESTRICTED)))
    #mixedmodels.restricted <- lapply(fullData, FUN = function(x) {lmer(STATS_DESIGNS_MM_RESTRICTED, x)})
    mixedmodels.restricted <- parLapply(cl = cl, fullData, fun = function(x) {lmer(STATS_DESIGNS_MM_RESTRICTED, x)})
    toc()
    
    stopCluster(cl)
    
    mmFull <- mixedmodels.full
    mmRestricted <- mixedmodels.restricted 
    
    tic()
    print(paste("Doing - KRmodcomp & anova (full & restricted models) : ", format(STATS_DESIGNS_MM_RESTRICTED), " vs ", format(STATS_DESIGNS_MM[[iDesign]])))
    mmFull <- mixedmodels.all[[1]]
    mmRestricted <- mixedmodels.all[[2]]
    
    mmFits_lmer.kr <- list()
    mmFits_lmer.anova <- list()
#     for(i in 1:nbPoints)
#     {
#       #mmFits_lmer.kr[[i]] <- KRmodcomp(mmFull[[i]], mmRestricted[[i]])
#       mmFits_lmer.anova[[i]] <- anova(mmFull[[i]], mmRestricted[[i]])
#       
#       if(i %% 100 == 0)
#         print(i)
#     }
    
    save(mmFull, mmRestricted, file="KRTest.RData")
    
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
    
    #submmFull <- mmFull[1:10]
    #submmRestricted <- mmRestricted[1:10]
    
    #cl <- makeCluster(8) 
    #clusterExport(cl, list("KRmodcomp"))

    tic()
    #mmFits_lmer.anovas <- parSapply(cl = cl, mmFull, mmRestricted, FUN = anova)
    comb <- data.frame(f=submmFull, r=submmRestricted)
    submFits_lmer.kr <- clusterMap(cl = cl, FUN = KRmodcomp, submmFull, submmRestricted)
    toc()
    stopCluster(cl)
    
    print("Done!")
    #mixedmodels.all <- staR_MMlme(fullData = fullData, subData = subDataset, iDesign = iDesign)
    #save() # Save on disk.
    
    # -- Summary --
    # mixedmodels.summary = 3D; H | V | Full
    #mixedmodels.summary <- staR_MM_Summary(mixedmodels.all, iDesign)
    #anovas <- NULL # Free memory.
    
    # -- pVals --    
    #pVals <- lapply(mmFits_lmer.kr, FUN = function(x) {x$'p.value'})
    mixedmodels.ps <- staR_mmPVals(mmFits_lmer.kr, iDesign, 0.05)
    mixedmodels.pVals <- mixedmodels.ps[[1]]
    mixedmodels.pSignificants <- mixedmodels.ps[[2]]
    #mixedmodels.ps <- staR_MM_PVals(mixedmodels.summaries, iDesign, 0.05)
    #mixedmodels.pVals <- mixedmodels.ps[[1]]
    #mixedmodels.pSignificants <- mixedmodels.ps[[2]]
    
    # -- Plot Stats --
    hStats <- plotStats(mixedmodels.pSignificants, timeData)
    
    # -- pVals Correction --
    #anovas.cps <- staR_FDR(anovas.ps)
  }
  
  ##################################
  ########### PLOT DATA ############
  ##################################
  # -- Plot Data --
  if(bERP) { hData <- plotData(subData, paramsList, timeData) }
  if(bERSP) { hData <- plotData_ERSP(subData, paramsList, timeData) }
  
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
    grid.arrange(hRows[[1]], top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
  
  if(length(hRows) == 2) 
  {
    grid.arrange(hRows[[1]], hRows[[2]], top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
  
  if(length(hRows) == 3) 
  {
    grid.arrange(hRows[[1]], hRows[[2]], hRows[[3]], top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
  }
  
  if(bSaveOnDiskImages)
  {
    dev.copy2pdf(file = paste(dirPlots, "/Data_", i, ".pdf", sep = ""))
    dev.off()
  }
  
  ##################################
  ###### PLOT COMPLEX DESIGN #######
  ##################################
  if(iDesign >= 11 && iDesign <= 19)
  {
    if(length(hStats[[3]]) == 1)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 2)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 3)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 4)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), top=textGrob(staR_getDesignName(iDesign), gp=gpar(fontsize=20,font=3)))
    }
    
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_", i, ".pdf", sep = ""))
      dev.off()
    }
  }
}