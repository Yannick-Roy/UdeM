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
library(ez)
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

#designs = c(1,2,3,4,5,11,12,13,14,15,16)
#designs = c(11,12,13,14,15,16)
#designs = c(1,2,3,4,5)
designs = 12

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = FALSE
bReloadMatlabFile = TRUE
bReprepMatlabData = TRUE
bSaveOnDiskImages = FALSE
bSaveOnDiskData = FALSE

bSmallSamples = TRUE

#######################
# Stats !
#######################
bAnova = FALSE
bMixedModels = TRUE

AnovaFunc = 'aov' # ez, aov, Anova
MMFunc = 'lmer'
#######################

# Support only 1 at the time for now... Please "FALSE" others.
bERSP = FALSE
bERP = TRUE 

nbPoints = 0
if(bERSP)
{
 nbPoints = 54000
}
if(bERP)
{
  nbPoints = 1536
}

bOnlyFullAnalysis = TRUE

# Clear Plots.
dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlots <- paste(dirPlotsPath, dirPlotsName, sep = "")
dir.create(dirPlots)

hTitles <- list()  

#save(fullData, timeData, freqData, subDataset, subData, paramsList, anovas.summaries, anovas.pVals, anovas.pSignificants,  file = "RWorkspaceVariables.RData")
for(i in designs)
{
  iDesign = i
  
  # Prep Plot Series !
  grid.arrange(textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=30)))
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
      matlabData <- staR_fillFromMatlab("~/Documents/Playground/UdeM/RMatlab_Data/export_mpt_erp_d1.mat", "MPT", fullData, nbPoints)
    }
    if(bERSP)
    {
      matlabData <- staR_fillFromMatlab("~/Documents/Playground/UdeM/RMatlab_Data/export_mpt.mat", "MPT", fullData, nbPoints)
    }
    
    fullData = matlabData[[1]]
    
    if(length(matlabData) >= 3)
      {if(length(matlabData[[2]]) >= 1)
        {timeData = matlabData[[2]][1,]}}
    
    if(length(matlabData) >= 3)
      {if(length(matlabData[[3]]) >= 1)
      {freqData = matlabData[[3]][1,]}}
    
    if(bSmallSamples)
    {
      lowLimit <- which(timeData > 0)[1]
      highLimit <- which(timeData < 600)
      highLimit <- highLimit[length(highLimit) - 1]
      
      fullData <- fullData[lowLimit:highLimit]
      timeData <- timeData[lowLimit:highLimit]
      
      #freqData
      #timeVals <- timeVals[lowLimit:highLimit]
    }
  }
  
  # Mainly because of Small Samples. But anyway nbPoints can't be different from fullData.
  nbPoints = length(fullData)
  
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
    #anovas.result <- staR_Anova(fullData = fullData, subData = subDataset, iDesign = iDesign, nbPoints, func = AnovaFunc)
    anovas.result <- staR_Anova(fullData = fullData, subData = NULL, iDesign = iDesign, nbPoints, func = AnovaFunc)
    anovas.all <- anovas.result[1:3]
    hTitles[[3]] <- anovas.result[[4]]

    #save(anovas.all, file = "RAnovas.RData") # Save on disk.
    
    # -- Summary --
    # anovas.summaries = 3D; H | V | Full
    anovas.summaries <- staR_Summary(anovas.all, iDesign, func = AnovaFunc)
    #anovas.all <- NULL # Free memory.
    
    # -- pVals --
    anovas.ps <- staR_PVals(anovas.summaries, iDesign, 0.05, func = AnovaFunc)
    anovas.pVals <- anovas.ps[[1]]
    anovas.pSignificants <- anovas.ps[[2]]
  
    # -- Plot Stats --
    #plot(unlist(anovas.pVals[[1]][[1]]), type="l")
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
    mixedmodels.all <- staR_MMlmer(fullData = fullData, subData = subDataset, iDesign = iDesign, func = MMFunc)
    hTitles[[3]] <- "Mixed Models..."
    #mixedmodels.all <- staR_MMlme(fullData = fullData, subData = subDataset, iDesign = iDesign)
    #save() # Save on disk.
    
    # -- Summary --
    mixedmodels.summary <- list()
    mixedmodels.summary[[1]] <- list()
    mixedmodels.summary[[2]] <- list()
    mixedmodels.summary[[3]] <- list()
    # mixedmodels.summary = 3D; H | V | Full
    #mixedmodels.summary <- staR_MM_Summary(mixedmodels.all, iDesign)
    mixedmodels.summaryTemp <- staR_MMKRComp(mixedmodels.all[[3]][[1]], mixedmodels.all[[3]][[2]])
    #anovas <- NULL # Free memory.
    mixedmodels.summary[[3]][[1]] <- mixedmodels.summaryTemp

    #mixedmodels.summary[[1]][[1]] <- staR_MMKRComp(mixedmodels.all[[1]][[1]][[1]], mixedmodels.all[[1]][[2]][[1]])
    #mixedmodels.summary[[1]][[2]] <- staR_MMKRComp(mixedmodels.all[[1]][[1]][[2]], mixedmodels.all[[1]][[2]][[2]])
    #mixedmodels.summary[[2]][[1]] <- staR_MMKRComp(mixedmodels.all[[2]][[1]][[1]], mixedmodels.all[[2]][[2]][[1]])
    #mixedmodels.summary[[2]][[2]] <- staR_MMKRComp(mixedmodels.all[[2]][[1]][[2]], mixedmodels.all[[2]][[2]][[2]])
    #mixedmodels.summary[[2]][[3]] <- staR_MMKRComp(mixedmodels.all[[2]][[1]][[3]], mixedmodels.all[[2]][[2]][[3]])
    #mixedmodels.summary[[2]][[4]] <- staR_MMKRComp(mixedmodels.all[[2]][[1]][[4]], mixedmodels.all[[2]][[2]][[4]])
    #################################################
    ## First Dimension should be 3:
    ## 1 - Horizontals | Rows (graphs are vertical, right side)
    ## 2 - Verticals | Cols (graphs are horizontal, bottom line)
    ## 3 - Full model with interactions.
    ######
    ## With mixed models you also have the restricted model 
    ## to compare wth to get the p-value.
    ## mixedmodels.all[[1]]                  --> Rows.
    ## mixedmodels.all[[1]][[1]]             --> First Row.
    ## mixedmodels.all[[1]][[1]][[1]]        --> Full model (first row)
    ## mixedmodels.all[[1]][[1]][[1]][[1]]   --> First pixel, full model, first row.
    ##
    ## ** PAS TOUT A FAIT... D2 = ful vs restricted. **
    ## Inverted dimension for calcul
    ##
    #################################################
    
    # -- pVals --    
    mixedmodels.ps <- staR_mmPVals(mixedmodels.summary, iDesign, 0.05)
    mixedmodels.pVals <- mixedmodels.ps[[1]]
    mixedmodels.pSignificants <- mixedmodels.ps[[2]]
    #mixedmodels.ps <- staR_MM_PVals(mixedmodels.summaries, iDesign, 0.05)
    #mixedmodels.pVals <- mixedmodels.ps[[1]]
    #mixedmodels.pSignificants <- mixedmodels.ps[[2]]
    
    # -- Plot Stats --
    plot(unlist(lapply(mixedmodels.summary, FUN = function(x) {x$p.value})), type="l")
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
  if(bOnlyFullAnalysis == FALSE)
  {
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
        #hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[3]][[3]]), size = "last")
        print("Modify me... Utilizing the first graph of D3.")
        hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[3]][[1]]), size = "last")
      }
    }
    
    if(length(hRows) == 1) 
    {
      grid.arrange(hRows[[1]], top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    
    if(length(hRows) == 2) 
    {
      grid.arrange(hRows[[1]], hRows[[2]], top=textGrob(staR_getDesignName(iDesignb, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    
    if(length(hRows) == 3) 
    {
      grid.arrange(hRows[[1]], hRows[[2]], hRows[[3]], top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/Data_", i, ".pdf", sep = ""))
      dev.off()
    }
  }
  
  ##################################
  ###### PLOT COMPLEX DESIGN #######
  ##################################
  if(iDesign >= 11 && iDesign <= 19)
  {
    if(length(hStats[[3]]) == 1)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 2)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 3)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 4)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc), gp=gpar(fontsize=20,font=3)))
    }
    
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_", i, ".pdf", sep = ""))
      dev.off()
    }
  }
}