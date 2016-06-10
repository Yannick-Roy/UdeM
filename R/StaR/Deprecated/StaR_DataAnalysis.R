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
source("StaR_Anovas.R")
source("StaR_MixedModels.R")
source("StaR_PlotStats.R")

designs = 31#c(2,3,4,5,11,12,13,14,15,16,17,18)

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = FALSE

bLoadMatlabFile = TRUE
bPrepMatlabData = TRUE

bSampleMatlabData = TRUE
bSmallSamples = TRUE

bSaveOnDiskImages = TRUE
bSaveOnDiskData = TRUE

bOnlyFullAnalysis = FALSE

ersp_dims <- c(400, 135) # Default
nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

stats.function = "t-test"

#######################
# Stats !
#######################
bAnova = FALSE
bMixedModels = FALSE
bTTests = TRUE

#AnovaFunc = 'aov' # ez, aov, anova
#MMFunc = 'lme' # lmer, lme
#######################

# Clear Plots.
#dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlotsFullPath <- paste(dirPlotsPath, dirPlotsName, sep = "")
dir.create(dirPlotsFullPath)

hTitles <- list()  

#save(fullData, timeData, freqData, subDataset, subData, paramsList, anovas.summaries, anovas.pVals, anovas.pSignificants,  file = "RWorkspaceVariables.RData")
for(curAnalysis in 1:2)
{
  if(curAnalysis == 1) # ERP
  {
    bERSP = FALSE
    bERP = TRUE 
  }
  if(curAnalysis == 2) # ERSP
  {
    bERSP = TRUE
    bERP = FALSE
  }
  
  if(bERSP)
  {
    nbPoints = 54000
  }
  if(bERP)
  {
    nbPoints = 1536
  }
  
  ########################################################################
  ################################ DATA ################################## 
  ########################################################################
  # Formatting the "Wide structure" for stats.
  if(bPrepMatlabData)
  {
    fullData <- staR_prepData()
  }
  
  # Fill "Wide structure" with real data.
  if(bLoadMatlabFile)
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
    fullData.original = fullData
    
    if(length(matlabData) >= 3)
    {if(length(matlabData[[2]]) >= 1)
    {timeData = matlabData[[2]][1,]}}
    
    if(length(matlabData) >= 3)
    {if(length(matlabData[[3]]) >= 1)
    {freqData = matlabData[[3]][1,]}}
  } 
  
  if(bSmallSamples)
  {
    if(bERP)
    {
      lowLimit <- which(timeData > 0)[1]
      highLimit <- which(timeData < 600)
      highLimit <- highLimit[length(highLimit) - 1]
      
      fullData <- fullData[lowLimit:highLimit]
      timeData <- timeData[lowLimit:highLimit]
    }
    
    #freqData
    #timeVals <- timeVals[lowLimit:highLimit]
    if(bERSP)
    {
      # Time
      lowLimit_time <- which(timeData > 50)[1] # From 50ms
      highLimit_time <- which(timeData < 400)  # To 500ms
      highLimit_time <- highLimit_time[length(highLimit_time) - 1]
      
      # Freq
      lowLimit_freq <- which(freqData > 10)[1] # From 10Hz
      highLimit_freq <- which(freqData < 13)   # To 15Hz
      highLimit_freq <- highLimit_freq[length(highLimit_freq) - 1]
      
      # TODO : Fix this, not the real "sub squarre in the matrix!"
      dataIndices <- NULL
      for(i in lowLimit_freq:highLimit_freq)
      {
        #print(paste("lowLimit_time : ", lowLimit_time))
        #print(paste("lowLimit_time : ", highLimit_time))
        #print(paste("length(timeData)", length(timeData)))
        dataIndices <- c(dataIndices, seq(i * length(timeData) + lowLimit_time, i * length(timeData) + lowLimit_time + (highLimit_time - lowLimit_time)))
      }
      
      # Sub Time & Freq
      timeData <- timeData[lowLimit_time:highLimit_time]
      freqData <- freqData[lowLimit_freq:highLimit_freq]
      fullData <- fullData[dataIndices]
    }
    
    print(paste("Data subsampled ! ", length(fullData)))
  }
  
  # Mainly because of Small Samples. But anyway nbPoints can't be different from fullData.
  nbPoints = length(fullData)
  ersp_dims <- c(length(timeData), length(freqData))
  
  #############################################################
  ###### Loop for Designs !
  #############################################################
  for(curDesign in designs)
  {
    iDesign = curDesign
    #iDesign = designs
    dirPlots <- paste(dirPlotsFullPath, "/Design_NA_", iDesign, sep = "")
    if(bERP){dirPlots <- paste(dirPlotsFullPath, "/Design_ERP_", iDesign, sep = "")}
    if(bERSP){dirPlots <- paste(dirPlotsFullPath, "/Design_ERSP_", iDesign, sep = "")}
    dir.create(dirPlots)
    
    # Prep Plot Series !
    grid.arrange(textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc = "lme"), gp=gpar(fontsize=30)))
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/Title_", iDesign, ".pdf", sep = ""))
      dev.off()
      Sys.sleep(5)
    }
    
    # Sub select, according to current stats.
    if(bReloadRData){
      print("Select Data from File...")
      #load
    } else {
      print("Select Data...")
      retVal <- staR_selectData(fullData, iDesign)
      subDataset = retVal[[1]]
      subData = retVal[[2]]
      if(bSaveOnDiskData)
      {
        #save(file = "RSelectedData")
      }
    }
    
    print("Get Params (mean, min, max, ...)")
    #paramsList <- staR_getDistParams(subData, timeData, iDesign)
    #if(bERSP) {timeVals <- seq(1, nbPoints)}
    timeVals <- timeData
    paramsList <- staR_getDistParams(subData, timeVals, iDesign)
    
    ########################################################################
    ################################ Stats ################################# 
    ########################################################################
    if(bTTests)
    {
      sigthreshold = 0.05
      
      # Get condition #1
      sub_ax <- lapply(fullData, subset, conditions == "SOM" & groups == "3")
      # Subset only the values
      sub_ax <- lapply(sub_ax, function(x) x$values)
      
      # Get condition #2
      sub_bx <- lapply(fullData, subset, conditions == "SOM" & groups == "4")
      # Subset only the values
      sub_bx <- lapply(sub_bx, function(x) x$values)
      
      ttests.pVals <- list()
      ttests.pVals[[1]] <- list()
      ttests.pVals[[2]] <- list()
      ttests.pVals[[3]] <- list()
      
      # T-Test
      ttests.tVals <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$statistic}, sub_ax, sub_bx)
      ttests.pVals[[3]][[1]] <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$'p.value'}, sub_ax, sub_bx)
      
      ttests.pSignificants <- ttests.pVals
      for(i in 1:length(ttests.pSignificants))
      {
        print(paste(length(ttests.pSignificants), i))
        if(length(ttests.pSignificants[[i]]) > 0)
        {
          for(j in 1:length(ttests.pSignificants[[i]]))
          {
            ttests.pSignificants[[i]][[j]][ttests.pSignificants[[i]][[j]] < sigthreshold] <- 0 #'Signif.'
            ttests.pSignificants[[i]][[j]][ttests.pSignificants[[i]][[j]] >= sigthreshold] <- 1 #'Non Signif.'
          }
        }
      }
      
      if(bERP) { hStatsResults <- plotStats(ttests.pSignificants, timeData, titles = NULL) }
      if(bERSP) { hStatsResults <- plotStats_ERSP(ttests.pSignificants, timeData, titles = NULL) }
      hStats <- hStatsResults[[1]]
      hStatsLayers <- hStatsResults[[2]]
    }
    
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
      anovas.pVals <- staR_PVals(anovas.summaries, iDesign, func = AnovaFunc)
      anovas.pSignificants <- staR_PSignificants(anovas.pVals, 0.05)
      
      # -- pVals Correction --
      #anovas.cps <- staR_FDR(anovas.pVals)
      #anovas.pSignificantsCorr <- staR_PSignificants(anovas.pVals, 0.05)
      
      # -- post-hoc Comparaison --
      #TukeyHSD(anovas.all)
      
      # -- Plot Stats --
      #plot(unlist(anovas.pVals[[1]][[1]]), type="l")
      if(bERP) { hStats <- plotStats(anovas.pSignificants, timeData, titles = NULL)[[1]] }
      if(bERSP) { hStats <- plotStats_ERSP(anovas.pSignificants, timeData, titles = NULL)[[1]] }
      
      #if(bERP) { hStatsCorr <- plotStats(anovas.pSignificantsCorr, timeData, titles = NULL)[[1]] }
      #if(bERSP) { hStatsCorr <- plotStats_ERSP(anovas.pSignificantsCorr, timeData, titles = NULL)[[1]] }
    }  
    
    ##################################
    ########### PLOT DATA ############
    ##################################
    # -- Plot Data --
    if(bERP) { hData <- plotData(subData, paramsList, timeData) }
    if(bERSP) { hData <- plotData_ERSP(subData, paramsList, timeData) }
    # -- Plot Data --
    
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
        grid.arrange(hRows[[1]], top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows) == 2) 
      {
        grid.arrange(hRows[[1]], hRows[[2]], top=textGrob(staR_getDesignName(iDesignb, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows) == 3) 
      {
        grid.arrange(hRows[[1]], hRows[[2]], hRows[[3]], top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
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
    if((iDesign >= 11 && iDesign <= 19) || (iDesign >= 31 && iDesign <= 39))
    {
      if(length(hStats[[3]]) == 1)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 2)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 3)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 4)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), top=textGrob(staR_getDesignName(iDesign, bAnova, bMixedModels, Afunc = AnovaFunc, MMfunc = MMFunc), gp=gpar(fontsize=20,font=3)))
      }
      
      if(bSaveOnDiskImages)
      {
        dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_", i, ".pdf", sep = ""))
        dev.off()
      }
    }
  }
}