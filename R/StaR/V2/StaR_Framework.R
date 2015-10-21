# ERP Stuff...
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(lattice)
library(parallel)
library(ez)
library(stringr)
require(timeSeries)
require(reshape2)
require(tictoc)
#require(fdrtool)

stde <- function(x) sd(x)/sqrt(length(x))

###########################################################################
#########################       Sequence !     ############################
###########################################################################
#source("StaR_Anovas.R")
#source("StaR_MixedModels.R")
source("StaR_Designs.R")
source("StaR_LoadData.R")
source("StaR_Plot.R")
source("StaR_Stats_aov.R")
source("StaR_Stats_lme.R")

designs = 11#c(2,3,4,5,11,12,13,14,15,16,17,18)

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = FALSE

bLoadMatlabFile = TRUE
bPrepMatlabData = TRUE

bSmallSamples = TRUE

bSaveOnDiskImages = TRUE
bSaveOnDiskData = TRUE

bFullStatsAnalysis = TRUE   # Full report on all the data.
bSubDataAnalysis = TRUE     # Multiple plots with data.

ersp_dims <- c(400, 135) # Default
nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

data.type = "ERP"
stats.function = "lme"
stats.bCompute = TRUE

sigthreshold = 0.05

# Clear Plots.
#dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
#dirPlotsPath <- "/media/user/Data/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlotsFullPath <- paste(dirPlotsPath, dirPlotsName, sep = "")
dir.create(dirPlotsFullPath)

hTitles <- list()  

#############################################################
###### Main Loop (ERP & ERSP) !
#############################################################
#save(fullData, timeData, freqData, subDataset, subData, paramsList, anovas.summaries, anovas.pVals, anovas.pSignificants,  file = "RWorkspaceVariables.RData")
#for(curAnalysis in 1:2)
curAnalysis = 1
{
  if(curAnalysis == 1) # ERP
  {
    data.type = "ERP"
    
    nbPoints = 1536
  }
  if(curAnalysis == 2) # ERSP
  {
    data.type = "ERSP"
    
    nbPoints = 54000
  }
  
  #############################################################
  ###### Prep Data !
  #############################################################
  # Formatting the "Wide structure" for stats.
  if(bPrepMatlabData) { fullData <- staR_prepData() }
  
  # Fill "Wide structure" with real data.
  if(bLoadMatlabFile)
  {
    # Read Matlab file !
    if(data.type == "ERSP") { 
      data.file = "~/Documents/Playground/UdeM/RMatlab_Data/export_mpt.mat" 
    } else { 
        data.file = "~/Documents/Playground/UdeM/RMatlab_Data/export_mpt_erp_d1.mat" 
    }
    
    print("Matlab Data - Loading...")
    matlabData <- staR_fillFromMatlab(data.file, "MPT", fullData, nbPoints, bSmallSamples = bSmallSamples, dataType = data.type)
    print("Matlab Data - Done!")
    
    print("Matlab Data - Getting Time & Freq (if ERSP)")
    # Get Data.
    fullData = matlabData[[1]]
    fullData.original = fullData
    
    # Get Time.
    if(length(matlabData) >= 3)
    {if(length(matlabData[[2]]) >= 1)
    {timeData = matlabData[[2]][1,]}}
    
    # Get Freq.
    if(length(matlabData) >= 3)
    {if(length(matlabData[[3]]) >= 1)
    {freqData = matlabData[[3]][1,]}}
    print("Matlab Data - Done!")
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
    if(data.type == "ERSP") { 
      dirPlots <- paste(dirPlotsFullPath, "/Design_ERSP_", iDesign, sep = "") 
    } else { 
      dirPlots <- paste(dirPlotsFullPath, "/Design_ERP_", iDesign, sep = "") 
    }
    dir.create(dirPlots)
    
    # Prep Plot Series !
    grid.arrange(textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=30)))
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/Title_", iDesign, ".pdf", sep = ""))
      dev.off()
      Sys.sleep(5)
    }
    
    #############################################################
    ###### Select sub data & params !
    #############################################################
    print("Select Data...")
    retVal <- staR_selectData(fullData, iDesign)
    subDataset = retVal[[1]]
    subData = retVal[[2]]
    print("Done !")
    
    if(length(subData) > 0)
    {
      print("Get Params (mean, min, max, ...)")
      paramsList <- staR_getDistParams(subData, timeData, iDesign)
      print("Done !")
    }
    
    #############################################################
    ###### Stats !
    #############################################################
    if(stats.bCompute == TRUE)
    {
      if(stats.function == "lme")
      {
        if(bFullStatsAnalysis)
        {
          stats.fullAnalysis.lme.retVal <- staR_lme(fullData, iDesign)
          
          stats.fullAnalysis.pVals <- stats.fullAnalysis.lme.retVal[[1]]
          stats.fullAnalysis.pTitles <- stats.fullAnalysis.lme.retVal[[2]]
        }
        
        if(bSubDataAnalysis)
        {
          stats.subAnalysis.lme.retVal <- staR_lme_sub(subDataset, iDesign)
          
          stats.subAnalysis.pVals <- stats.subAnalysis.lme.retVal[[1]]
          stats.subAnalysis.pTitles <- stats.subAnalysis.lme.retVal[[2]]
        }
        
      } else if(stats.function == "aov")
      {
        if(bFullStatsAnalysis)
        {
          stats.fullAnalysis.aov.retVal <- staR_aov(fullData, iDesign)
        
          stats.fullAnalysis.pVals <- stats.fullAnalysis.aov.retVal[[1]]
          stats.fullAnalysis.pTitles <- stats.fullAnalysis.aov.retVal[[2]]
        }
        
        if(bSubDataAnalysis)
        {
          stats.subAnalysis.aov.retVal <- staR_aov_sub(subDataset, iDesign)
          
          stats.subAnalysis.pVals <- stats.subAnalysis.aov.retVal[[1]]
          stats.subAnalysis.pTitles <- stats.subAnalysis.aov.retVal[[2]]
        }
      }
      
      #############################################################
      ###### Pvals - Full Analysis !
      #############################################################
      bShowPlot = TRUE
      if(bShowPlot)
      {
        ###  --- Full Analysis ---
        if(bFullStatsAnalysis)
        {
          for(i in 1:length(stats.fullAnalysis.pVals))
          {
            plot(unlist(stats.fullAnalysis.pVals[[i]]), type="l", log="y", ylim = c(0.001, 1))
            title(main = stats.fullAnalysis.pTitles[[i]])
            abline(h = sigthreshold)
            
            if(bSaveOnDiskImages)
            {
              dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
              dev.off()
              Sys.sleep(10)
            }
          }
        }
        
        ###  --- Sub Analysis ---
        if(bSubDataAnalysis)
        {
          for(curDim in 1:length(stats.subAnalysis.pVals))
          {
            for(i in 1:length(stats.subAnalysis.pVals[[curDim]]))
            {
              for(j in 1:length(stats.subAnalysis.pVals[[curDim]][[i]]))
              {
                plot(unlist(stats.subAnalysis.pVals[[curDim]][[i]][[j]]), type="l", log="y", ylim = c(0.001, 1))
                title(main = stats.subAnalysis.pTitles[[curDim]][[i]][[j]])
                abline(h = sigthreshold)
                
                if(bSaveOnDiskImages)
                {
                  dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
                  dev.off()
                  Sys.sleep(10)
                }
              }
            }
          }
        }
      }
      
      #############################################################
      ###### Pvals Signif ( < threshold ) !
      #############################################################
      print(paste("Doing - PSignif."))
      
      ## TODO : if full analysis
      stats.fullAnalysis.pSignif <- stats.fullAnalysis.pVals
      for(i in 1:length(stats.fullAnalysis.pSignif))
      {
          stats.fullAnalysis.pSignif[[i]][stats.fullAnalysis.pSignif[[i]] < sigthreshold] <- 0 #'Signif.'
          stats.fullAnalysis.pSignif[[i]][stats.fullAnalysis.pSignif[[i]] >= sigthreshold] <- 1 #'Non Signif.'
      }
      stats.fullAnalysis.pSignificants <- stats.fullAnalysis.pSignif
      
      stats.subAnalysis.pSignif <- stats.subAnalysis.pVals
      if(bSubDataAnalysis)
      {
        for(curDim in 1:length(stats.subAnalysis.pSignif))
        {
          for(i in 1:length(stats.subAnalysis.pSignif[[curDim]]))
          {
            for(j in 1:length(stats.subAnalysis.pSignif[[curDim]][[i]]))
            {
              stats.subAnalysis.pSignif[[curDim]][[i]][[j]][stats.subAnalysis.pSignif[[curDim]][[i]][[j]] < sigthreshold] <- 0 #'Signif.'
              stats.subAnalysis.pSignif[[curDim]][[i]][[j]][stats.subAnalysis.pSignif[[curDim]][[i]][[j]] >= sigthreshold] <- 1 #'Non Signif.'
            }
          }
        }
      }
      stats.subAnalysis.pSignificants <- stats.subAnalysis.pSignif
      print("Done!")
      
      #save() # Save on disk.
      if(bFullStatsAnalysis)
      {
        save(fullData, timeData, freqData, subData, iDesign, paramsList, stats.fullAnalysis.pVals, stats.fullAnalysis.pSignificants, stats.fullAnalysis.pTitles, file = paste(dirPlots, "/Workspace_Full.RData", sep=""))
      }
      
      # -- Plot Stats --
      
      ########################
      ### Plot Full Analysis
      ########################
      #graphs15 <- lapply(mixedmodels.pSignificants[[3]], FUN = function(x, y, arg1){plotStats_ERP_Graph(yVal = unlist(x), yTitle = y, xTimeVals = arg1)}, arg1 = timeData)
      combinedValsTitles <- list()
      for(i in 1:length(stats.fullAnalysis.pSignificants)) {combinedValsTitles[[i]] <- list(stats.fullAnalysis.pSignificants[[i]], stats.fullAnalysis.pTitles[[i]])}
      
      if(data.type == "ERSP") {
        stats.fullAnalysis.ERSP <- lapply(combinedValsTitles, FUN = function(x, arg1, arg2){plotStats_ERSP_Graph(pVals = unlist(x[[1]]), pTitle = as.character(x[[2]]), xTimeVals = arg1, yFreqVals = arg2)}, arg1 = timeData, arg2 = freqData)
        stats.fullAnalysis.hPlots <- stats.fullAnalysis.ERSP #lapply(stats.fullAnalysis.ERSP, FUN = function(x) {x[[2]]})
      } else {
        stats.fullAnalysis.ERP <- lapply(combinedValsTitles, FUN = function(x, arg1){plotStats_ERP_Graph(yVal = unlist(x[[1]]), yTitle = as.character(x[[2]]), xTimeVals = arg1)}, arg1 = timeData)
        stats.fullAnalysis.hPlots <- lapply(stats.fullAnalysis.ERP, FUN = function(x) {x[[2]]})
      }
      
      
      ########################
      ### Plot Sub Analysis
      ########################
      if(bSubDataAnalysis)
      {
        #plot(unlist(lapply(mixedmodels.summary, FUN = function(x) {x$p.value})), type="l")
        if(data.type == "ERSP") {
          hStats <- plotStats_ERSP(stats.subAnalysis.pSignificants, stats.subAnalysis.pTitles, timeData, freqData, ersp_dims)
        } else {
          hStats <- plotStats_ERP(stats.subAnalysis.pSignificants, timeData, stats.subAnalysis.pTitles)[[1]]
        }
        
        #if(data.type == "ERSP") { hStats <- plotStats_ERSP(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle, ersp_dims)}
        #else { hStats <- plotStats_ERP(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]] }
      }
    }      
    
    #############################################################
    ###### Plot sub analysis (data & stats) !
    #############################################################
    # -- Plot Data --
    if(bSubDataAnalysis)
    {
      if(data.type == "ERSP") { 
        hData <- plotData_ERSP(subData, paramsList, timeData, ersp_dims) 
      } else { 
        hData <- plotData_ERP(subData, paramsList, timeData) 
      }
      
      if(length(hData) > 0)
      {
        for(i in 1:length(hData))
        {
          for(j in 1:length(hData[[i]]))
          {
            for(k in 1:length(hData[[i]][[j]]))
            {
              plot(hData[[i]][[j]][[k]])
              
              if(bSaveOnDiskImages)
              {
                dev.copy2pdf(file = paste(dirPlots, "/DataPlots_", i,"_",j,"_",k, ".pdf", sep = ""))
                dev.off()
                
                Sys.sleep(10)
              }
            }
          }
        }
      }
    
      # -- Plot All --
      designMatrix <- staR_getDesignMatrix(iDesign)
      
      hRows <- list()
      for(i in 1:designMatrix$nbLayer)
      {
        hRows[[i]] <- list()
        for(j in 1:designMatrix$nbRow)
        {
          for(k in 1:designMatrix$nbCol)
          {
            if(k > 1)
            {
              hRows[[i]][[j]] <- cbind(hRows[[i]][[j]], ggplotGrob(hData[[i]][[j]][[k]]), size = "last")
            } else {
              hRows[[i]][[j]] <- ggplotGrob(hData[[i]][[j]][[k]])
            }
          }
        
          if(stats.bCompute) # Don't add stat graph if there aren't being computed!
          {
            hRows[[i]][[j]] <- cbind(hRows[[i]][[j]], ggplotGrob(hStats[[1]][[i]][[j]]), size = "last")
          }
        }
      }
      
      if(designMatrix$nbRow > 1 && stats.bCompute)
      {
        nbRows <- length(hRows[[1]])
        for(k in 1:designMatrix$nbCol)
        {
          if(k > 1)
          {
            hRows[[1]][[nbRows + 1]] <- cbind(hRows[[1]][[nbRows + 1]], ggplotGrob(hStats[[2]][[1]][[k]]), size = "last")
          } else {
            hRows[[1]][[nbRows + 1]] <- ggplotGrob(hStats[[2]][[1]][[k]])
          }
        }
        
        hRows[[1]][[nbRows + 1]] <- cbind(hRows[[1]][[nbRows + 1]], ggplotGrob(ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100)), size = "last")
        
#         if(iDesign >= 11 && iDesign <= 16)
#         {
#           #hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[3]][[3]]), size = "last")
#           print("Modify me... Utilizing the first graph of D3.")
#           hRows[[nbRows + 1]] <- cbind(hRows[[nbRows + 1]], ggplotGrob(hStats[[3]][[1]]), size = "last")
#         }
        
      }
      
      if(length(hRows[[1]]) == 1) 
      {
        grid.arrange(hRows[[1]][[1]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows[[1]]) == 2) 
      {
        grid.arrange(hRows[[1]][[1]], hRows[[1]][[2]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows[[1]]) == 3) 
      {
        grid.arrange(hRows[[1]][[1]], hRows[[1]][[2]], hRows[[1]][[3]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows[[1]]) == 4) 
      {
        grid.arrange(hRows[[1]][[1]], hRows[[1]][[2]], hRows[[1]][[3]], hRows[[1]][[4]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(bSaveOnDiskImages)
      {
        dev.copy2pdf(file = paste(dirPlots, "/Data_", i, ".pdf", sep = ""))
        dev.off()
      }
    }
    
    #############################################################
    ###### Plot full analysis !
    #############################################################
    if(bFullStatsAnalysis && stats.bCompute)
    {
      if(length(stats.fullAnalysis.hPlots) == 1)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 2)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 3)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 4)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 5)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 6)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), ggplotGrob(stats.fullAnalysis.hPlots[[6]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 7)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), ggplotGrob(stats.fullAnalysis.hPlots[[6]]), ggplotGrob(stats.fullAnalysis.hPlots[[7]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(length(stats.fullAnalysis.hPlots) == 15)
      {
        grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), ggplotGrob(stats.fullAnalysis.hPlots[[6]]), ggplotGrob(stats.fullAnalysis.hPlots[[7]]), 
                     ggplotGrob(stats.fullAnalysis.hPlots[[8]]), ggplotGrob(stats.fullAnalysis.hPlots[[9]]), ggplotGrob(stats.fullAnalysis.hPlots[[10]]), ggplotGrob(stats.fullAnalysis.hPlots[[11]]), ggplotGrob(stats.fullAnalysis.hPlots[[12]]), ggplotGrob(stats.fullAnalysis.hPlots[[13]]), ggplotGrob(stats.fullAnalysis.hPlots[[14]]), ggplotGrob(stats.fullAnalysis.hPlots[[15]]),
                     top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=14,font=3)))
      }
      if(bSaveOnDiskImages)
      {
        dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_D", iDesign, "_", i, ".pdf", sep = ""))
        dev.off()
        Sys.sleep(20)
      }
    }
  }
}