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

#**************************************************************************
#**************************       Sequence !     **************************
#**************************************************************************
source("StaR_Designs.R")
source("StaR_LoadData.R")
source("StaR_Plot.R")
source("StaR_Stats_aov.R")
source("StaR_Stats_lme.R")

designs = c(17)#,11,12,13,14,15,16,17,18)

bReloadRData = FALSE

bLoadMatlabFile = TRUE
bPrepMatlabData = TRUE

bSmallSamples = FALSE

bSaveOnDiskImages = TRUE
bSaveOnDiskData_R = TRUE
bSaveOnDiskData_Matlab = TRUE

bFullStatsAnalysis = TRUE   # Full report on all the data.
bSubDataAnalysis = TRUE     # Multiple plots with data.

ersp_dims <- c(400, 135) # Default
nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

data.domain = 1
data.type = "N/A"
stats.function = "lme" #"aov"
stats.bCompute = TRUE
stats.bCorrection = TRUE
stats.correctionFunction = "fdr"

bTempDisableSubAnalysis = FALSE

sigthreshold = 0.05

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
#dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
#dirPlotsPath <- "/media/user/Data/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlotsPath <- "~/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/"

#**************************************************************************
#***** Main Loop (ERP & ERSP) !
#**************************************************************************
#for(curAnalysis in 1:2)
curAnalysis = 2
{
  #for(domainNo in 1:3)
  domainNo = 1
  {
    data.domain = domainNo
    
    #for(analType in 1:2)
    analType = 1
    {
      if(analType == 1){stats.function = "lme"}
      if(analType == 2){stats.function = "aov"}
      
      #************************************************************
      ###### Data type !
      #************************************************************
      if(curAnalysis == 1) { data.type = "ERP" } #ERP
      if(curAnalysis == 2) { data.type = "ERSP" } # ERSP
      # TODO: Spectra / Power
      
      #************************************************************
      ###### Create Folder Structure!
      #************************************************************
      dirPlotsFullPath <- paste(dirPlotsPath, dirPlotsName, sep = "")
      dir.create(dirPlotsFullPath)  
      dirPlotsFullPath <- paste(dirPlotsFullPath, "/", data.type, sep = "")
      dir.create(dirPlotsFullPath)  
      dirPlotsFullPath <- paste(dirPlotsFullPath, "/Domain_", data.domain, sep = "")
      dir.create(dirPlotsFullPath)  
      dirPlotsFullPath <- paste(dirPlotsFullPath, "/Stats_", stats.function, sep = "")
      dir.create(dirPlotsFullPath) 

      # Reset Variables
      stats.subAnalysis = NULL
      stats.subAnalysis.pVals = NULL
      stats.subAnalysis.pVals.Corrected = NULL
      stats.subAnalysis.pVals.Original = NULL
      stats.subAnalysis.pTitles = NULL
      stats.subAnalysis.pSignif = NULL
      stats.subAnalysis.pSignificant = NULL
      stats.subAnalysis.pSignificants = NULL
      stats.subAnalysis.aov.retVal = NULL
      
      stats.fullAnalysis = NULL
      stats.fullAnalysis.pVals = NULL
      stats.fullAnalysis.pVals.Corrected = NULL
      stats.fullAnalysis.pVals.Original = NULL
      stats.fullAnalysis.pTitles = NULL
      stats.fullAnalysis.pSignif = NULL
      stats.fullAnalysis.pSignificant = NULL
      
      tryCatch({
        #************************************************************
        ###### Prep Data !
        #************************************************************
        # Formatting the "Wide structure" for stats.
        # TODO : Allow to design study parameters here. (rather than to modify source code!)
        if(bPrepMatlabData) { fullDataStructure <- staR_prepData() }
        
        # Test Structure!
        write.csv(fullDataStructure, file = "StaR_WideStructure.csv")
        
        # Fill "Wide structure" with real data.
        if(bLoadMatlabFile)
        {
          # Read Matlab file !
            data.file = paste("/media/user/BrainData/Study_mTBI_x/MPT_Export/MPT_exp_", tolower(data.type), "_D", data.domain, ".mat", sep="")
            #data.file = paste("~/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/MPT_exp_", tolower(data.type), "_D", data.domain, ".mat", sep="")
          
          print("Matlab Data - Loading...")
          matlabData <- staR_fillFromMatlab(data.file, "MPT", fullDataStructure, bSmallSamples = bSmallSamples, dataType = data.type)
          print("Matlab Data - Done!")
          
          print("Matlab Data - Getting Time & Freq (if ERSP)")
          # Get Data.
          fullData = matlabData[[1]]
          fullData.original = fullData
          
          # Get Time.
          if(length(matlabData) >= 2)
          {if(length(matlabData[[2]]) >= 1)
          {timeData = matlabData[[2]]}}
          
          # Get Freq.
          if(length(matlabData) >= 3)
          {if(length(matlabData[[3]]) >= 1)
          {freqData = matlabData[[3]]}}
          print("Matlab Data - Done!")
        } 
        
        # Mainly because of Small Samples. But anyway nbPoints can't be different from fullData.
        nbPoints = length(fullData)
        ersp_dims <- c(length(timeData), length(freqData))
        
        #************************************************************
        ###### Loop for Designs !
        #************************************************************
        for(curDesign in designs)
        {
          iDesign = curDesign
          
          dirPlots <- paste(dirPlotsFullPath, "/Design_NA_", iDesign, sep = "")
          if(data.type == "ERSP") { 
            dirPlots <- paste(dirPlotsFullPath, "/Design_ERSP_", iDesign, sep = "") 
          } else { 
            dirPlots <- paste(dirPlotsFullPath, "/Design_ERP_", iDesign, sep = "") 
          }
          dir.create(dirPlots)
          
          # Prep Plot Series !
          hAnalysisTitles <- getAnalysisTitles(dirPlots, iDesign, data.domain, stats.function, stats.bCorrection, stats.correctionFunction, sigthreshold, bSaveOnDiskImages)
          
          if(bTempDisableSubAnalysis) {bSubDataAnalysis = TRUE}
          if(bSubDataAnalysis && length(STATS_SUB_DESIGNS[[iDesign]]) == 0)
          {
            print("======== Disabling Sub Analysis, the current design doesn't support it... =========")
            bSubDataAnalysis = FALSE
            bTempDisableSubAnalysis = TRUE # To re-enable SubDataAnalysis automatically after this design.
          }
          
          #************************************************************
          ###### Select sub data & params !
          #************************************************************
          print("Select Data...")
          retVal <- staR_selectData(fullData, iDesign)
          subDataset = retVal[[1]]
          subData = retVal[[2]]
          subData.Titles = retVal[[3]]
          print("Done !")
          
          paramsList <- list()
          if(length(subData) > 0)
          {
            print("Get Params (mean, min, max, ...)")
            paramsList <- staR_getDistParams(subData, timeData, iDesign)
            print("Done !")
          }
          
          #************************************************************
          ###### Stats !
          #************************************************************          
          if(stats.bCompute)
          {
            if(stats.function == "lme")
            {
              if(bFullStatsAnalysis)
              {
                stats.fullAnalysis.lme.retVal <- staR_lme(fullData, iDesign)
                
                # TODO: Save also the report... It's not just about the pVals!
                stats.fullAnalysis.pVals <- stats.fullAnalysis.lme.retVal[[1]]
                stats.fullAnalysis.pTitles <- stats.fullAnalysis.lme.retVal[[2]]
              }
              
              if(bSubDataAnalysis)
              {
                stats.subAnalysis.lme.retVal <- staR_lme_sub(subDataset, iDesign, subData.Titles)
                
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
                stats.subAnalysis.aov.retVal <- staR_aov_sub(subDataset, iDesign, subData.Titles)
                
                stats.subAnalysis.pVals <- stats.subAnalysis.aov.retVal[[1]]
                stats.subAnalysis.pTitles <- stats.subAnalysis.aov.retVal[[2]]
              }
            }
            
            #************************************************************
            ###### Pvals - Correction !
            #************************************************************
            if(stats.bCorrection)
            {
              if(bFullStatsAnalysis)
              {
                stats.fullAnalysis.pVals.Corrected <- list()
                for(i in 1:length(stats.fullAnalysis.pVals))
                {
                  stats.fullAnalysis.pVals.Corrected[[i]] <- p.adjust(unlist(stats.fullAnalysis.pVals[[i]]), stats.correctionFunction) 
                }
                
                stats.fullAnalysis.pVals.Original <- stats.fullAnalysis.pVals
                stats.fullAnalysis.pVals <- stats.fullAnalysis.pVals.Corrected
                
                ## Debug
                for(i in 1:length(stats.fullAnalysis.pVals.Corrected))
                {
                  print(paste("Graph ==>", stats.fullAnalysis.pTitles[[i]], " max :", max(unlist(stats.fullAnalysis.pVals.Corrected[[i]])), " min :", min(unlist(stats.fullAnalysis.pVals.Corrected[[i]]))))
                }
              }
              
              if(bSubDataAnalysis)
              {
                stats.subAnalysis.pVals.Corrected <- list()
                for(curDim in 1:length(stats.subAnalysis.pVals)) # Vertical or Horizontal ?
                {
                  stats.subAnalysis.pVals.Corrected[[curDim]] <- list()
                  for(i in 1:length(stats.subAnalysis.pVals[[curDim]])) # Layers
                  {
                    stats.subAnalysis.pVals.Corrected[[curDim]][[i]] <- list()
                    for(j in 1:length(stats.subAnalysis.pVals[[curDim]][[i]])) # Stats (combined on row or col or 3D)
                    {
                      stats.subAnalysis.pVals.Corrected[[curDim]][[i]][[j]] <- p.adjust(unlist(stats.subAnalysis.pVals[[curDim]][[i]][[j]]), stats.correctionFunction) 
                    }
                  }   
                }           
                
                stats.subAnalysis.pVals.Original <- stats.subAnalysis.pVals
                stats.subAnalysis.pVals <- stats.subAnalysis.pVals.Corrected
                
                ## Debug
                for(curDim in 1:length(stats.subAnalysis.pVals.Corrected))
                {
                  for(i in 1:length(stats.subAnalysis.pVals.Corrected[[curDim]]))
                  {
                    for(j in 1:length(stats.subAnalysis.pVals.Corrected[[curDim]][[i]]))
                    {
                      print(paste("Graph ==>", stats.subAnalysis.pTitles[[curDim]][[i]][[j]], " max :", max(unlist(stats.subAnalysis.pVals.Corrected[[curDim]][[i]][[j]])), " min :", min(unlist(stats.subAnalysis.pVals.Corrected[[curDim]][[i]][[j]]))))
                    }
                  }
                }
              }
            }
            
            #************************************************************
            ###### Pvals Signif ( < threshold ) !
            #************************************************************
            print(paste("Doing - PSignif."))
            
            ## TODO : if full analysis
            stats.fullAnalysis.pSignif <- stats.fullAnalysis.pVals
            for(i in 1:length(stats.fullAnalysis.pSignif))
            {
                stats.fullAnalysis.pSignif[[i]][stats.fullAnalysis.pSignif[[i]] < sigthreshold] <- 0 #'Signif.'
                stats.fullAnalysis.pSignif[[i]][stats.fullAnalysis.pSignif[[i]] >= sigthreshold] <- 1 #'Non Signif.'
            }
            stats.fullAnalysis.pSignificants <- stats.fullAnalysis.pSignif
            
            if(bSubDataAnalysis)
            {
              stats.subAnalysis.pSignif <- stats.subAnalysis.pVals
              
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
              
              stats.subAnalysis.pSignificants <- stats.subAnalysis.pSignif
            }
            print("Done!")
            
            #************************************************************
            ###### Pvals - Plot Raw !
            #************************************************************
            bShowPlot = TRUE
            if(bShowPlot)
            {
              doRawPlots(bFullStatsAnalysis, stats.fullAnalysis.pVals, stats.fullAnalysis.pTitles, bSubDataAnalysis, stats.subAnalysis.pVals, stats.subAnalysis.pTitles, sigthreshold, bSaveOnDiskImages, iDesign, dirPlots)
            }
            
            #save() # Save on disk.
            if(bSaveOnDiskData_R)
            {
              designName <- staR_getDesignName(iDesign, stats.function)
              print(paste("Saving ", designName, "..."))
              
              #save(fullData, timeData, freqData, subData, iDesign, paramsList, stats.fullAnalysis.pVals, stats.fullAnalysis.pSignificants, stats.fullAnalysis.pTitles, subData.Titles,  file = paste(dirPlots, "/Workspace_Full.RData", sep=""))
              save(fullData, timeData, freqData, subData, subDataset, iDesign, designName, paramsList, stats.fullAnalysis.pVals, stats.fullAnalysis.pVals.Original, stats.fullAnalysis.pTitles, stats.subAnalysis.pVals, stats.subAnalysis.pVals.Original, stats.subAnalysis.pTitles, file = paste(dirPlots, "/Workspace_Fullx.RData", sep=""))
            }
            
            if(bSaveOnDiskData_Matlab)
            {
              #writeMat(paste(dirPlots, "/Workspace_Fullx.mat", sep=""), fullData=fullData, timeData = timeData, freqData = freqData, subData = subData, subDataset = subDataset, iDesign = iDesign, designName = designName, paramsList = paramsList, pVals = stats.fullAnalysis.pVals, pSignificants = stats.fullAnalysis.pSignificants, pTitles = stats.fullAnalysis.pTitles)
              # paramlist -> subdata -> times | means | sdes | maxs | mins
              # mapply(as.matrix(subData[[1]][[1]][[1]])
              # writeMat(paste(dirPlots, "/Workspace_Fullx.mat", sep=""), timeData = timeData, freqData = freqData, data=mapply(as.matrix(subData[[1]][[1]][[1]]), FUN=unlist), iDesign = iDesign, designName = designName, pVals = unlist(stats.fullAnalysis.pVals), pSignificants = unlist(stats.fullAnalysis.pSignificants))
              
              #mList <- lapply(paramsList, FUN=function(x) x$means)
              #c <- sapply(subData[[1]][[1]], FUN = function(x) mapply(as.matrix(x), FUN=unlist), simplify="matrix")
              mList <- lapply(paramsList, FUN= function(x) lapply(x, FUN= function(x) lapply(x, FUN=function(x) x$means)))
              mUnlist <- unlist(mList)
              
              dataSub <- unlist(lapply(subData, FUN= function(x) lapply(x, FUN= function(x) lapply(x, FUN=function(x) x))))
              #writeMat(paste(dirPlots, "/Workspace_Fullx.mat", sep=""), timeData = timeData, freqData = freqData, means=mUnlist, iDesign = iDesign, designName = designName, pValsFull = unlist(stats.fullAnalysis.pVals), pSignificantsFull = unlist(stats.fullAnalysis.pSignificants), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pTitlesSub = unlist(stats.subAnalysis.pTitles), 'n' = n, 'd' = d)
              
              ############################
              # TEST MATLAB LINEARIZED !!!
              ############################
              n <- c()
              d <- c()
              tryCatch ({
                for(i in 1:length(paramsList))
                {
                  for(j in 1:length(paramsList[[i]]))
                  { 
                    for(k in 1:length(paramsList[[i]][[j]]))
                    {  
                      n <- c(n, subData.Titles[[i]][[j]][[k]])
                      d <- c(d, paramsList[[i]][[j]][[k]]$means)
                      #print(subData.Titles[[i]][[j]][[k]])
                      #print(paramsList[[i]][[j]][[k]]$means[[1]])
                    }
                  }
                }
              } , error = function(e) {
                print(paste("# ERRROR # ", error))
              })
              ############################
              ############################
              
              #writeMat(paste(dirPlots, "/Workspace_Fullx.mat", sep=""), typeData = data.type, timeData = timeData, freqData = freqData, iDesign = iDesign, designName = designName, means=mUnlist, pValsFull = unlist(stats.fullAnalysis.pVals), pSignificantsFull = unlist(stats.fullAnalysis.pSignificants), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pTitlesSub = unlist(stats.subAnalysis.pTitles), 'n' = n, 'd' = d)
              writeMat(paste(dirPlots, "/Workspace_Fullx.mat", sep=""), typeData = data.type, timeData = timeData, freqData = freqData, iDesign = iDesign, designName = designName, pValsFull = unlist(stats.fullAnalysis.pVals.Original), pValsFullCorrected = unlist(stats.fullAnalysis.pVals), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pValsSub = unlist(stats.subAnalysis.pVals.Original), pValsSubCorrected = unlist(stats.subAnalysis.pVals), pTitlesSub = unlist(stats.subAnalysis.pTitles), 'dataSub' = d, 'titlesSub' = n)
            }
            
            # -- Plot Stats --
            
            ########################
            ### Plot Full Analysis
            ########################
            #stats.fullAnalysis.pSignificants <- stats.fullAnalysis.pVals
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
          
          #************************************************************
          ###### Plot sub analysis (data & stats) !
          #************************************************************
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
                      
                      Sys.sleep(8)
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
            
            for(i in 1:length(hRows))
            {
              if(designMatrix$nbRow > 1 && stats.bCompute)
              {
                nbRows <- length(hRows[[i]])
                for(k in 1:designMatrix$nbCol)
                {
                  if(k > 1)
                  {
                    hRows[[i]][[nbRows + 1]] <- cbind(hRows[[i]][[nbRows + 1]], ggplotGrob(hStats[[2]][[1]][[k]]), size = "last")
                  } else {
                    hRows[[i]][[nbRows + 1]] <- ggplotGrob(hStats[[2]][[1]][[k]])
                  }
                }
                
                hRows[[i]][[nbRows + 1]] <- cbind(hRows[[i]][[nbRows + 1]], ggplotGrob(ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100)), size = "last")
              }
            
              if(length(hRows[[i]]) == 1) 
              {
                grid.arrange(hRows[[i]][[1]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
              }
              
              if(length(hRows[[i]]) == 2) 
              {
                grid.arrange(hRows[[i]][[1]], hRows[[i]][[2]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
              }
              
              if(length(hRows[[i]]) == 3) 
              {
                grid.arrange(hRows[[i]][[1]], hRows[[i]][[2]], hRows[[i]][[3]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
              }
              
              if(length(hRows[[i]]) == 4) 
              {
                grid.arrange(hRows[[i]][[1]], hRows[[i]][[2]], hRows[[i]][[3]], hRows[[i]][[4]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
              }
              
              if(bSaveOnDiskImages)
              {
                dev.copy2pdf(file = paste(dirPlots, "/Data_", i, ".pdf", sep = ""))
                dev.off()
              }
            }
          }
       
          #************************************************************
          ###### Plot full analysis !
          #************************************************************
          if(bFullStatsAnalysis && stats.bCompute)
          {
            if(length(stats.fullAnalysis.hPlots) == 1)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 2)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 3)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 4)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 5)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 6)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), ggplotGrob(stats.fullAnalysis.hPlots[[6]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
            }
            if(length(stats.fullAnalysis.hPlots) == 7)
            {
              grid.arrange(ggplotGrob(stats.fullAnalysis.hPlots[[1]]), ggplotGrob(stats.fullAnalysis.hPlots[[2]]), ggplotGrob(stats.fullAnalysis.hPlots[[3]]), ggplotGrob(stats.fullAnalysis.hPlots[[4]]), ggplotGrob(stats.fullAnalysis.hPlots[[5]]), ggplotGrob(stats.fullAnalysis.hPlots[[6]]), ggplotGrob(stats.fullAnalysis.hPlots[[7]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=10,font=3)))
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
              Sys.sleep(10)
            }
          }
        }
      }, error = function(e) {
        print(e)
        #print("====> ERROR, Skipping this one ! <=====")
        
        #fileConn<-file(paste(dirPlotsPath, dirPlotsName,"/Error.txt", sep = ""))
        #writeLines(c("====> ERROR, Skipping this one ! <=====", paste("Design :", iDesign), paste("Function :", stats.function), paste("Domain :", data.domain), e), fileConn)
        #close(fileConn)
      })
    }
  }
}

print(" -- Done! Have a good day! --")
