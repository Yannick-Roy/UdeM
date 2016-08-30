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
source("StaR_LoadData.R")
source("StaR_Stats_lme.R")

nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

data.domain = "ALL"
data.type = "N/A"

##---------------------------------------------------------
#-- Select From Here to Re-Run, Simply Modify the Flags. --
#----------------------------------------------------------
bLoadMatlabFile = TRUE
bPrepMatlabData = TRUE

bFullDomains = TRUE

bSmallSamples = FALSE

bSaveOnDiskImages = FALSE
bSaveOnDiskData_R = TRUE
bSaveOnDiskData_Matlab = TRUE

# Get Design String, from Vars.
vars<- c("domains", "groups", "sessions", "orders", "motions")

# NbDomains
nbDomains = 3

fullDomains <- list()

# Correction Functions
stats.correctionFunction.PointByPoint = "fdr"
stats.correctionFunction.PostHoc = "bon"

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")

OS = 1   # 1 - Mac / 2 - Linux / 3 - Windows

dirPlotsPath <- ""
dirMatlabExportPath <- ""
if(OS == 1) { 
   # --- MAC Paths ---
   dirMatlabExportPath <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/"
   dirPlotsPath <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/Star Images/"
} else if(OS == 2) {
   # --- Linux Paths ---
   dirMatlabExportPath <- "/media/user/BrainData/Study_mTBI_x/MPT_Export/"
   dirPlotsPath <- "/media/user/BrainData/Study_mTBI_x/MPT_Export/Star Images/"
} else if(OS == 3) {
   # --- Windows Paths ---
   dirMatlabExportPath <- "E:/Study_mTBI_x/MPT_Export/"
   dirPlotsPath <- "E:/Study_mTBI_x/MPT_Export/Star Images/"
}

#**************************************************************************
#***** Main Loop (ERP & ERSP) !
#**************************************************************************
#for(curAnalysis in 1:2)
curAnalysis = 1
{
   for(domainNo in 1:(nbDomains + 1))
   {
      data.domain = domainNo
      
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
      dirPlots <- dirPlotsFullPath
      
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
         if(bLoadMatlabFile && data.domain <= nbDomains)
         {
            # Read Matlab file !
            data.file = paste(dirMatlabExportPath, "MPT_exp_", tolower(data.type), "_D", data.domain, ".mat", sep="")
            
            print("Matlab Data - Loading...")
            matlabData <- staR_fillFromMatlab(data.file, "MPT", fullDataStructure, bSmallSamples = bSmallSamples, dataType = data.type, domain = data.domain)
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
            
            fullDomains[[data.domain]] = fullData
         } 
         
         if(bFullDomains && domainNo > nbDomains)
         {
            fullDomains.tmp <- list()
            for(p in 1:length(fullDomains[[1]]))
            {
               fullDomains.tmp [[p]] = rbind(fullDomains[[1]][[p]], fullDomains[[2]][[p]], fullDomains[[3]][[p]])
               
               if(p %% 100 == 0)
                 print(p)
            }
            
            fullData = fullDomains.tmp
            
            data.domain = "X"
         }
         
         # Mainly because of Small Samples. But anyway nbPoints can't be different from fullData.
         nbPoints = length(fullData)
         ersp_dims <- c(length(timeData), length(freqData))
         
         # Prep Plot Series !
         # hAnalysisTitles <- getAnalysisTitles(dirPlots, iDesign, data.domain, stats.function, stats.bCorrection, stats.correctionFunction, sigthreshold, bSaveOnDiskImages)
         # TODO : FIX ME ^   
         
         #************************************************************
         ###### Stats !
         #************************************************************      
         # Remove "domains" from the list for single domain, one by one.
         if(data.domain < nbDomains){
            tmpVars = vars[vars != "domains"]
         }else{
            tmpVars = vars  
         }
         
         # Create "formula" for lme design.
         fixedVars <- paste("values ~ ", paste(tmpVars, collapse=" * "))
         
         # lme()
         stats.fullAnalysis.lme.retVal <- staR_lme2(fullData, tmpVars, fixedVars)
         
         # Split retVal - lme report.
         stats.fullAnalysis.lme.report <- stats.fullAnalysis.lme.retVal[[1]][[1]]
         
         # Split retVal - Full Analysis (effects / interactions).
         stats.fullAnalysis.pVals <- stats.fullAnalysis.lme.retVal[[1]][[2]][[1]]
         stats.fullAnalysis.pTitles <- stats.fullAnalysis.lme.retVal[[1]][[2]][[2]]
         
         # Split retVal - Sub Analysis (single Post Hoc).
         stats.subAnalysis.pVals <- stats.fullAnalysis.lme.retVal[[2]][[1]]
         stats.subAnalysis.pTitles <- stats.fullAnalysis.lme.retVal[[2]][[2]]
         #stats.subAnalysis.pVals <- pVals
         #stats.subAnalysis.pTitles <- pTitles
         
         #************************************************************
         ###### Custom Post Hoc(s)
         #************************************************************   
         #mainTitle = "My Custom Post Hoc"
         #plotValues = stats.subAnalysis.pVals
         #plotTitles = stats.subAnalysis.pTitles
         #staR_saveCustom(dirPlots, "StaR_Custom2.mat", data.type, timeData, freqData, plotValues, plotTitles, mainTitle, nbRows = 1, nbCols = 2)
         
         #************************************************************
         ###### Pvals - Correction !
         #************************************************************
         stats.fullAnalysis.pVals.Corrected <- list()
         for(i in 1:length(stats.fullAnalysis.pVals))
         {
            stats.fullAnalysis.pVals.Corrected[[i]] <- p.adjust(unlist(stats.fullAnalysis.pVals[[i]]), stats.correctionFunction.PointByPoint) 
         }
         
         stats.fullAnalysis.pVals.Original <- stats.fullAnalysis.pVals
         stats.fullAnalysis.pVals <- stats.fullAnalysis.pVals.Corrected
         
         for(i in 1:length(stats.fullAnalysis.pVals.Corrected))
         {
            print(paste("Graph ==>", stats.fullAnalysis.pTitles[[i]], " max :", max(unlist(stats.fullAnalysis.pVals.Corrected[[i]])), " min :", min(unlist(stats.fullAnalysis.pVals.Corrected[[i]]))))
         }
         
         #************************************************************
         ###### Get Data! (sub plots from data)
         #************************************************************
         subDataset <- staR_selectData(fullData, stats.subAnalysis.pTitles)
         
         subDistrib <- staR_getDistParams(subDataset)
         
         #************************************************************
         ###### Save Data! 
         #************************************************************
         # Save on disk.
         if(bSaveOnDiskData_R)
         {
            print("Saving R Workspace...")
            
            #save(fullData, timeData, freqData, subData, subDataset, 'designName', 'paramsList', stats.fullAnalysis.pVals, stats.fullAnalysis.pVals.Original, stats.fullAnalysis.pTitles, stats.subAnalysis.pVals, stats.subAnalysis.pVals.Original, stats.subAnalysis.pTitles, file = paste(dirPlots, "/Workspace_Fullx.RData", sep=""))
            
            print("Done!")
         }
         
         if(bSaveOnDiskData_Matlab)
         {
            print("Saving Matlab Workspace...")
            
            #writeMat(paste(dirPlots, "/Workspace_D", data.domain, ".mat", sep=""), domainData = data.domain, typeData = data.type, timeData = timeData, freqData = freqData, designName = 'designName', pValsFull = unlist(stats.fullAnalysis.pVals.Original), pValsFullCorrected = unlist(stats.fullAnalysis.pVals), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pValsSub = unlist(stats.subAnalysis.pVals.Original), pValsSubCorrected = unlist(stats.subAnalysis.pVals), pTitlesSub = unlist(pTitles), 'dataSub' = unlist(subDistrib), 'titlesSub' = unlist(stats.subAnalysis.pTitles))
            writeMat(paste(dirPlots, "/Workspace_D", data.domain, ".mat", sep=""), domainData = data.domain, typeData = data.type, timeData = timeData, freqData = freqData, designName = 'designName', pValsFull = unlist(stats.fullAnalysis.pVals.Original), pValsFullCorrected = unlist(stats.fullAnalysis.pVals), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pValsSub = unlist(pVals), pValsSubCorrected = unlist(pVals), pTitlesSub = unlist(pTitles), 'dataSub' = unlist(subDistrib), 'titlesSub' = unlist(stats.subAnalysis.pTitles))
            
            print("Done!")
         }
         
         ##### POST HOC #####
         
         varList <- list()
         #varList[[1]] = list("groups", "orders", "motions", "sessions")
         #varList[[1]] = list(c("groups", "orders"), c("groups", "motions"))#, c("groups", "sessions"))
         #varList[[1]] = list(c("groups", "orders", "sessions"), c("groups", "motions", "sessions"))
         varList[[1]] = list(c("groups", "orders", "motions", "sessions"))
         varSortedBy <- "domains"
         
         for (l in 1:length(varList))
         {
            for (v in 1:length(varList[[l]]))
            {
               print(varList[[l]][[v]])
               retVal <- StaR_PostHoc_do(varList[[l]][[v]], varSortedBy)
               
               pVals = retVal[[1]]
               pTitles = retVal[[2]]
               
               fileVarString <- paste(varList[[l]][[v]], collapse = "_")
               writeMat(paste(dirPlots, "/Workspace_PH_", fileVarString, ".mat", sep=""), domainData = data.domain, typeData = data.type, timeData = timeData, freqData = freqData, designName = 'designName', pValsFull = unlist(stats.fullAnalysis.pVals.Original), pValsFullCorrected = unlist(stats.fullAnalysis.pVals), pTitlesFull = unlist(stats.fullAnalysis.pTitles), pValsSub = unlist(pVals), pValsSubCorrected = unlist(pVals), pTitlesSub = unlist(pTitles), 'dataSub' = unlist(subDistrib), 'titlesSub' = unlist(stats.subAnalysis.pTitles))
            }
         }
      }, error = function(e) {
         print(e)
      })
   }
}
# 
# # -- Sub Analysis
# stats.subAnalysis.pVals.Corrected <- list()
# for(curEffectLevel in 1:length(stats.subAnalysis.pVals)) # Vertical or Horizontal ?
# {
#    stats.subAnalysis.pVals.Corrected[[curEffectLevel]] <- list()
#    for(i in 1:length(stats.subAnalysis.pVals[[curEffectLevel]])) # Layers
#    {
#       stats.subAnalysis.pVals.Corrected[[curEffectLevel]][[i]] <- p.adjust(unlist(stats.subAnalysis.pVals[[curEffectLevel]][[i]]), stats.correctionFunction.PointByPoint) 
#    }   
# }           
# 
# stats.subAnalysis.pVals.Original <- stats.subAnalysis.pVals
# stats.subAnalysis.pVals <- stats.subAnalysis.pVals.Corrected

print(" -- Done! Have a good day! --")