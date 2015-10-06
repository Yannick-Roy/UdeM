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

designs = 1#c(1,2,3,4)#,5,11,12,13) #,14,15,16,17,18)

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = FALSE
bReloadMatlabFile = TRUE
bReprepMatlabData = TRUE
bSaveOnDiskImages = TRUE
bSaveOnDiskData = FALSE

bSmallSamples = FALSE

bOnlyFullAnalysis = TRUE

# Clear Plots.
#dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlotsFullPath <- paste(dirPlotsPath, dirPlotsName, sep = "")
dir.create(dirPlotsFullPath)

hTitles <- list()  

#save(fullData, timeData, freqData, subDataset, subData, paramsList, anovas.summaries, anovas.pVals, anovas.pSignificants,  file = "RWorkspaceVariables.RData")
for(curDesign in designs)
{
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
    
    nbPoints = 0
    if(bERSP)
    {
      nbPoints = 54000
    }
    if(bERP)
    {
      nbPoints = 1536
    }
    
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
    
    ########################################################################
    ################################ DATA ################################## 
    ########################################################################
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
    
    ########################################################################
    ################################ Stats ################################# 
    ########################################################################
    print("Get Params (mean, min, max, ...)")
    #paramsList <- staR_getDistParams(subData, timeData, iDesign)
    #if(bERSP) {timeVals <- seq(1, nbPoints)}
    timeVals <- timeData
    paramsList <- staR_getDistParams(subData, timeVals, iDesign)
    
    
    # mixedmodels.all = 3D; H | V | Full
    hTitles[[3]] <- "Mixed Models..."
    
    #mixedmodels.all <- staR_MMlme(fullData = fullData, subData = subDataset, iDesign = iDesign)
    if(FALSE)
    {
      cl <- makeCluster(8) 
      clusterExport(cl, list("lme", "STATS_DESIGNS", "STATS_DESIGNS_RND", "iDesign"))
      
      tic()
      print(paste("Doing - lme (fullData) : ", format(STATS_DESIGNS[[iDesign]]), " random=", format(STATS_DESIGNS_RND)))
      
      mixedmodels.full <- parLapply(cl = cl, fullData, fun = function(x) {lme(STATS_DESIGNS[[iDesign]], random=STATS_DESIGNS_RND, x)})
      toc()
      
      stopCluster(cl)
    }else{
      mixedmodels.full <- lapply(fullData, FUN = function(x) {lme(STATS_DESIGNS[[iDesign]], random=STATS_DESIGNS_RND, x)})
    }
    
    mixedmodels.subH <- list()
    mixedmodels.subV <- list()
    
    print("Done!")
    
    mixedmodels.all <- list(mixedmodels.subH, mixedmodels.subV, mixedmodels.full)
    
    #save() # Save on disk.
    
    # -- Summary --
    mixedmodels.summary <- list()
    mixedmodels.summary[[1]] <- list()
    mixedmodels.summary[[2]] <- list()
    mixedmodels.summary[[3]] <- list()
    # mixedmodels.summary = 3D; H | V | Full
    mixedmodels.summaryTemp <- lapply(mixedmodels.all[[3]], anova)   
    mixedmodels.summary[[3]][[1]] <- mixedmodels.summaryTemp
  
    # -- pVals --    
    print("... lme ...")
    mixedmodels.pVals <- list()
    mixedmodels.pValsTitle <- list()
    mixedmodels.pVals[[3]] <- list()
    mixedmodels.pValsTitle[[3]] <- list()
    n = length(mixedmodels.summary[[3]][[1]][[1]]$`p-value`)
    for(i in 1:(n - 1))
    {
      mixedmodels.pVals[[3]][[i]] <- lapply(mixedmodels.summary[[3]][[1]], FUN = function(x) {x$'p-value'[[i+1]]})
      mixedmodels.pValsTitle[[3]] <- row.names(mixedmodels.summary[[3]][[1]][[1]])[2:n]
    }
    print("... done ...")
    
    sigthreshold = 0.05
    
    bShowPlot = TRUE
    if(bShowPlot)
    {
      for(i in 1:length(mixedmodels.pVals[[3]]))
      {
        plot(unlist(mixedmodels.pVals[[3]][[i]]), type="l", log="y", ylim = c(0.001, 1))
        title(main = mixedmodels.pValsTitle[[3]][[i]])
        abline(h = sigthreshold)
        
        if(bSaveOnDiskImages)
        {
          dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
          dev.off()
          Sys.sleep(20)
        }
      }
    }
    
    print(paste("Doing - PSignif."))
    mixedmodels.pSignif <- mixedmodels.pVals
    for(i in 1:length(mixedmodels.pSignif))
    {
      print(paste(length(mixedmodels.pSignif), i))
      if(length(mixedmodels.pSignif[[i]]) > 0)
      {
        for(j in 1:length(mixedmodels.pSignif[[i]]))
        {
          mixedmodels.pSignif[[i]][[j]][mixedmodels.pSignif[[i]][[j]] < sigthreshold] <- 0 #'Signif.'
          mixedmodels.pSignif[[i]][[j]][mixedmodels.pSignif[[i]][[j]] >= sigthreshold] <- 1 #'Non Signif.'
        }
      }
    }
    mixedmodels.pSignificants <- mixedmodels.pSignif
    print("Done!")
    
    # -- Plot Stats --
    #plot(unlist(lapply(mixedmodels.summary, FUN = function(x) {x$p.value})), type="l")
    hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]]
    if(bERP) { hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]] }
    if(bERSP) { hStats <- plotStats_ERSP(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]] }
    
    ########################################################################
    ############################## PLOT DATA ############################### 
    ########################################################################
    # -- Plot Data --
    hData <- plotData(subData, paramsList, timeData)
    if(bERSP) { hData <- plotData_ERSP(subData, paramsList, timeData) }
    
    bOnlyFullAnalysis = TRUE
    
    if(length(hStats[[3]]) == 1)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 2)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 3)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 4)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 5)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 6)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(length(hStats[[3]]) == 7)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), ggplotGrob(hStats[[3]][[7]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_D", iDesign, "_", i, ".pdf", sep = ""))
      dev.off()
      Sys.sleep(20)
    }
  }
}
