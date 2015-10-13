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
library(stringr)
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

designs = 19#c(2,3,4,5,11,12,13,14,15,16,17,18)

#fullDataAnalysis <- function(iDesign = 1, bReloadFile = FALSE, bReprepData = FALSE, bSaveOnDisk = FALSE)
#iDesign = 13
bReloadRData = FALSE

bLoadMatlabFile = TRUE
bPrepMatlabData = TRUE

bSampleMatlabData = FALSE
bSmallSamples = FALSE

bSaveOnDiskImages = TRUE
bSaveOnDiskData = TRUE

bOnlyFullAnalysis = TRUE

ersp_dims <- c(400, 135) # Default
nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

stats.function = "aov"

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
    
    print("Get Params (mean, min, max, ...)")
    #paramsList <- staR_getDistParams(subData, timeData, iDesign)
    #if(bERSP) {timeVals <- seq(1, nbPoints)}
    timeVals <- timeData
    paramsList <- staR_getDistParams(subData, timeVals, iDesign)
    
    
    ########################################################################
    ################################ Stats ################################# 
    ########################################################################
    if(stats.function == "lme")
    {
      # mixedmodels.all = 3D; H | V | Full
      hTitles[[3]] <- "Mixed Models..."
      
      #mixedmodels.all <- staR_MMlme(fullData = fullData, subData = subDataset, iDesign = iDesign)
      if(TRUE)
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
    }
    
    if(stats.function == "lmer")
    {
      cl <- makeCluster(8) 
      clusterExport(cl, list("lmer", "STATS_DESIGNS_MM", "STATS_DESIGNS_MM_RESTRICTED", "iDesign"))
      
      tic()
      print(paste("Doing - lmer (fullData) : ", format(STATS_DESIGNS_MM[[iDesign]])))
      #mixedmodels.full <- lapply(fullData, FUN = function(x) {lmer(STATS_DESIGNS_MM[[iDesign]], x)})
      mixedmodels.full <- parLapply(cl = cl, fullData, fun = function(x) {lmer(STATS_DESIGNS_MM[[iDesign]], x)})
      
      print(paste("Doing - lmer (fullData) : ", format(STATS_DESIGNS_MM_RESTRICTED)))
      #mixedmodels.restricted <- lapply(fullData, FUN = function(x) {lmer(STATS_DESIGNS_MM_RESTRICTED, x)})
      mixedmodels.restricted <- parLapply(cl = cl, fullData, fun = function(x) {lmer(STATS_DESIGNS_MM_RESTRICTED, x)})
      toc()
      
      stopCluster(cl)
      
      print("Done!")
      
      tic()
      print(paste("Doing - KRmodcomp..."))
      results <- mapply(FUN = KRmodcomp, mixedmodels.full, mixedmodels.restricted)
      print("Done!")
      toc()
      
    
    }
    
    if(stats.function == "aov")
    {
      cl <- makeCluster(8) 
      clusterExport(cl, list("aov", "STATS_DESIGNS", "iDesign"))
      
      tic()
      print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
      anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
      #else if(func == 'anova') { anovas.full <- lapply(fullData, FUN = function(x) {anova(STATS_DESIGNS[[iDesign]], x)}) }
      #else if(func == 'ez') { anovas.full <- lapply(fullData, FUN = function(x) {ezANOVA(data = x, dv = values, wid = subjects, within = .(subjects), between = .(groups, conditions))})} #{STATS_DESIGNS_ez[[iDesign]]}) }
      print("Done!")
      toc()
      
      stopCluster(cl)
      
      anovas.full.titles = paste(stats.function, " : ",  format(STATS_DESIGNS[[iDesign + 20]]))
      
      print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
      anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
      
      print(paste("Doing - PVals (Full)"))
      #anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[2]][[1]]$'Pr(>F)'[[1]]})
      
      anovas.full.vals <- lapply(anovas.full.summary, FUN = function(x){
        pVals <- list()
        full.titles <- list()
        
        for(i in 1:length(x))
        {
          for(j in 1:length(x[[i]]))
          {
            pVals[[i]] <- list()
            full.titles[[i]] <- list()
            n <- length(x[[i]][[1]]$`Pr(>F)`)
  
            pVals[[i]] <- x[[i]][[1]]$`Pr(>F)`[1:(n-1)]
            full.titles[[i]] <- lapply(row.names(x[[i]][[1]])[1:(n-1)], FUN = function(x) {str_replace_all(string=x, pattern=" ", repl="")})
          }
        }
        
        #list(pVals, full.titles)
        list(unlist(lapply(pVals, unlist)), unlist(lapply(full.titles, unlist)))})
      
     # unlist(lapply(anovas.full.vals[[1]][[1]], unlist))
     # unlist(lapply(anovas.full.vals[[1]][[2]], unlist))
      
      anovas.pVals <- list()
      anovas.pValsTitle <- list()
      for(i in 1:length(anovas.full.vals[[1]][[1]]))
      {
        anovas.pVals[[i]] <- lapply(anovas.full.vals, FUN = function(x){ x[[1]][[i]] })
        anovas.pValsTitle[[i]] <- lapply(anovas.full.vals, FUN = function(x){ x[[2]][[i]] })
      }
    
      mixedmodels.pVals <- list()
      mixedmodels.pVals[[3]] <- list()
      mixedmodels.pValsTitle <- list()
      mixedmodels.pValsTitle[[3]] <- list()
      
      mixedmodels.pVals[[3]] <- anovas.pVals
      mixedmodels.pValsTitle[[3]] <- lapply(anovas.pValsTitle, unique)
    }
    
    
    sigthreshold = 0.05
    
    bShowPlot = TRUE
    if(bShowPlot)
    {
      for(i in 1:length(mixedmodels.pVals[[3]]))
      {
        print(paste("plot #", i, " of ", length(mixedmodels.pVals[[3]])))
        plot(unlist(mixedmodels.pVals[[3]][[i]]), type="l", log="y", ylim = c(0.001, 1))
        title(main = mixedmodels.pValsTitle[[3]][[i]])
        abline(h = sigthreshold)
        
        if(bSaveOnDiskImages)
        {
          dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
          dev.off()
          Sys.sleep(15)
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
    
    #save() # Save on disk.
    #save(fullData, timeData, freqData, subData, iDesign, paramsList, mixedmodels.pVals, mixedmodels.pSignificants, mixedmodels.pValsTitle, file = paste(dirPlots, "/Workspace.RData", sep=""))
    
    # -- Plot Stats --
    #plot(unlist(lapply(mixedmodels.summary, FUN = function(x) {x$p.value})), type="l")
    hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]]
    if(bERP) { hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]] }
    if(bERSP) { hStats <- plotStats_ERSP(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle, ersp_dims)}
    
    ########################################################################
    ############################## PLOT DATA ############################### 
    ########################################################################
    # -- Plot Data --
    hData <- plotData(subData, paramsList, timeData)
    if(bERSP) { hData <- plotData_ERSP(subData, paramsList, timeData, ersp_dims) }
    
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
    if(length(hStats[[3]]) == 15)
    {
      grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), ggplotGrob(hStats[[3]][[7]]), 
                   ggplotGrob(hStats[[3]][[8]]), ggplotGrob(hStats[[3]][[9]]), ggplotGrob(hStats[[3]][[10]]), ggplotGrob(hStats[[3]][[11]]), ggplotGrob(hStats[[3]][[12]]), ggplotGrob(hStats[[3]][[13]]), ggplotGrob(hStats[[3]][[14]]), ggplotGrob(hStats[[3]][[15]]),
                   top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
    }
    if(bSaveOnDiskImages)
    {
      dev.copy2pdf(file = paste(dirPlots, "/ComplexDesign_D", iDesign, "_", i, ".pdf", sep = ""))
      dev.off()
      Sys.sleep(20)
    }
  }
}
