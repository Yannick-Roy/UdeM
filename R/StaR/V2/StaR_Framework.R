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

bFullStatsAnalysis = TRUE   # Full report on all the data.
bSubDataAnalysis = TRUE     # Multiple plots with data.

ersp_dims <- c(400, 135) # Default
nbPoints = 0 # Need real value (runtime)
timeData = 0 # Need real value (runtime)
freqData = 0 # Need real value (runtime)

data.type = "ERP"
stats.function = "aov"
stats.bCompute = TRUE

sigthreshold = 0.05

# Clear Plots.
#dev.off()

dirPlotsName <- format(Sys.time(), "%b%d_%Hh%M")
#dirPlotsPath <- "~/Documents/Playground/UdeM/RMatlab_Data/StaR_Images/"
dirPlotsPath <- "/media/user/Data/Playground/UdeM/RMatlab_Data/StaR_Images/"
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
    if(data.type == "ERSP") { data.file = "~/Documents/Playground/UdeM/RMatlab_Data/export_mpt.mat" }
    else { data.file = "~/Documents/Playground/UdeM/RMatlab_Data/export_mpt_erp_d1.mat" }
    
    matlabData <- staR_fillFromMatlab(data.file, "MPT", fullData, nbPoints, bSmallSamples = bSmallSamples, dataType = data.type)

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
      stats.pVals <- list()
      
      if(stats.function == "lme")
      {
  
      } else if(stats.function == "aov")
      {
        #stats.aov.results <- staR_aov(fullData, iDesign)
        cl <- makeCluster(8) 
        clusterExport(cl, list("aov", "STATS_DESIGNS", "iDesign"))
        
        print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
        tic()
        anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
        #anovas.full <- lapply(fullData, FUN = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
        #else if(func == 'anova') { anovas.full <- lapply(fullData, FUN = function(x) {anova(STATS_DESIGNS[[iDesign]], x)}) }
        #else if(func == 'ez') { anovas.full <- lapply(fullData, FUN = function(x) {ezANOVA(data = x, dv = values, wid = subjects, within = .(subjects), between = .(groups, conditions))})} #{STATS_DESIGNS_ez[[iDesign]]}) }
        
        anovas.full.titles = paste("aov", " : ",  format(STATS_DESIGNS[[iDesign + 20]]))
        toc()
        print("Done!")
        
        stopCluster(cl)
        
        print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
        anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
        
        anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]})
        
        print(paste("Doing - PVals (Full)"))
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
        mixedmodels.pValsTitle[[3]] <- unlist(lapply(anovas.pValsTitle, unique))
      }
      
      #############################################################
      ###### Pvals !
      #############################################################
      bShowPlot = TRUE
      if(bShowPlot)
      {
        for(i in 1:length(stats.pVals))
        {
          plot(unlist(stats.pVals[[3]][[i]]), type="l", log="y", ylim = c(0.001, 1))
          title(main = mixedmodels.pValsTitle[[3]][[i]])
          abline(h = sigthreshold)
          
          if(bSaveOnDiskImages)
          {
            dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
            dev.off()
            Sys.sleep(10)
          }
        }
      }
      
      #############################################################
      ###### Pvals Signif ( < threshold ) !
      #############################################################
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
      save(fullData, timeData, freqData, subData, iDesign, paramsList, mixedmodels.pVals, mixedmodels.pSignificants, mixedmodels.pValsTitle, file = paste(dirPlots, "/Workspace.RData", sep=""))
      
      # -- Plot Stats --
      #plot(unlist(lapply(mixedmodels.summary, FUN = function(x) {x$p.value})), type="l")
      hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]]
      if(data.type == "ERSP") { hStats <- plotStats_ERSP(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle, ersp_dims)}
      else { hStats <- plotStats(mixedmodels.pSignificants, timeData, mixedmodels.pValsTitle)[[1]] }
    }      
    
    #############################################################
    ###### Select sub data & params !
    #############################################################
    # -- Plot Data --
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
  
    #############################################################
    ###### Plot data !
    #############################################################
    # -- Plot All --
    if(bSubDataAnalysis == TRUE)
    {
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
            }
            else
            {
              hRows[[i]][[j]] <- ggplotGrob(hData[[i]][[j]][[k]])
            }
          }
        }
        
        if(stats.bCompute) # Don't add stat graph if there aren't being computed!
        {
          hRows[[i]][[j]] <- cbind(hRows[[i]][[j]], ggplotGrob(hStats[[1]][[i]][[k]]), size = "last")
        }
      }
      
      if(designMatrix$nbRow > 1 && stats.bCompute)
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
      
      if(length(hRows[[1]]) == 1) 
      {
        grid.arrange(hRows[[1]][[1]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows[[1]]) == 2) 
      {
        grid.arrange(hRows[[1]][[1]], hRows[[1]][[2]], top=textGrob(staR_getDesignName(iDesignb, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(length(hRows[[1]]) == 3) 
      {
        grid.arrange(hRows[[1]][[1]], hRows[[1]][[2]], hRows[[1]][[3]], top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      
      if(bSaveOnDiskImages)
      {
        dev.copy2pdf(file = paste(dirPlots, "/Data_", i, ".pdf", sep = ""))
        dev.off()
      }
    }
    
    #############################################################
    ###### Plot full design !
    #############################################################
    if(stats.bCompute)
    {
      if(length(hStats[[3]]) == 1)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), top=textGrob(staR_getDesignName(iDesign, FALSE, TRUE, Afunc = NULL, MMfunc ="lme"), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 2)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 3)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 4)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 5)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 6)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 7)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), ggplotGrob(hStats[[3]][[7]]), top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
      }
      if(length(hStats[[3]]) == 15)
      {
        grid.arrange(ggplotGrob(hStats[[3]][[1]]), ggplotGrob(hStats[[3]][[2]]), ggplotGrob(hStats[[3]][[3]]), ggplotGrob(hStats[[3]][[4]]), ggplotGrob(hStats[[3]][[5]]), ggplotGrob(hStats[[3]][[6]]), ggplotGrob(hStats[[3]][[7]]), 
                     ggplotGrob(hStats[[3]][[8]]), ggplotGrob(hStats[[3]][[9]]), ggplotGrob(hStats[[3]][[10]]), ggplotGrob(hStats[[3]][[11]]), ggplotGrob(hStats[[3]][[12]]), ggplotGrob(hStats[[3]][[13]]), ggplotGrob(hStats[[3]][[14]]), ggplotGrob(hStats[[3]][[15]]),
                     top=textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20,font=3)))
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
