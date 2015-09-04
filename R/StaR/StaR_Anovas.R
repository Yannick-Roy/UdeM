##########################################################################
# *** aov() shouldn't be used for unbalanced design ! ***
# http://r.789695.n4.nabble.com/lm-ANOVA-vs-AOV-td819452.html
# http://www.r-statistics.com/tag/unbalanced-design/
#
##########################################################################


library(tictoc)
library(fdrtool)

source("StaR_Designs.R")

###########################################################################
############################       UTILs      #############################
###########################################################################
staR_InvertDimensions <- function(array2D)
{ 
  b <- list()
  for(i in 1:length(array2D))
  {
    for(j in 1:length(array2D[[i]]))
    {
      if(i == 1) { b[[j]] <- list()}
      b[[j]][[i]] <- array2D[[i]][[j]]
    }
  }
  
  b
}

###########################################################################
###########################       ANOVAs      #############################
###########################################################################
# Calculate FullData Anovas.
staR_Anova <- function(fullData, subData = NULL, iDesign = 1, nbPoints = 1536, func = 'aov')
{
  print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign]])))
  tic()
  if(func == 'aov') { anovas.full <- lapply(fullData, FUN = function(x) {aov(STATS_DESIGNS[[iDesign]], x)}) }
  else if(func == 'anova') { anovas.full <- lapply(fullData, FUN = function(x) {anova(STATS_DESIGNS[[iDesign]], x)}) }
  toc()
  print("Done!")
  
  ##############################
  
  print(paste("Doing - Combining (subData) : ", format(STATS_DESIGNS[[iDesign]])))
  tic()
  
  #  Combine chaque pixel nbPoints (e.g. 1536) rbind. 3 dimension, figure it out !
  hData <- list()
  vData <- list()
  for(k in 1:nbPoints)   
  {
    hData[[k]] <- list()
    for(i in 1:length(subData))
    {
      for(j in 1:length(subData[[i]]))
      {
        # -- Horizontal --
        #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
        if(j == 1) {hData[[k]][[i]] <- subData[[i]][[j]][[k]]}
        else {hData[[k]][[i]] <- rbind(hData[[k]][[i]], subData[[i]][[j]][[k]])}
      }
    }
    
    if(length(subData) > 1)
    {
      vData[[k]] <- list()
      for(j in 1:length(subData[[1]]))
      {
        for(i in 1:length(subData))
        {
          # -- Vertical --
          #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
          if(i == 1) {vData[[k]][[j]] <- subData[[i]][[j]][[k]]}
          else {vData[[k]][[j]] <- rbind(vData[[k]][[j]], subData[[i]][[j]][[k]])}
        }
      }
    }
  }
  
  subDataCombined <- list()
  subDataCombined[[1]] <- staR_InvertDimensions(hData)
  if(length(subData) > 1)
  {
    subDataCombined[[2]] <- staR_InvertDimensions(vData)
  }
  
  toc()
  print("Done!")
  ##############################
  
  if(length(subData) >= 1)
  {
    tic()
    anovas.subH <- list()
    for(i in 1:length(subDataCombined[[1]]))
    {      
      if(iDesign >= 1 && iDesign <= 5)
      {
        print(paste("Doing - Anova ", i, "/", length(subDataCombined[[1]]), " (subData H) : ", format(STATS_DESIGNS[[iDesign]])))  
        anovas.subH[[i]] <- lapply(subDataCombined[[1]][[i]], FUN = function(x) {aov(STATS_DESIGNS[[iDesign]], x)})
      }
      else if(iDesign >= 11 && iDesign <= 16)
      {
        print(paste("Doing - Anova ", i, "/", length(subDataCombined[[1]]), " (subData H) : ", format(STATS_SUB_DESIGNS[[iDesign]][[1]])))  
        anovas.subH[[i]] <- lapply(subDataCombined[[1]][[i]], FUN = function(x) {aov(STATS_SUB_DESIGNS[[iDesign]][[1]], x)})
      }
    }
    
    anovas.subV <- list()
    if(length(subData) > 1)
    {
      for(i in 1:length(subDataCombined[[2]]))
      {
        #         if(iDesign >= 1 && iDesign <= 5)
        #         {
        #           print(paste("Doing - Anova ", i, "/", length(subDataCombined[[1]]), " (subData V) : ", format(STATS_DESIGNS[[iDesign]])))  
        #           anovas.subH[[i]] <- lapply(subDataCombined[[2]][[i]], FUN = function(x) {aov(STATS_DESIGNS[[iDesign]][[1]], x)})
        #         }
        if(iDesign >= 11 && iDesign <= 16)
        {
          print(paste("Doing - Anova ", i, "/", length(subDataCombined[[2]]), " (subData V) : ", format(STATS_SUB_DESIGNS[[iDesign]][[2]])))  
          anovas.subV[[i]] <- lapply(subDataCombined[[2]][[i]], FUN = function(x) {aov(STATS_SUB_DESIGNS[[iDesign]][[2]], x)})
        }
      }
    }
    toc()
  }
  print("Done!")
  
  t <- list(anovas.subH, anovas.subV, anovas.full)
}

###########################################################################
##########################       SUMMARY      #############################
###########################################################################
staR_Summary <- function(data, iDesign = 1)
{
  anovas.summary <- list()
  tic()
  
  if(length(data) > 1)
  {
    anovas.summary[[1]] <- list()
    if(length(data[[1]]) >= 1)
    {
      # subH
      print(paste("Doing - Summary (subH) : ", format(STATS_DESIGNS[[iDesign]])))
      for(i in 1:length(data[[1]]))
      {
        anovas.summary[[1]][[i]] <- lapply(data[[1]][[i]], FUN = function(x) {summary(x)})
      }
    }    
    else
    {
      print("No Horizontal Summary. (length < 1)")
    }
    
    anovas.summary[[2]] <- list()
    if(length(data[[2]]) >= 1)
    {
      # subV
      print(paste("Doing - Summary (subV) : ", format(STATS_DESIGNS[[iDesign]])))      
      for(i in 1:length(data[[2]]))
      {
        anovas.summary[[2]][[i]] <- lapply(data[[2]][[i]], FUN = function(x) {summary(x)})
      }
    }
    else
    {
      print("No Vertical Summary. (length < 2)")
    }
    
    anovas.summary[[3]] <- list()
    if(length(data) < 3)
    {
      print("No Full Summary. (length < 3)")
    }
    else
    {
      # Full ([[3]][[1]] - to stay coherent.)
      print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign]])))      
      anovas.summary[[3]][[1]] <- lapply(data[[3]], FUN = function(x) {summary(x)})
    }
  }
  toc()
  print("Done!")
  
  anovas.summary
}

###########################################################################
###########################       P-VALS      #############################
###########################################################################
staR_PVals <- function(summaries, iDesign, sigthreshold = 0.05)
{
  anovas.pVals <- list()
  
  #  if(bAll == TRUE)
  
  print(paste("Doing - PVals : ", format(STATS_DESIGNS[[iDesign]])))
  tic()
  
  if(iDesign >= 1 && iDesign <= 5)
  {
    anovas.pVals[[1]] <- list()
    anovas.pVals[[1]][[1]] <- lapply(summaries[[1]][[1]], FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]})          
  }
  
  if(iDesign >= 11 && iDesign <= 16)
  {
    # subH            
    anovas.pVals[[1]] <- list()
    for(i in 1:length(summaries[[1]]))
    {
      print(paste("Doing - PVals (Horizontal) : ", i, "/", length(summaries[[1]])))
      anovas.pVals[[1]][[i]] <- lapply(summaries[[1]][[i]], FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]}) 
    }
    
    # subV
    anovas.pVals[[2]] <- list()
    for(i in 1:length(summaries[[2]]))
    {
      print(paste("Doing - PVals (Vertical) : ", i, "/", length(summaries[[2]])))
      anovas.pVals[[2]][[i]] <- lapply(summaries[[2]][[i]], FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]}) 
    }
    
    # Full
    print(paste("Doing - PVals (Full)"))
    anovas.pVals[[3]] <- list()
    anovas.pVals[[3]][[1]] <- lapply(summaries[[3]][[1]], FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]}) 
    anovas.pVals[[3]][[2]] <- lapply(summaries[[3]][[1]], FUN = function(x) {x[[1]]$'Pr(>F)'[[2]]})
    anovas.pVals[[3]][[3]] <- lapply(summaries[[3]][[1]], FUN = function(x) {x[[1]]$'Pr(>F)'[[3]]}) 
  }
  
  print(paste("Doing - PSignif."))
  
  anovas.pSignif <- anovas.pVals
  for(i in 1:length(anovas.pSignif))
  {
    print(paste(length(anovas.pSignif), i))
    for(j in 1:length(anovas.pSignif[[i]]))
    {
      anovas.pSignif[[i]][[j]][anovas.pSignif[[i]][[j]] < sigthreshold] <- 0 #'Signif.'
      anovas.pSignif[[i]][[j]][anovas.pSignif[[i]][[j]] >= sigthreshold] <- 1 #'Non Signif.'
    }
  }
  
  toc()
  print("Done!")
  
  retVal <- list(anovas.pVals, anovas.pSignif)
}

###########################################################################
##########################       Designs      #############################
###########################################################################
staR_getDesignMatrix <- function(iDesign)
{
  # Same design with / without random.
  if(iDesign > 20) {iDesign <- iDesign - 20}
  
  if(iDesign == 1) {designMatrix = data.frame("nbCol" = 4, "nbRow" = 1)} # Conditions
  if(iDesign == 2) {designMatrix = data.frame("nbCol" = 3, "nbRow" = 1)} # Sessions
  if(iDesign == 3) {designMatrix = data.frame("nbCol" = 2, "nbRow" = 1)} # Motions
  if(iDesign == 4) {designMatrix = data.frame("nbCol" = 2, "nbRow" = 1)} # Orders
  if(iDesign == 5) {designMatrix = data.frame("nbCol" = 2, "nbRow" = 1)} # Groups
  
  if(iDesign == 11) {designMatrix = data.frame("nbCol" = 4, "nbRow" = 2)} # Groups * Conditions
  if(iDesign == 12) {designMatrix = data.frame("nbCol" = 3, "nbRow" = 2)} # Groups * Sessions
  if(iDesign == 13) {designMatrix = data.frame("nbCol" = 2, "nbRow" = 2)} # Groups * Motions
  if(iDesign == 14) {designMatrix = data.frame("nbCol" = 2, "nbRow" = 2)} # Groups * Orders
  if(iDesign == 15) {designMatrix = data.frame("nbCol" = 3, "nbRow" = 2)} # Sessions * Motions
  if(iDesign == 16) {designMatrix = data.frame("nbCol" = 3, "nbRow" = 2)} # Sessions * Orders
  
  #STATS_DESIGNS[[17]] = values ~ (groups * sessions * motions)
  #STATS_DESIGNS[[18]] = values ~ (groups * sessions * orders)
  #STATS_DESIGNS[[19]] = values ~ (groups * sessions * motions * orders)
  
  designMatrix
}

staR_getDesignName <- function(iDesign)
{
  paste("ANOVA : ", format(STATS_DESIGNS[[iDesign]]))
}

staR_FDR <- function(pVals)
{
  p.adjust(p = pVals, method="fdr")
}