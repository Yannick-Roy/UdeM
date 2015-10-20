
## We want to put the 1st D (points) in 3th D and keep Layer x Rows x Cols (i x j x k)
staR_InvertDimensions3D <- function(array3D)
{ 
  b <- list()
  for(p in 1:length(array3D))
  {
    for(i in 1:length(array3D[[p]]))
    {
      for(j in 1:length(array3D[[p]][[i]]))
      {
        #print(paste(p,i,j))
        if(length(b) < i) {b[[i]] <- list()}
        if(length(b[[i]]) <  j) {b[[i]][[j]] <- list()} 
        
        b[[i]][[j]][[p]] <- array3D[[p]][[i]][[j]]
      }
    }
  }  
  return (b)
}


staR_aov <- function(fullData, iDesign, bSubAnalysis = FALSE)
{
  cl <- makeCluster(4) 
  clusterExport(cl, list("aov", "STATS_DESIGNS", "iDesign"))
  
  print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
  tic()
  anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})

  anovas.full.titles = paste("aov", " : ",  format(STATS_DESIGNS[[iDesign + 20]]))
  toc()
  print("Done!")
  
  stopCluster(cl)
  
  print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
  anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
  
  return (staR_aov_pvals(anovas.full.summary))
  
#   print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
#   tic()
#   anovas.full <- lapply(fullData, FUN = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
#   #else if(func == 'anova') { anovas.full <- lapply(fullData, FUN = function(x) {anova(STATS_DESIGNS[[iDesign]], x)}) }
#   #else if(func == 'ez') { anovas.full <- lapply(fullData, FUN = function(x) {ezANOVA(data = x, dv = values, wid = subjects, within = .(subjects), between = .(groups, conditions))})} #{STATS_DESIGNS_ez[[iDesign]]}) }
#   
#   anovas.full.titles = paste("aov", " : ",  format(STATS_DESIGNS[[iDesign + 20]]))
#   toc()
#   print("Done!")
#   
#   print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
#   anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
#   
#   anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]})
#   
#   print(paste("Doing - PVals (Full)"))
#   anovas.pVals <- list()
#   anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'})
#   #anovas.pVals[[2]] <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[2]]})
#   #anovas.pVals[[3]] <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[3]]})
#   
#   list(anovas.pVals, anovas.full.titles)
}

staR_aov_pvals <- function(summary)
{
  anovas.full.summary <- summary
  print(paste("Doing - PVals"))
  df.final <- lapply(anovas.full.summary, FUN = function(a) {
    #a <- anovas.full.summary[[1]]
    b <- lapply(a, FUN = function(x) {row.names(x[[1]])})
    c <- lapply(a, FUN = function(x) {x[[1]]$`Pr(>F)`})
    b.clean <- lapply(b, FUN = function(x) {str_replace_all(string=x, pattern=" ", repl="")})
    df <- data.frame("variables" = unlist(b.clean), "pVal" = unlist(c))
    row.names(df) <- seq(1:dim(df)[[1]])
    df.valid <- subset(df, variables != "Residuals")
    
    #list(df.valid$variables, df.valid$pVal)
  })
  
  pVs <- list()
  pNames <- list()
  for(i in 1:dim(df.final[[1]])[[1]])
  {
    pVs[[i]] <- list()
    pNames[[i]] <- list()
    for(j in 1:length(df.final))
    {
      pVs[[i]][[j]] <- df.final[[j]][,2][[i]]
      pNames[[i]][[j]] <- as.character(df.final[[j]][,1][[i]])
    }
    pNames[[i]] <- as.character(unique(pNames[[i]]))
  }
  
  return(list(pVs, pNames))
}


# staR_aov_sub <- function(subData, iDesign)
# {
#   cl <- makeCluster(8) 
#   clusterExport(cl, list("aov", "STATS_SUB_DESIGNS", "iDesign"))
#   
#   print(paste("Doing - Anova (fullData) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]])))
#   tic()
#   anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]], x)})
#   
#   anovas.full.titles = paste("aov", " : ",  format(STATS_SUB_DESIGNS[[iDesign + 20]]))
#   toc()
#   print("Done!")
#   
#   stopCluster(cl)
#   
#   print(paste("Doing - Summary (full) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]])))      
#   anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
#   
#   return (staR_aov_pvals(anovas.full.summary))
# }

staR_aov_sub <- function(subData, iDesign)
{
  #stats.fullAnalysis.aov.retVal <- staR_aov(fullData, iDesign)
  hDataset <- list()
  vDataset <- list()
  for(p in 1:nbPoints)  # Points
  {
    ## -----------------------
    ## -- Horizontal (Rows) --
    ## -----------------------
    hDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(i in 1:length(subData)) # Layers
      {
        hDataset[[p]][[i]] <- list()
        if(length(subData[[i]]) > 0)
        {
          for(j in 1:length(subData[[i]])) # Rows
          {
            hDataset[[p]][[i]][[j]] <- list()
            if(length(subData[[i]][[j]]) > 0)
            {
              for(k in 1:length(subData[[i]][[j]])) # Cols
              { 
                #print(paste(p, " (h) - ", i, j, k))
                # -- Horizontal --
                #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
                if(k == 1) {hDataset[[p]][[i]][[j]] <- subData[[i]][[j]][[k]][[p]]}
                else {hDataset[[p]][[i]][[j]] <- rbind(hDataset[[p]][[i]][[j]], subData[[i]][[j]][[k]][[p]])}
              }
            }
          }
        }
      }
    }
    
    ## -----------------------
    ## -- Vertival (Cols) --
    ## -----------------------
    vDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(i in 1:length(subData)) # Layers
      {
        vDataset[[p]][[i]] <- list()
        if(length(subData[[i]]) > 0)
        {
          for(k in 1:length(subData[[i]][[1]])) # Cols
          {
            vDataset[[p]][[i]][[k]] <- list()
            for(j in 1:length(subData[[i]])) # Rows
            {
              #print(paste(p, " (v) - ", i, j, k))
              # -- Vertical --
              if(j == 1) {vDataset[[p]][[i]][[k]] <- subData[[i]][[j]][[k]][[p]]}
              else {vDataset[[p]][[i]][[k]] <- rbind(vDataset[[p]][[i]][[k]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    }
  }
  
  stats.subAnalysis.hData <- staR_InvertDimensions3D(hDataset)
  stats.subAnalysis.vData <- staR_InvertDimensions3D(vDataset)
  
  stats.subAnalysis.combinedData <- list()
  stats.subAnalysis.combinedData[[1]] <- stats.subAnalysis.hData # Rows.
  stats.subAnalysis.combinedData[[2]] <- stats.subAnalysis.vData # Cols.
  
  #stats.subAnalysis.aov.retVal <- staR_aov(stats.subAnalysis.hData[[1]][[1]], iDesign)
  
  stats.subAnalysis.pVals <- list()
  stats.subAnalysis.pTitles <- list()
  for(curDim in 1:length(stats.subAnalysis.combinedData)) # Row & Cols (1 - Horizontal | 2 - Vertical)
  {
    if(length(stats.subAnalysis.combinedData[[curDim]]) > 0)
    {
      stats.subAnalysis.pVals[[curDim]] <- list()
      stats.subAnalysis.pTitles[[curDim]] <- list()
      for(i in 1:length(stats.subAnalysis.combinedData[[curDim]])) # Layers
      {
        stats.subAnalysis.pVals[[curDim]][[i]] <- list()
        stats.subAnalysis.pTitles[[curDim]][[i]] <- list()
        for(j in 1:length(stats.subAnalysis.combinedData[[curDim]][[i]])) # Rows
        {
          print(paste("Doing - Anova (subData - [",curDim, ",", i, ",", j, "]) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]])))
          
          cl <- makeCluster(4) 
          clusterExport(cl, list("aov", "STATS_SUB_DESIGNS", "iDesign", "curDim"))
          tic()
          anovas.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[curDim]][[i]][[j]], fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]], x)})
          toc()
          stopCluster(cl)
          
          anovas.full.titles = paste("aov", " : ",  format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]]))
          print("Done!")
          
          print(paste("Doing - Summary (sub - [",curDim, ",", i, ",", j, "]) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]])))      
          anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
          
          stats.subAnalysis.aov.retVal <- staR_aov_pvals(anovas.full.summary)
          
          stats.subAnalysis.pVals[[curDim]][[i]][[j]] <- unlist(stats.subAnalysis.aov.retVal[[1]])
          stats.subAnalysis.pTitles[[curDim]][[i]][[j]]  <- unique(stats.subAnalysis.aov.retVal[[2]])
        }
      }
    }
  }
  
  return(list(stats.subAnalysis.pVals, stats.subAnalysis.pTitles))
}
