source("StaR_Tools.R")

staR_aov <- function(fullData, iDesign, bSubAnalysis = FALSE)
{
  cl <- makeCluster(2) 
  clusterExport(cl, list("aov", "STATS_DESIGNS", "iDesign"))
  
  print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
  tic()
  anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
  toc()
  print("Done!")
  
  stopCluster(cl)
  
  print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
  anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
    
  return (staR_aov_pvals(anovas.full.summary))
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

###################################################################################
######  SUB !
###################################################################################
staR_aov_sub <- function(subData, iDesign, subTitles = NULL)
{
  #=======================================#
  # DELETE ME! - TEST ONLY
  #subData <- subDataset
  
  #stats.fullAnalysis.aov.retVal <- staR_aov(fullData, iDesign)
  # DELETE ME! - TEST ONLY
  #=======================================#
  
  hDataset <- list()
  vDataset <- list()
  lDataset <- list()

  #=======================================#
  #===== ETAPE 1 
  #===== Invert dimension to start with points, 
  #===== rather than having it in the last dimension.
  #=======================================#
  #TODO : This formatting is exactly the same for lme & aov! Put it in tools or stats.
  print("Formatting SubData for aov...")
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
    
    ## -----------------------
    ## -- 3D (Layers) --
    ## -----------------------
    lDataset[[p]] <- list()
    if(length(subData) > 0)
    {
      for(j in 1:length(subData[[1]])) # Rows
      {
        lDataset[[p]][[j]] <- list()
        if(length(subData[[1]][[j]]) > 0)
        {
          for(k in 1:length(subData[[1]][[j]])) # Cols
          {
            lDataset[[p]][[j]][[k]] <- list()
            for(i in 1:length(subData)) # Layers
            {
              #print(paste(p, " (v) - ", i, j, k))
              # -- Vertical --
              if(i == 1) {lDataset[[p]][[j]][[k]] <- subData[[i]][[j]][[k]][[p]]}
              else {lDataset[[p]][[j]][[k]] <- rbind(lDataset[[p]][[j]][[k]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    } 
  }
  
  stats.subAnalysis.hData <- staR_InvertDimensions3D(hDataset)
  stats.subAnalysis.vData <- staR_InvertDimensions3D(vDataset)
  stats.subAnalysis.lData <- staR_InvertDimensions3D(lDataset)
  
  stats.subAnalysis.VarVals <- staR_InvertDimensions3D(subTitles)
  
  # Notes *format*: subVarVals[[hData]][[vData]][[lData]]
  # For example, with Design 11:  subVarVals[[1]][[2]]][[1]]  (groups=3, conditions=FOM)
  # For example, with Design 11:  subVarVals[[2]][[3]][[1]]  (groups=4, conditions=SOF)
  # CurDim is the dimension I want to get out.
  
  stats.subAnalysis.combinedData <- list()
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 0) {stats.subAnalysis.combinedData[[1]] <- stats.subAnalysis.hData} # Rows.
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 1) {stats.subAnalysis.combinedData[[2]] <- stats.subAnalysis.vData} # Cols.
  if(length(STATS_SUB_DESIGNS[[iDesign]]) > 2) {stats.subAnalysis.combinedData[[3]] <- stats.subAnalysis.lData} # Layers.
  
  #stats.subAnalysis.aov.retVal <- staR_aov(stats.subAnalysis.hData[[1]][[1]], iDesign)
  
  stats.subAnalysis.pVals <- list()
  stats.subAnalysis.pTitles <- list()
  if(length(stats.subAnalysis.combinedData) > 0)
  {
    for(curDim in 1:length(stats.subAnalysis.combinedData)) # Row & Cols & Layers (1 - Horizontal | 2 - Vertical | 3 - Layers)
    {
      if(length(stats.subAnalysis.combinedData[[curDim]]) > 0)
      {
        stats.subAnalysis.pVals[[curDim]] <- list()
        stats.subAnalysis.pTitles[[curDim]] <- list()
        for(i in 1:length(stats.subAnalysis.combinedData[[curDim]]))
        {
          stats.subAnalysis.pVals[[curDim]][[i]] <- list()
          stats.subAnalysis.pTitles[[curDim]][[i]] <- list()
          for(j in 1:length(stats.subAnalysis.combinedData[[curDim]][[i]]))
          {
            print(paste("Doing - Anova (subData - [",curDim, ",", i, ",", j, "]) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]])))
            cl <- makeCluster(2) 
            clusterExport(cl, list("aov", "STATS_SUB_DESIGNS", "iDesign"))
            tic()
           
            tryCatch({
            ###############
            # TODO : Fix me with envir...
            ###############
            if(curDim == 1) # TODO : Fix me with envir...
            {
              anovas.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[1]][[i]][[j]], fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]][[1]], x)})
            }
            if(curDim == 2) # TODO : Fix me with envir...
            {
              anovas.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[2]][[i]][[j]], fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]][[2]], x)})
            }
            if(curDim == 3) # TODO : Fix me with envir...
            {
              anovas.full <- parLapply(cl = cl, stats.subAnalysis.combinedData[[3]][[i]][[j]], fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]][[3]], x)})
            }
            toc()
            stopCluster(cl)
            
            anovas.full.titles = paste("aov", " : ",  format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]]))
            print("Done!")
          }, error = function(e) {
            print(paste("======= ERROR in aov_sub :", e, "========"))
            anovas.full.titles = paste("aov - FAILED", " : fixed = ",  format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]]))
            anovas.full <- list()
          })
            
            print(paste("Doing - Summary (sub - [",curDim, ",", i, ",", j, "]) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]][[curDim]])))      
            anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
            
            stats.subAnalysis.aov.retVal <- staR_aov_pvals(anovas.full.summary)
            
            stats.subAnalysis.pVals[[curDim]][[i]][[j]] <- unlist(stats.subAnalysis.aov.retVal[[1]])
            stats.subAnalysis.pTitles[[curDim]][[i]][[j]]  <- unique(stats.subAnalysis.aov.retVal[[2]])
            print(paste("unique ", unique(stats.subAnalysis.aov.retVal[[2]])))
            
            # TEST
            titleCombined <- NULL
            if(curDim == 1) 
            {
              for(vv in 1:length(stats.subAnalysis.VarVals[[1]]))
              {
                if(is.null(titleCombined)) {
                  titleCombined <- stats.subAnalysis.VarVals[[j]][[vv]][[i]]
                } else {
                  titleCombined <- paste(titleCombined, stats.subAnalysis.VarVals[[j]][[vv]][[i]], sep=" | ")
                }
              }
            }
            if(curDim == 2) 
            {
              for(vv in 1:length(stats.subAnalysis.VarVals))
              {
                if(is.null(titleCombined)) {
                  titleCombined <- stats.subAnalysis.VarVals[[vv]][[j]][[i]]
                } else {
                  titleCombined <- paste(titleCombined, stats.subAnalysis.VarVals[[vv]][[j]][[i]], sep=" | ")
                }
              }
            }
            if(curDim == 3) 
            {
              stats.subAnalysis.VarVals
              for(vv in 1:length(stats.subAnalysis.VarVals))
              {
                if(is.null(titleCombined)) {
                  titleCombined <- stats.subAnalysis.VarVals[[i]][[j]][[vv]]
                } else {
                  titleCombined <- paste(titleCombined, stats.subAnalysis.VarVals[[i]][[j]][[vv]], sep=" | ")
                }
              }
            }
            
            stats.subAnalysis.pTitles[[curDim]][[i]][[j]]  <- titleCombined
            print(stats.subAnalysis.pTitles[[curDim]][[i]][[j]])
          }
        }
      }
    }
  }
  
  return(list(stats.subAnalysis.pVals, stats.subAnalysis.pTitles))
}


############################################
################# TESTS !!! ################
############################################
staR_aov_split <- function(fullData, iDesign, bSubAnalysis = FALSE)
{
  anovas.full.summary <- list()
  for(i in 1:6)
  {
    if(i == 1) {fullData.part <- fullData[1:10000]}
    if(i == 2) {fullData.part <- fullData[10001:20000]}
    if(i == 3) {fullData.part <- fullData[20001:30000]}
    if(i == 4) {fullData.part <- fullData[30001:40000]}
    if(i == 5) {fullData.part <- fullData[40001:50000]}
    if(i == 6) {fullData.part <- fullData[50001:54000]}
    
    cl <- makeCluster(2) 
    clusterExport(cl, list("aov", "STATS_DESIGNS", "iDesign"))
    
    print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
    tic()
    anovas.full <- parLapply(cl = cl, fullData.part, fun = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
    toc()
    print("Done!")
    
    stopCluster(cl)
    
    print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
    anovas.full.summary <- cbind(anovas.full.summary, lapply(anovas.full, FUN = function(x) {summary(x)}))
    
    print(paste("aov - done with part #", i))
  }
  
  return (staR_aov_pvals(anovas.full.summary))
}

