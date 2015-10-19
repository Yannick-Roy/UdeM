
staR_aov <- function(fullData, iDesign, bSubAnalysis = FALSE)
{
  cl <- makeCluster(8) 
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
    pNames[[i]] <- unique(pNames[[i]])
  }
  
  return(list(pVs, pNames))
}


staR_aov_sub <- function(subData, iDesign)
{
  cl <- makeCluster(8) 
  clusterExport(cl, list("aov", "STATS_SUB_DESIGNS", "iDesign"))
  
  print(paste("Doing - Anova (fullData) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]])))
  tic()
  anovas.full <- parLapply(cl = cl, fullData, fun = function(x) {aov(STATS_SUB_DESIGNS[[iDesign + 20]], x)})
  
  anovas.full.titles = paste("aov", " : ",  format(STATS_SUB_DESIGNS[[iDesign + 20]]))
  toc()
  print("Done!")
  
  stopCluster(cl)
  
  print(paste("Doing - Summary (full) : ", format(STATS_SUB_DESIGNS[[iDesign + 20]])))      
  anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
  
  return (staR_aov_pvals(anovas.full.summary))
}

# staR_aov_sub() <- function(subData)
# {
#   print(paste("Doing - Combining (subData) : ", format(STATS_DESIGNS[[iDesign]])))
#   tic()
#   
#   #  Combine chaque pixel nbPoints (e.g. 1536) rbind. 3 dimension, figure it out !
#   hData <- list()
#   vData <- list()
#   for(p in 1:nbPoints)  # Points
#   {
#     hData[[p]] <- list()
#     if(length(subData) > 0)
#     {
#       for(i in 1:length(subData)) # i - Layers
#       {
#         if(length(subData[[i]]) > 0)
#         {
#           for(j in 1:length(subData[[i]])) # j - Rows
#           {
#             if(length(subData[[i]][[j]]) > 0)
#             {
#               for(k in 1:length(subData[[i]][[j]])) # k - Cols
#               { 
#                 # -- Horizontal --
#                 #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
#                 if(k == 1) {hData[[p]][[i]][[j]] <- subData[[i]][[j]][[k]][[p]]}
#                 else {hData[[p]][[i]][[j]] <- rbind(hData[[p]][[i]][[j]], subData[[i]][[j]][[k]][[p]])}
#               }
#             }
#           }
#         }
#       }
#       
#       
# #       if(length(subData) > 1)
# #       {
# #         vData[[k]] <- list()
# #         if(length(subData[[i]]) > 1)
# #         {
# #           for(j in 1:length(subData[[1]]))
# #           {
# #             for(i in 1:length(subData))
# #             {
# #               # -- Vertical --
# #               #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
# #               if(i == 1) {vData[[k]][[j]] <- subData[[i]][[j]][[k]]}
# #               else {vData[[k]][[j]] <- rbind(vData[[k]][[j]], subData[[i]][[j]][[k]])}
# #             }
# #           }
# #         }
# #       }
#     }
#   }
#   
#   subDataCombined <- list()
#   if(length(subData) > 0)
#   {
#     subDataCombined[[1]] <- staR_InvertDimensions(hData)
#     if(length(subData) > 1)
#     {
#       subDataCombined[[2]] <- staR_InvertDimensions(vData)
#     }
#   }
#   
#   toc()
#   print("Done!")
#   ##############################
# }
