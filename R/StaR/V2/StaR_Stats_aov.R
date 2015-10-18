
staR_aov <- function(fullData, iDesign)
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
  
  return (list(pVs, pNames))
  
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