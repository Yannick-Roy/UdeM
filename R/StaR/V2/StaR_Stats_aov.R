
staR_aov <- function(fullData, iDesign)
{
  print(paste("Doing - Anova (fullData) : ", format(STATS_DESIGNS[[iDesign + 20]])))
  tic()
  anovas.full <- lapply(fullData, FUN = function(x) {aov(STATS_DESIGNS[[iDesign + 20]], x)})
  #else if(func == 'anova') { anovas.full <- lapply(fullData, FUN = function(x) {anova(STATS_DESIGNS[[iDesign]], x)}) }
  #else if(func == 'ez') { anovas.full <- lapply(fullData, FUN = function(x) {ezANOVA(data = x, dv = values, wid = subjects, within = .(subjects), between = .(groups, conditions))})} #{STATS_DESIGNS_ez[[iDesign]]}) }
  
  anovas.full.titles = paste("aov", " : ",  format(STATS_DESIGNS[[iDesign + 20]]))
  toc()
  print("Done!")
  
  print(paste("Doing - Summary (full) : ", format(STATS_DESIGNS[[iDesign + 20]])))      
  anovas.full.summary <- lapply(anovas.full, FUN = function(x) {summary(x)})
  
  anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[1]]})
  
  print(paste("Doing - PVals (Full)"))
  anovas.pVals <- list()
  anovas.pVals <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'})
  #anovas.pVals[[2]] <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[2]]})
  #anovas.pVals[[3]] <- lapply(anovas.full.summary, FUN = function(x) {x[[1]]$'Pr(>F)'[[3]]})
  
  list(anovas.pVals, anovas.full.titles)
}