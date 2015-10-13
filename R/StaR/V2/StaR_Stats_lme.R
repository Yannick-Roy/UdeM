library(lme4)
library(nlme)
library(pbkrtest)

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