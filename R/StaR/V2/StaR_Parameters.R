library(R.matlab)

errorCode <- 0

starParams <- readMat("testParamsR.mat")

if(!exists("starParams"))
  print("## ERROR : starParams doesn't exist...")

if(!("starDesign" %in% names(starParams))) { print("## ERROR : starDesign doesn't exist...") }
if(!("starStats" %in% names(starParams))) { print("## ERROR : starStats doesn't exist...") } 
if(!("starThreshold" %in% names(starParams))) { print("## ERROR : starThreshold doesn't exist...") }
if(!("starVars" %in% names(starParams))) { print("## ERROR : starVars doesn't exist...") }
if(!("starFixVars" %in% names(starParams))) { print("## ERROR : starFixVars doesn't exist...") }

print(paste("Design: ", starParams$starDesign))
print(paste("Stats: ", starParams$starStats))
print(paste("Threshold: ", starParams$starThreshold))
print(paste("Vars: ", starParams$starVars)) 
print(paste("FixVars: ", starParams$starFixVars))

writeMat("testParamsM.mat", 'design' = starParams$starDesign)

n <- c("groups", "conditions")
d <- c(2, 4)



# TEST MATLAB LINEARIZED !!!
n <- c()
d <- c()
tryCatch ({
  for(i in 1:length(paramsList))
  {
    for(j in 1:length(paramsList[[i]]))
    { 
      for(k in 1:length(paramsList[[i]][[j]]))
      {  
        n <- c(n, subData.Titles[[i]][[j]][[k]])
        d <- c(d, paramsList[[i]][[j]][[k]]$means)
        print(subData.Titles[[i]][[j]][[k]])
        print(paramsList[[i]][[j]][[k]]$means[[1]])
      }
    }
  }
} , error = function(e) {
  print(paste("# ERRROR # ", error))
})

writeMat("~/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Test2.mat", 'n' = n, 'd' = d)

d1.name <- stats.fullAnalysis.pTitles[[1]]
d1.val <- c()
d2.name <- stats.fullAnalysis.pTitles[[2]]
d2.val <- c()
for(i in 1:length(subDataset[[1]]))
{
  d1.val <- c(d1.val, as.character(unique(subDataset[[1]][[i]][[1]][[1]][stats.fullAnalysis.pTitles[[1]]])[[1]]))
}
for(i in 1:length(subDataset[[1]][[1]]))
{
  d2.val <- c(d2.val, as.character(unique(subDataset[[1]][[1]][[i]][[1]][stats.fullAnalysis.pTitles[[2]]])[[1]]))
}

writeMat("~/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/Star Images/Test_pVals.mat", 'n' = n, 'd' = d)