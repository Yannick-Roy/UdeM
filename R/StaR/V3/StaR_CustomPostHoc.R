#====================================================
#=========== Principal Effects & Domains ============
#====================================================

#-------------------------------
# Custom #1 - Domains * Groups
#-------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#-------------------------------
# Custom #2 - Domains * Orders
#-------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:orders)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#-------------------------------
# Custom #3 - Domains * Motions
#-------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:motions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#-------------------------------
# Custom #4 - Domains * Sessions
#-------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#-------------------------------
# Custom #4X - Sessions
#-------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x)})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

#====================================================
#=========== Principal Effects & Domains ============
#====================================================

#----------------------------------------
# Custom #5 - Domains * Groups * Orders
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:orders)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#----------------------------------------
# Custom #6 - Domains * Groups * Motions
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:motions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#----------------------------------------
# Custom #7, 8 - Domains * Groups * Sessions
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#----------------------------------------
# Custom #9,10,11 - Domains * Groups * Orders * Sessions
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:orders:sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#----------------------------------------
# Custom #12,13,14 - Domains * Groups * Motions * Sessions
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:motions:sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]

save.image()

#----------------------------------------
# Custom #15,16,17,18 - Domains * Groups * Orders * Motions * Sessions
#----------------------------------------
stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, ~ domains:groups:orders:motions:sessions)})
stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by="domains")})
stats.fullAnalysis.lme.lsmeans.pairs[[1]]



#====================================================
# Test code
#====================================================
#--------------------
# With only 1 point.
#--------------------
# varList <- c("sessions", "motions")
# varSortedBy <- "domains"
# p1.pValsAndTitles <- StaR_PostHoc_getPairs(p1.pairs.sum, varList, varSortedBy)
# 
# p1.pairs <- stats.fullAnalysis.lme.lsmeans.pairs[[1]]
# p1.pairs.sum <- summary(stats.fullAnalysis.lme.lsmeans.pairs[[1]])

#--------------------
# With all points!
#--------------------
varList <- c("sessions")
varSortedBy <- "" #"domains"

stats.fullAnalysis.lme.lsmeans.pairs.sum <- lapply(stats.fullAnalysis.lme.lsmeans.pairs, FUN = summary)
pValsAndTitles <- lapply(stats.fullAnalysis.lme.lsmeans.pairs.sum, FUN = function(x) {StaR_PostHoc_getPairs(x, varList, varSortedBy)})  

# Rearrange from pValsAndTitles[[nbPoints]][[2]][[1]][[Combinations]]
#           to pTitles[[Combinations]][[nbPoints]]
#           and pVals[[Combinations]][[nbPoints]]
# 2nd dim has length of 2, because pTitles = 1 and pVals = 2.
pVals <- list()
pTitles <- list()
for (p in 1:length(pValsAndTitles))
{
   for(c in 1:length(pValsAndTitles[[p]][[1]][[1]]))
   {
      #print(pValsAndTitles[[p]][[1]][[1]][[c]])
      
      if(length(pTitles) < c || is.null(pTitles[[c]])) {pTitles[[c]] <-  pValsAndTitles[[p]][[1]][[1]][[c]]}
      
      if(length(pVals) < c || is.null(pVals[[c]])) {pVals[[c]] <- list()}
      pVals[[c]][[p]] <- pValsAndTitles[[p]][[2]][[1]][[c]]
   }
}


#====================================================
# StaR_PostHoc_getPairs()
# 
#====================================================
StaR_PostHoc_getPairs <- function(lsm.pairs, varList, varSortedBy = "domains")
{
   b <- lsm.pairs
   
   # Step 1 - Subset with "varSortedBy".
   nbSep = 0;
   subsets <- list()
   pTitles <- list()
   pVals <- list()
   for (v in unique(b[[varSortedBy]])){
      nbSep = nbSep + 1
      
      subset.cur <- b[b[[varSortedBy]] == v, ]
      subsets[[length(subsets) + 1]] <- subset.cur
      
      # Step 2 - Get Contrasts.
      b.contrast <- subset.cur$contrast
      b.contrast.strings <- as.character(b.contrast)
      
      # Step 3 - Split "Paired" Contrast String, based on "-"
      b.contrast.substrings <- strsplit(b.contrast.strings, "-")
      
      # Step 4 - Split Effect Value String, based on ","
      b.contrast.effectstrings <- strsplit(b.contrast.substrings[[1]], ",")
      b.contrast.effectstrings <- lapply(b.contrast.substrings, FUN = function(x) {strsplit(x, ",")})
      
      # Variables to save the full loop. (e.g. all 3 domains) 
      pVals[[nbSep]] = NULL
      pTitles[[nbSep]] <- list()
      
      pTitles.pairs <- lapply(b.contrast.effectstrings, FUN = function(x) {
         if(length(x[[1]]) != length(varList)) {
            print("## ERROR Size 1 (PostHoc LSMeans) ##")
         }else {
            title <- list()
            
            for (i in 1:2) {
               curTitle <- NULL
               for (j in 1:length(x[[1]])) {
                  if(is.null(curTitle)){
                     curTitle <- paste(varSortedBy, "=", v, ";", varList[[j]], "=", str_trim(x[[i]][[j]]), sep="")
                  }else {
                     curTitle <- paste(curTitle, ";", varList[[j]], "=", str_trim(x[[i]][[j]]), sep="")
                  }
               }
               title[[i]] <- curTitle
            }
         }
         return (title)
      })
      
      pTitles[[nbSep]] <- lapply(pTitles.pairs, FUN = function(x) {paste(x[[1]], " - ", x[[2]], sep="")})
      pVals[[nbSep]] <- subset.cur$p.value
      
      if(length(pTitles[[nbSep]]) != length(pVals[[nbSep]])){print("## ERROR Size 2 (PostHoc LSMeans) ##")}
   }
   
   pTitles <- list(unlist(pTitles))
   pVals <- list(unlist(pVals))
   
   return (list(pTitles, pVals))
}