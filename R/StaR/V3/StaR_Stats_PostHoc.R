StaR_PostHoc_do <- function(vars, varSortBy = "domains")
{
   design = paste(vars, collapse = ":")
   design = paste(varSortedBy, design, sep =":")
   design = paste("~", design, sep="")
   
   stats.fullAnalysis.lme.lsmeans <- lapply(stats.fullAnalysis.lme.report, FUN = function(x) {lsmeans(x, formula(design))})
   stats.fullAnalysis.lme.lsmeans.pairs <- lapply(stats.fullAnalysis.lme.lsmeans, FUN = function(x) {pairs(x, by=varSortBy)})
   stats.fullAnalysis.lme.lsmeans.pairs[[1]]
   
   stats.fullAnalysis.lme.lsmeans.pairs.sum <- lapply(stats.fullAnalysis.lme.lsmeans.pairs, FUN = summary)
   pValsAndTitles <- lapply(stats.fullAnalysis.lme.lsmeans.pairs.sum, FUN = function(x) {StaR_PostHoc_getPairs(x, vars, varSortBy)})  
   
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
   return (list(pVals, pTitles))
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