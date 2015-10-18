###################
### LAB VERSION ###
###################
#         anovas.full.vals <- lapply(anovas.full.summary, FUN = function(x){
#           pVals <- list()
#           full.titles <- list()
#           
#           for(i in 1:length(x))
#           {
#             for(j in 1:length(x[[i]]))
#             {
#               pVals[[i]] <- list()
#               full.titles[[i]] <- list()
#               n <- length(x[[i]][[1]]$`Pr(>F)`)
#               
#               pVals[[i]] <- x[[i]][[1]]$`Pr(>F)`[1:(n-1)]
#               full.titles[[i]] <- lapply(row.names(x[[i]][[1]])[1:(n-1)], FUN = function(x) {str_replace_all(string=x, pattern=" ", repl="")})
#             }
#           }
#           #list(pVals, full.titles)
#           list(unlist(lapply(pVals, unlist)), unlist(lapply(full.titles, unlist)))})

# unlist(lapply(anovas.full.vals[[1]][[1]], unlist))
# unlist(lapply(anovas.full.vals[[1]][[2]], unlist))
#         
#         anovas.pVals <- list()
#         anovas.pValsTitle <- list()
#         for(i in 1:length(anovas.full.vals[[1]][[1]]))
#         {
#           anovas.pVals[[i]] <- lapply(anovas.full.vals, FUN = function(x){ x[[1]][[i]] })
#           anovas.pValsTitle[[i]] <- lapply(anovas.full.vals, FUN = function(x){ x[[2]][[i]] })
#         }
###################
### LAB VERSION ###
###################


####################
### HOME VERSION ###
####################
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

stats.fullAnalysis.pVals <- pVs
stats.fullAnalysis.pTitles <- pNames
####################
### HOME VERSION ###
####################