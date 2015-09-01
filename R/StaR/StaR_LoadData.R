library(R.matlab)

source("StaR_Anovas.R")
###########################################################################
########################### Prep Structure Data ###########################
###########################################################################
staR_prepData <- function()
{
  cond_orders_vals = c("FO", "SO")
  cond_motion_vals = c("F", "M")
  conditions_vals = c("FOM", "FOF", "SOF", "SOM")
  subjects_vals = c(seq(from = 1, to = 15), seq(from = 31, to = 45))
  groups_vals = c(3,4)
  sessions_vals = c(1,2,3)
  
  motions = rep(cond_motion_vals, times = length(groups_vals) * (length(subjects_vals)  / 2) * length(sessions_vals) * length(cond_orders_vals), each = 1)
  orders = rep(cond_orders_vals, times = length(groups_vals) * (length(subjects_vals) / 2) * length(sessions_vals), each = length(cond_motion_vals))

  #conditions = rep(conditions_vals, times = length(groups_vals) * (length(subjects_vals) / 2) * length(sessions_vals))
  conditions <- paste(orders, motions, sep="")
  
  sessions = rep(sessions_vals, times = 360 / (length(conditions_vals) * length(sessions_vals)) , each = length(conditions_vals))
  subjects = rep(subjects_vals, times = length(groups_vals) / 2, each = length(conditions) / length(groups_vals) / (length(subjects_vals)/2))
  groups = rep(groups_vals, times = 1, each = length(conditions) / length(groups_vals))
  
  values = seq(1,360)
  
  fullData = data.frame(groups, subjects, sessions, orders, motions, conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
  #fullData = data.frame(groups, subjects, sessions, conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
  
  fullData$subjects <- factor(fullData$subjects)
  fullData$groups <- factor(fullData$groups)
  fullData$orders <- factor(fullData$orders)
  fullData$motions <- factor(fullData$motions)
  fullData$conditions <- factor(fullData$conditions)
  
  fullData
}

###########################################################################
###################       Fill Structure from Matlab File !     ###########
###########################################################################
staR_fillFromMatlab <- function(fileName, toolBox, dataStructure, nbPoints = 1536)
{
  mld_erps.d <- readMat(fileName)
  
  if(toolBox == "MPT")
  {
    fullData <- list()
    for(j in 1:nbPoints)
    {
      fullData[[j]] = dataStructure
      
      for (i in 0:(90 - 1)) 
      {
        fullData[[j]]$values[i*4 + 1] = mld_erps.d$linearProjectedMeasure[0 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 2] = mld_erps.d$linearProjectedMeasure[1 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 3] = mld_erps.d$linearProjectedMeasure[2 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 4] = mld_erps.d$linearProjectedMeasure[3 * nbPoints + j, i + 1]
      }
      
      if(j %% 100 == 0)
        print(paste("Filling Data Structure with Matlab Data :", j))
    }
  }
  
  # Factor them !
  for(i in 1:nbPoints)
  {
    fullData[[i]]$subjects <- factor(fullData[[i]]$subjects)
    fullData[[i]]$groups <- factor(fullData[[i]]$groups)
    fullData[[i]]$sessions <- factor(fullData[[i]]$sessions)
    fullData[[i]]$orders <- factor(fullData[[i]]$orders)
    fullData[[i]]$motions <- factor(fullData[[i]]$motions)
    
    if(i %% 100 == 0)
      print(i)
  }
  
  times <- list()
  if("times" %in% names(mld_erps.d))
  {
    times <- mld_erps.d$times
  }
  
  freqs <- list()
  if("freqs" %in% names(mld_erps.d))
  {
    freqs <- mld_erps.d$freqs
  }
  
  list(fullData, times, freqs)
}

###########################################################################
########################       Select Data !     ##########################
###########################################################################
staR_selectData <- function(fullData, iDesign)
{
  
  subDataset <- list() 
  subDataset[[1]] <- list()
  
  if(iDesign == 1)
  {
    subDataset[[1]][[1]] <- lapply(fullData, subset, conditions == "FOF")    
    subDataset[[1]][[2]] <- lapply(fullData, subset, conditions == "FOM")
    subDataset[[1]][[3]] <- lapply(fullData, subset, conditions == "SOF")
    subDataset[[1]][[4]] <- lapply(fullData, subset, conditions == "SOM")
  }
  
  if(iDesign == 2)
  {
    subDataset[[1]][[1]] <- lapply(fullData, subset, sessions == 1)    
    subDataset[[1]][[2]] <- lapply(fullData, subset, sessions == 2)
    subDataset[[1]][[3]] <- lapply(fullData, subset, sessions == 3)
  }
  
  if(iDesign == 3)
  {
    subDataset[[1]][[1]] <- lapply(fullData, subset, motions == "F")    
    subDataset[[1]][[2]] <- lapply(fullData, subset, motions == "M")
  }
  
  if(iDesign == 4)
  {
    subDataset[[1]][[1]] <- lapply(fullData, subset, orders == "FO")    
    subDataset[[1]][[2]] <- lapply(fullData, subset, orders == "SO")
  }
  
  if(iDesign == 5)
  {
    subDataset[[1]][[1]] <- lapply(fullData, subset, groups == 3)    
    subDataset[[1]][[2]] <- lapply(fullData, subset, groups == 4)
  }
  
  if(iDesign == 11)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, conditions == "FOF" & groups == 3)
    subDataset[[1]][[2]] <- lapply(fullData, subset, conditions == "FOM" & groups == 3)    
    subDataset[[1]][[3]] <- lapply(fullData, subset, conditions == "SOF" & groups == 3)    
    subDataset[[1]][[4]] <- lapply(fullData, subset, conditions == "SOM" & groups == 3)    
    subDataset[[2]][[1]] <- lapply(fullData, subset, conditions == "FOF" & groups == 4)    
    subDataset[[2]][[2]] <- lapply(fullData, subset, conditions == "FOM" & groups == 4)    
    subDataset[[2]][[3]] <- lapply(fullData, subset, conditions == "SOF" & groups == 4)    
    subDataset[[2]][[4]] <- lapply(fullData, subset, conditions == "SOM" & groups == 4)
  }
  
  if(iDesign == 12)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, sessions == 1 & groups == 3)
    subDataset[[1]][[2]] <- lapply(fullData, subset, sessions == 2 & groups == 3)    
    subDataset[[1]][[3]] <- lapply(fullData, subset, sessions == 3 & groups == 3)       
    subDataset[[2]][[1]] <- lapply(fullData, subset, sessions == 1 & groups == 4)    
    subDataset[[2]][[2]] <- lapply(fullData, subset, sessions == 2 & groups == 4)    
    subDataset[[2]][[3]] <- lapply(fullData, subset, sessions == 3 & groups == 4)    
  }  
  
  if(iDesign == 13)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, motions == "F" & groups == 3)
    subDataset[[1]][[2]] <- lapply(fullData, subset, motions == "M" & groups == 3)       
    subDataset[[2]][[1]] <- lapply(fullData, subset, motions == "F" & groups == 4)    
    subDataset[[2]][[2]] <- lapply(fullData, subset, motions == "M" & groups == 4)        
  }
  
  if(iDesign == 14)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, orders == "FO" & groups == 3)
    subDataset[[1]][[2]] <- lapply(fullData, subset, orders == "SO" & groups == 3)       
    subDataset[[2]][[1]] <- lapply(fullData, subset, orders == "FO" & groups == 4)    
    subDataset[[2]][[2]] <- lapply(fullData, subset, orders == "SO" & groups == 4)        
  }
  
  if(iDesign == 15)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, sessions == 1 & motions == "F")
    subDataset[[1]][[2]] <- lapply(fullData, subset, sessions == 2 & motions == "F")    
    subDataset[[1]][[3]] <- lapply(fullData, subset, sessions == 3 & motions == "F")       
    subDataset[[2]][[1]] <- lapply(fullData, subset, sessions == 1 & motions == "M")    
    subDataset[[2]][[2]] <- lapply(fullData, subset, sessions == 2 & motions == "M")    
    subDataset[[2]][[3]] <- lapply(fullData, subset, sessions == 3 & motions == "M")         
  }
  
  if(iDesign == 16)
  {
    subDataset[[2]] <- list()
    subDataset[[1]][[1]] <- lapply(fullData, subset, sessions == 1 & orders == "FO")
    subDataset[[1]][[2]] <- lapply(fullData, subset, sessions == 2 & orders == "FO")    
    subDataset[[1]][[3]] <- lapply(fullData, subset, sessions == 3 & orders == "FO")       
    subDataset[[2]][[1]] <- lapply(fullData, subset, sessions == 1 & orders == "SO")    
    subDataset[[2]][[2]] <- lapply(fullData, subset, sessions == 2 & orders == "SO")    
    subDataset[[2]][[3]] <- lapply(fullData, subset, sessions == 3 & orders == "SO")        
  }
  
#   if(iDesign == )
#   {
#     subDataset[[1]] <- lapply(fullData, subset, orders == "FO" & motions == "M" & sessions == 1)
#     subDataset[[2]] <- lapply(fullData, subset, orders == "FO" & motions == "F" & sessions == 1)
#     subDataset[[3]] <- lapply(fullData, subset, orders == "SO" & motions == "M" & sessions == 1)
#     subDataset[[4]] <- lapply(fullData, subset, orders == "SO" & motions == "F" & sessions == 1)
#     subDataset[[5]] <- lapply(fullData, subset, orders == "FO" & motions == "M" & sessions == 2)
#     subDataset[[6]] <- lapply(fullData, subset, orders == "FO" & motions == "F" & sessions == 2)
#     subDataset[[7]] <- lapply(fullData, subset, orders == "SO" & motions == "M" & sessions == 2)
#     subDataset[[8]] <- lapply(fullData, subset, orders == "SO" & motions == "F" & sessions == 2)
#     subDataset[[9]] <- lapply(fullData, subset, orders == "FO" & motions == "M" & sessions == 3)
#     subDataset[[10]] <- lapply(fullData, subset, orders == "FO" & motions == "F" & sessions == 3)
#     subDataset[[11]] <- lapply(fullData, subset, orders == "SO" & motions == "M" & sessions == 3)
#     subDataset[[12]] <- lapply(fullData, subset, orders == "SO" & motions == "F" & sessions == 3)
#   } 
  
  dsDataList <- list()
  for(i in 1:length(subDataset))
  {
    dsDataList[[i]] <- list()
    for(j in 1:length(subDataset[[i]]))
    {  
      dsDataList[[i]][[j]] <- lapply(subDataset[[i]][[j]], FUN = function(x) {x$value})
    }
  }
  
  retVal = list(subDataset, dsDataList)
}

###########################################################################
####################       mean / max / min / etc.      ###################
###########################################################################
staR_getDistParams <- function(data, timeVals, iDesign)
{
  print("Getting Dist. Parameters...")
  tic()
  
  designMatrix <- staR_getDesignMatrix(iDesign)
  
  dfParamsList <- list()
  for(i in 1:designMatrix$nbRow)
  {
    dfParamsList[[i]] <- list()
    for(j in 1:designMatrix$nbCol)
    {
      xSD <- lapply(data[[i]][[j]], stde)
      xMean <- lapply(data[[i]][[j]], mean)
      xMax <- lapply(data[[i]][[j]], max)
      xMin <- lapply(data[[i]][[j]], min)
      
      if(is.null(timeVals)) {timeVals = seq(1, length(data[[i]][[j]]))}
      print(min(timeVals))
      print(max(timeVals))
      dfParamsList[[i]][[j]]  <- data.frame(times = timeVals, means = unlist(xMean), sds = unlist(xSD), maxs = unlist(xMax), mins = unlist(xMin))
    }
  }
  
  toc()
  print("Done!")
  
  dfParamsList
}