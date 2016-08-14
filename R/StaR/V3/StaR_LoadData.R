require(R.matlab)
require(tictoc)

###########################################################################
########################### Prep Structure Data ###########################
###########################################################################
staR_prepData <- function()
{
  cond_orders_vals = c("FO", "SO")
  cond_motion_vals = c("F", "M")
  conditions_vals = c("FOM", "FOF", "SOF", "SOM")
  #subjects_vals = c(seq(from = 1, to = 15), seq(from = 31, to = 45))
  subjects_vals = c(1,2,3,4)
  groups_vals = c(3,4)
  sessions_vals = c(1,2,3)
  
  # There are 2 logics here.
  # No 1 - You duplicate subject for each group.
  #        Gr 1 Sub 1
  #        Gr 1 Sub 2
  #        Gr 2 Sub 1
  #        Gr 2 Sub 2
  # No 2 - You have different subject (not paired - kinda).
  #        Gr 1 Sub 1
  #        Gr 1 Sub 2
  #        Gr 2 Sub 3
  #        Gr 2 Sub 4
  bDuplicateSubjects = TRUE
  
  if(bDuplicateSubjects){
    nbSubjectsTotal = length(groups_vals) * length(subjects_vals);
  }else {
    nbSubjectsTotal = length(subjects_vals);
  }
  
  nbRecordings = nbSubjectsTotal * length(sessions_vals)
  nbSets = nbRecordings * length(conditions_vals)
  
  motions = rep(cond_motion_vals, times = nbSubjectsTotal * length(sessions_vals) * length(cond_orders_vals), each = 1)
  orders = rep(cond_orders_vals, times = nbSubjectsTotal * length(sessions_vals), each = length(cond_motion_vals)) # Needs to repeat twice. FO FO SO SO to combine with orders... F M F M... FOF FOM SOF SOM

  #conditions = rep(conditions_vals, times = length(groups_vals) * (length(subjects_vals) / 2) * length(sessions_vals))
  conditions <- paste(orders, motions, sep="")
  
  sessions = rep(sessions_vals, times = nbSets / (length(conditions_vals) * length(sessions_vals)) , each = length(conditions_vals))
  groups = rep(groups_vals, times = 1, each = length(conditions) / length(groups_vals))
  
  if (bDuplicateSubjects) {
    subjects = rep(subjects_vals, times = length(groups_vals), each = length(conditions_vals) * length(sessions_vals)) # Be careful, it takes "conditions_vals" length!
  } else {
    subjects = rep(subjects_vals, times = 1, each = length(sessions_vals) * length(conditions_vals))
  }
    
  values = seq(1,nbSets)
  
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
staR_fillFromMatlab <- function(fileName, toolBox, dataStructure, bSmallSamples = FALSE, dataType = "ERP")
{
  print(paste("Opening Matlab File: ", fileName))
  mlData.d <- readMat(fileName)
 
  times <- list()
  if("times" %in% names(mlData.d))
  {
    times <- mlData.d$times
  } else
  {
    print("### change me! ### (staR_fillFromMatlab)")
    times <- 1:1536
    #times <- 1
  }
  
  freqs <- list()
  if("freqs" %in% names(mlData.d))
  {
    freqs <- mlData.d$freqs
  } else
  {
    freqs <- 1
  }
  
  print("Change the nbSets - for dimension of stuff")
  nbSets = 4 * 2 * 3
  nbPoints = length(freqs) * length(times)
  
  if(toolBox == "MPT")
  {
    fullData <- list()
    for(j in 1:nbPoints)
    {
      fullData[[j]] = dataStructure
      
      for (i in 0:(nbSets - 1)) 
      {
        fullData[[j]]$values[i*4 + 1] = mlData.d$linearProjectedMeasure[0 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 2] = mlData.d$linearProjectedMeasure[1 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 3] = mlData.d$linearProjectedMeasure[2 * nbPoints + j, i + 1]
        fullData[[j]]$values[i*4 + 4] = mlData.d$linearProjectedMeasure[3 * nbPoints + j, i + 1]
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
  
  if(bSmallSamples)
  {
    retVal <- staR_SmallSamples(fullData, times, freqs, dataType)    
    fullData <- retVal[[1]]
    times <- retVal[[2]]
    freqs <- retVal[[3]]
  }
  
  if(class(times) == "matrix") times <- times[1,]
  if(class(freqs) == "matrix") freqs <- freqs[1,]
  
  return(list(fullData, times, freqs))
}

# Sub sample the big dataset for testing. (speed)
staR_SmallSamples <- function(fullData, times, freqs, dataType)
{
  print("Small Samples...")
  
  if(is.null(fullData)) {fullData <- list()}
  if(is.null(times)) {timeData <- list()} else { timeData <- times}
  if(is.null(freqs)) {freqData <- list()} else { freqData <- freqs}
  
  if(dataType == "ERP")
  {
    lowLimit <- which(timeData > 0)[1]
    highLimit <- which(timeData < 600)
    highLimit <- highLimit[length(highLimit) - 1]
    
    fullData <- fullData[lowLimit:highLimit]
    timeData <- timeData[lowLimit:highLimit]
  }
  
  #freqData
  #timeVals <- timeVals[lowLimit:highLimit]
  if(dataType == "ERSP")
  {
    print(timeData)
    
    # Time
    lowLimit_time <- which(timeData > -100)[1] # From -100ms
    highLimit_time <- which(timeData < 600)  # To 600ms
    highLimit_time <- highLimit_time[length(highLimit_time) - 1]
    
    # Freq
    lowLimit_freq <- which(freqData > 8)[1] # From 8Hz
    highLimit_freq <- which(freqData < 16)   # To 16Hz
    highLimit_freq <- highLimit_freq[length(highLimit_freq) - 1]
    
    # TODO : Fix this, not the real "sub squarre in the matrix!"
    dataIndices <- NULL
    for(i in lowLimit_freq:highLimit_freq)
    {
      print(paste("lowLimit_time : ", lowLimit_time))
      print(paste("lowLimit_time : ", highLimit_time))
      print(paste("length(timeData)", length(timeData)))
      dataIndices <- c(dataIndices, seq(i * length(timeData) + lowLimit_time, i * length(timeData) + lowLimit_time + (highLimit_time - lowLimit_time)))
    }
    
    # Sub Time & Freq
    timeData <- timeData[lowLimit_time:highLimit_time]
    freqData <- freqData[lowLimit_freq:highLimit_freq]
    fullData <- fullData[dataIndices]
  }
  
  print(paste("Data subsampled ! ", length(fullData)))
  
  return(list(fullData, timeData, freqData))
}

###########################################################################
####################       mean / max / min / etc.      ###################
###########################################################################
staR_getDistParams <- function(data)
{
   print("Getting Dist. Parameters...")
   tic()
   
   dfParamsList <- list()
   for(i in 1:length(data)) # Level of Effects (1 = main effects)
   {
      dfParamsList[[i]] <- list()
      for(j in 1:length(data[[i]])) # Number of Variables 
      {
         dfParamsList[[i]][[j]] <- list()
         for(k in 1:length(data[[i]][[j]])) # Each Combinations / Values
         {
            #xSDE <- lapply(data[[i]][[j]][[k]], FUN = function(x) {stde(x$values)})
            xMean <- lapply(data[[i]][[j]][[k]], FUN = function(x) {mean(x$values)})
            #xMax <- lapply(data[[i]][[j]][[k]], FUN = function(x) {max(x$values)})
            #xMin <- lapply(data[[i]][[j]][[k]], FUN = function(x) {min(x$values)})
            
            #timeVals = seq(1, length(data[[i]][[j]][[k]]))
            #dfParamsList[[i]][[j]][[k]]  <- data.frame(means = unlist(xMean), sdes = unlist(xSDE), maxs = unlist(xMax), mins = unlist(xMin))
            dfParamsList[[i]][[j]][[k]]  <- xMean
         }
      }
   }
   toc()
   print("Done!")
   
   return (dfParamsList)
}

staR_selectData <- function(fullData, titles)
{
   # You are always using 1 variable with 1 value for which you might iterate on a second variable
   # while having 1,2 or 3 fixed variables.
   
   subDataset <- list()
   subDataTitle <- list()
   
   # For each level of effect/interactions. (1 = main effect, 2 = interactions, 3 = triple interactions...)
   for(curLevelEffect in 1:length(titles))
   {
      subDataset[[curLevelEffect]] <- list()
      subDataTitle[[curLevelEffect]] <- list()
      
      # For each level, get the variables.
      for(curVar in 1:length(titles[[curLevelEffect]]))
      {
         subDataset[[curLevelEffect]][[curVar]] <- list()
         subDataTitle[[curLevelEffect]][[curVar]] <- list()
         
         for(curVal in 1:length(titles[[curLevelEffect]][[curVar]]))
         {
            print(titles[[curLevelEffect]][[curVar]][[curVal]])
            
            tmpVarValComb = strsplit(titles[[curLevelEffect]][[curVar]][[curVal]], ";", fixed = TRUE)
            tmpVarVal = lapply(tmpVarValComb, FUN = function(x) {strsplit(x, "=", fixed = TRUE)})
            
            VarVal = tmpVarVal[[1]]
            
            # Main Effects. (or first level of effects... to be continued in > 1)
            logicalSubset <- grepl(VarVal[[1]][[2]], fullData[[1]][,VarVal[[1]][[1]]])
            tmpSubData <- lapply(fullData, FUN = function(x) {subset(x, logicalSubset)})
            
            # Interactions
            if(length(VarVal) > 1)
            {
               for(logicalCond in 2:length(VarVal))
               {
                  logicalSubset <- grepl(VarVal[[logicalCond]][[2]], tmpSubData[[1]][,VarVal[[logicalCond]][[1]]])
                  tmpSubData <- lapply(tmpSubData, FUN = function(x) {subset(x, logicalSubset)}) 
               }
            }  
            
            subDataset[[curLevelEffect]][[curVar]][[curVal]] <- tmpSubData
            
            # Log
            print(paste(length(tmpSubData[[1]][[1]]), " rows selected..."))
            
            # Debug
            print(tmpSubData[[1]])
         }
      }
   }
   
   return(subDataset)
}


staR_saveCustom <- function(dirPath, fileName, typeData, timeData, freqData, plotValues, plotTitles, mainTitle, nbRows, nbCols)
{
   print("Saving Matlab Custom StaR File....")
   
   writeMat(paste(dirPlots, "/", fileName, sep=""), typeData = typeData, timeData = timeData, freqData = freqData, plotValues = unlist(plotValues), plotTitles = unlist(plotTitles), mainTitle = mainTitle, nbRows = nbRows, nbCols = nbCols)
   
   print("Done!")  
}