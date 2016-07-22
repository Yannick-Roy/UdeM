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
########################       Select Data !     ##########################
###########################################################################
staR_selectData <- function(fullData, iDesign)
{
  ###############################################################
  ### Layer x Row x Col 
  ###
  ### example - Groups x Condititions
  ### Layer 1
  ###       C1      C2      C3      C4
  ### G3  [1,1,1] [1,1,2] [1,1,3] [1,1,4]  -Stat-
  ### G3  [1,2,1] [1,2,2] [1,2,3] [1,2,4]  -Stat-
  ###      -Stat-  -Stat-  -Stat-  -Stat-
  ###
  ### If you had Sessions, then you have 3 layers of this.
  ###############################################################
  subDataset <- list() 
  subDataset[[1]] <- list()
  subDataset[[1]][[1]] <- list()
  
  subDatasetVarVals <- list() 
  subDatasetVarVals[[1]] <- list()
  subDatasetVarVals[[1]][[1]] <- list()
  
  if(iDesign > 20) {iDesign <- iDesign - 20}
  
  if(iDesign == 1)
  {
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, conditions == "FOF")    
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, conditions == "FOM")
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, conditions == "SOF")
    subDataset[[1]][[1]][[4]] <- lapply(fullData, subset, conditions == "SOM")
    
    subDatasetVarVals[[1]][[1]][[1]] <- "conditions=FOF"
    subDatasetVarVals[[1]][[1]][[2]] <- "conditions=FOM"
    subDatasetVarVals[[1]][[1]][[3]] <- "conditions=SOF"
    subDatasetVarVals[[1]][[1]][[4]] <- "conditions=SOM"
    
    subDataset
  }
  
  if(iDesign == 2)
  {
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, sessions == 1)    
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, sessions == 2)
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, sessions == 3)
    
    subDatasetVarVals[[1]][[1]][[1]] <- "sessions=1"
    subDatasetVarVals[[1]][[1]][[2]] <- "sessions=2"
    subDatasetVarVals[[1]][[1]][[3]] <- "sessions=3"
  }
  
  if(iDesign == 3)
  {
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, motions == "F")    
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, motions == "M")
    
    subDatasetVarVals[[1]][[1]][[1]] <- "motions=F"
    subDatasetVarVals[[1]][[1]][[2]] <- "motions=M"
  }
  
  if(iDesign == 4)
  {
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, orders == "FO")    
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, orders == "SO")
    
    subDatasetVarVals[[1]][[1]][[1]] <- "orders=FO"
    subDatasetVarVals[[1]][[1]][[2]] <- "orders=SO"
  }
  
  if(iDesign == 5)
  {
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, groups == 3)    
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, groups == 4)
    
    subDatasetVarVals[[1]][[1]][[1]] <- "groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "groups=4"
  }
  
  if(iDesign == 11)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, conditions == "FOF" & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, conditions == "FOM" & groups == 3)    
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, conditions == "SOF" & groups == 3)    
    subDataset[[1]][[1]][[4]] <- lapply(fullData, subset, conditions == "SOM" & groups == 3)    
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, conditions == "FOF" & groups == 4)    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, conditions == "FOM" & groups == 4)    
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, conditions == "SOF" & groups == 4)    
    subDataset[[1]][[2]][[4]] <- lapply(fullData, subset, conditions == "SOM" & groups == 4)
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "conditions=FOF;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "conditions=FOM;groups=3"
    subDatasetVarVals[[1]][[1]][[3]] <- "conditions=SOF;groups=3"
    subDatasetVarVals[[1]][[1]][[4]] <- "conditions=SOM;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "conditions=FOF;groups=4"
    subDatasetVarVals[[1]][[2]][[2]] <- "conditions=FOM;groups=4"
    subDatasetVarVals[[1]][[2]][[3]] <- "conditions=SOF;groups=4"
    subDatasetVarVals[[1]][[2]][[4]] <- "conditions=SOM;groups=4"
  }
  
  if(iDesign == 12)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, sessions == 1 & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, sessions == 2 & groups == 3)    
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, sessions == 3 & groups == 3)       
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, sessions == 1 & groups == 4)    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, sessions == 2 & groups == 4)    
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, sessions == 3 & groups == 4)  
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "sessions=1;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "sessions=2;groups=3"
    subDatasetVarVals[[1]][[1]][[3]] <- "sessions=3;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "sessions=1;groups=4"
    subDatasetVarVals[[1]][[2]][[2]] <- "sessions=2;groups=4"
    subDatasetVarVals[[1]][[2]][[3]] <- "sessions=3;groups=4"
  }  
  
  if(iDesign == 13)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, motions == "F" & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, motions == "M" & groups == 3)       
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, motions == "F" & groups == 4)    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, motions == "M" & groups == 4)   
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "motions=F;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "motions=M;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "motions=F;groups=4"
    subDatasetVarVals[[1]][[2]][[2]] <- "motions=M;groups=4"
  }
  
  if(iDesign == 14)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, orders == "FO" & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, orders == "SO" & groups == 3)       
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, orders == "FO" & groups == 4)    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, orders == "SO" & groups == 4)     
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "orders=FO;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "orders=SO;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "orders=FO;groups=4"
    subDatasetVarVals[[1]][[2]][[2]] <- "orders=SO;groups=4"
  }
  
  if(iDesign == 15)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, sessions == 1 & motions == "F")
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, sessions == 2 & motions == "F")    
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, sessions == 3 & motions == "F")       
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, sessions == 1 & motions == "M")    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, sessions == 2 & motions == "M")    
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, sessions == 3 & motions == "M")  
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "sessions=1;motions=F"
    subDatasetVarVals[[1]][[1]][[2]] <- "sessions=2;motions=F"
    subDatasetVarVals[[1]][[1]][[3]] <- "sessions=3;motions=F"
    subDatasetVarVals[[1]][[2]][[1]] <- "sessions=1;motions=M"
    subDatasetVarVals[[1]][[2]][[2]] <- "sessions=2;motions=M"
    subDatasetVarVals[[1]][[2]][[3]] <- "sessions=3;motions=M"
  }
  
  if(iDesign == 16)
  {
    subDataset[[1]][[2]] <- list()
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, sessions == 1 & orders == "FO")
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, sessions == 2 & orders == "FO")    
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, sessions == 3 & orders == "FO")       
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, sessions == 1 & orders == "SO")    
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, sessions == 2 & orders == "SO")    
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, sessions == 3 & orders == "SO")  
    
    subDatasetVarVals[[1]][[2]] <- list()
    subDatasetVarVals[[1]][[1]][[1]] <- "sessions=1;orders=FO"
    subDatasetVarVals[[1]][[1]][[2]] <- "sessions=2;orders=FO"
    subDatasetVarVals[[1]][[1]][[3]] <- "sessions=3;orders=FO"
    subDatasetVarVals[[1]][[2]][[1]] <- "sessions=1;orders=SO"
    subDatasetVarVals[[1]][[2]][[2]] <- "sessions=2;orders=SO"
    subDatasetVarVals[[1]][[2]][[3]] <- "sessions=3;orders=SO"
  }
  
  if(iDesign == 17)
  {
    subDataset[[2]] <- list()
    subDataset[[2]][[1]] <- list()
    subDataset[[2]][[2]] <- list()
    subDataset[[1]][[1]] <- list()
    subDataset[[1]][[2]] <- list()
    
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, motions == "M" & sessions == 1 & groups == 3)
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, motions == "F" & sessions == 1 & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, motions == "M" & sessions == 2 & groups == 3)
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, motions == "F" & sessions == 2 & groups == 3)
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, motions == "M" & sessions == 3 & groups == 3)
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, motions == "F" & sessions == 3 & groups == 3)
    
    subDataset[[2]][[1]][[1]] <- lapply(fullData, subset, motions == "M" & sessions == 1 & groups == 4)
    subDataset[[2]][[2]][[1]] <- lapply(fullData, subset, motions == "F" & sessions == 1 & groups == 4)
    subDataset[[2]][[1]][[2]] <- lapply(fullData, subset, motions == "M" & sessions == 2 & groups == 4)
    subDataset[[2]][[2]][[2]] <- lapply(fullData, subset, motions == "F" & sessions == 2 & groups == 4)
    subDataset[[2]][[1]][[3]] <- lapply(fullData, subset, motions == "M" & sessions == 3 & groups == 4)
    subDataset[[2]][[2]][[3]] <- lapply(fullData, subset, motions == "F" & sessions == 3 & groups == 4)
    
    subDatasetVarVals[[2]] <- list()
    subDatasetVarVals[[2]][[1]] <- list()
    subDatasetVarVals[[2]][[2]] <- list()
    subDatasetVarVals[[1]][[1]] <- list()
    subDatasetVarVals[[1]][[2]] <- list()
    
    subDatasetVarVals[[1]][[1]][[1]] <- "motions=M;sessions=1;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "motions=F;sessions=1;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "motions=M;sessions=2;groups=3"
    subDatasetVarVals[[1]][[2]][[2]] <- "motions=F;sessions=2;groups=3"
    subDatasetVarVals[[1]][[1]][[3]] <- "motions=M;sessions=3;groups=3"
    subDatasetVarVals[[1]][[2]][[3]] <- "motions=F;sessions=3;groups=3"
    
    subDatasetVarVals[[2]][[1]][[1]] <- "motions=M;sessions=1;groups=4"
    subDatasetVarVals[[2]][[2]][[1]] <- "motions=F;sessions=1;groups=4"
    subDatasetVarVals[[2]][[1]][[2]] <- "motions=M;sessions=2;groups=4"
    subDatasetVarVals[[2]][[2]][[2]] <- "motions=F;sessions=2;groups=4"
    subDatasetVarVals[[2]][[1]][[3]] <- "motions=M;sessions=3;groups=4"
    subDatasetVarVals[[2]][[2]][[3]] <- "motions=F;sessions=3;groups=4"
  }
  
  if(iDesign == 18)
  {
    subDataset[[2]] <- list()
    subDataset[[2]][[1]] <- list()
    subDataset[[2]][[2]] <- list()
    subDataset[[1]][[1]] <- list()
    subDataset[[1]][[2]] <- list()
    
    subDataset[[1]][[1]][[1]] <- lapply(fullData, subset, orders == "FO" & sessions == 1 & groups == 3)
    subDataset[[1]][[2]][[1]] <- lapply(fullData, subset, orders == "SO" & sessions == 1 & groups == 3)
    subDataset[[1]][[1]][[2]] <- lapply(fullData, subset, orders == "FO" & sessions == 2 & groups == 3)
    subDataset[[1]][[2]][[2]] <- lapply(fullData, subset, orders == "SO" & sessions == 2 & groups == 3)
    subDataset[[1]][[1]][[3]] <- lapply(fullData, subset, orders == "FO" & sessions == 3 & groups == 3)
    subDataset[[1]][[2]][[3]] <- lapply(fullData, subset, orders == "SO" & sessions == 3 & groups == 3)
    
    subDataset[[2]][[1]][[1]] <- lapply(fullData, subset, orders == "FO" & sessions == 1 & groups == 4)
    subDataset[[2]][[2]][[1]] <- lapply(fullData, subset, orders == "SO" & sessions == 1 & groups == 4)
    subDataset[[2]][[1]][[2]] <- lapply(fullData, subset, orders == "FO" & sessions == 2 & groups == 4)
    subDataset[[2]][[2]][[2]] <- lapply(fullData, subset, orders == "SO" & sessions == 2 & groups == 4)
    subDataset[[2]][[1]][[3]] <- lapply(fullData, subset, orders == "FO" & sessions == 3 & groups == 4)
    subDataset[[2]][[2]][[3]] <- lapply(fullData, subset, orders == "SO" & sessions == 3 & groups == 4)
    
    subDatasetVarVals[[2]] <- list()
    subDatasetVarVals[[2]][[1]] <- list()
    subDatasetVarVals[[2]][[2]] <- list()
    subDatasetVarVals[[1]][[1]] <- list()
    subDatasetVarVals[[1]][[2]] <- list()
    
    subDatasetVarVals[[1]][[1]][[1]] <- "orders=FO;sessions=1;groups=3"
    subDatasetVarVals[[1]][[2]][[1]] <- "orders=SO;sessions=1;groups=3"
    subDatasetVarVals[[1]][[1]][[2]] <- "orders=FO;sessions=2;groups=3"
    subDatasetVarVals[[1]][[2]][[2]] <- "orders=SO;sessions=2;groups=3"
    subDatasetVarVals[[1]][[1]][[3]] <- "orders=FO;sessions=3;groups=3"
    subDatasetVarVals[[1]][[2]][[3]] <- "orders=SO;sessions=3;groups=3"
    
    subDatasetVarVals[[2]][[1]][[1]] <- "orders=FO;sessions=1;groups=4"
    subDatasetVarVals[[2]][[2]][[1]] <- "orders=SO;sessions=1;groups=4"
    subDatasetVarVals[[2]][[1]][[2]] <- "orders=FO;sessions=2;groups=4"
    subDatasetVarVals[[2]][[2]][[2]] <- "orders=SO;sessions=2;groups=4"
    subDatasetVarVals[[2]][[1]][[3]] <- "orders=FO;sessions=3;groups=4"
    subDatasetVarVals[[2]][[2]][[3]] <- "orders=SO;sessions=3;groups=4"
  }
  
  # Clean & Easy way of putting the titles. But only supports 1 value for 1 variable. (uses the "unique" approach)
  # Otherwise, it would have to be more specific and more "design" dependant. Like the VarVal approach (committed July 5th 2016)
  if(iDesign <= 18)
  {
    dsDataList <- list()
    dsDataList.Title <- list()
    for(i in 1:length(subDataset))
    {
      dsDataList[[i]] <- list()
      dsDataList.Title[[i]] <- list()
      for(j in 1:length(subDataset[[i]]))
      { 
        dsDataList[[i]][[j]] <- list()
        dsDataList.Title[[i]][[j]] <- list()
        for(k in 1:length(subDataset[[i]][[j]]))
        {  
          dsDataList[[i]][[j]][[k]] <- lapply(subDataset[[i]][[j]][[k]], FUN = function(x) {x$value})
          
          dsDataList.Title[[i]][[j]][[k]] <- list()
          tryCatch({
            if(length(unique(subDataset[[i]][[j]][[k]][[1]]$groups)) == 1) {dsDataList.Title[[i]][[j]][[k]] <- paste(paste(dsDataList.Title[[i]][[j]][[k]], "groups=", subDataset[[i]][[j]][[k]][[1]]$groups[[1]], sep=""), " | ")}
            if(length(unique(subDataset[[i]][[j]][[k]][[1]]$orders)) == 1) {dsDataList.Title[[i]][[j]][[k]] <- paste(paste(dsDataList.Title[[i]][[j]][[k]], "orders=", subDataset[[i]][[j]][[k]][[1]]$orders[[1]], sep=""), " | ")}
            if(length(unique(subDataset[[i]][[j]][[k]][[1]]$motions)) == 1) {dsDataList.Title[[i]][[j]][[k]] <- paste(paste(dsDataList.Title[[i]][[j]][[k]], "motions=", subDataset[[i]][[j]][[k]][[1]]$motions[[1]], sep=""), " | ")}
            if(length(unique(subDataset[[i]][[j]][[k]][[1]]$sessions)) == 1) {dsDataList.Title[[i]][[j]][[k]] <- paste(paste(dsDataList.Title[[i]][[j]][[k]], "sessions=", subDataset[[i]][[j]][[k]][[1]]$sessions[[1]], sep=""), " | ")}
            if(length(unique(subDataset[[i]][[j]][[k]][[1]]$conditions)) == 1) {dsDataList.Title[[i]][[j]][[k]] <- paste(paste(dsDataList.Title[[i]][[j]][[k]], "conditions=", subDataset[[i]][[j]][[k]][[1]]$conditions[[1]], sep=""), " | ")}
          }, error = function(e) {
            dsDataList.Title[[i]][[j]][[k]] = "N/A"
            print(paste("==== ERROR in staR_selectData :", e))
          })
        }
      }
    }
  }else{
    subDataset <- list()
    dsDataList <- list()
    dsDataList.Title <- list()
  }
  retVal = list(subDataset, dsDataList, subDatasetVarVals)#dsDataList.Title)
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
  if(designMatrix$nbLayer >= 1)
  {
    for(i in 1:designMatrix$nbLayer)
    {
      dfParamsList[[i]] <- list()
      if(designMatrix$nbRow >= 1)
      {
        for(j in 1:designMatrix$nbRow)
        {
          dfParamsList[[i]][[j]] <- list()
          if(designMatrix$nbCol >= 1)
          {
            for(k in 1:designMatrix$nbCol)
            {
              xSDE <- lapply(data[[i]][[j]][[k]], stde)
              xMean <- lapply(data[[i]][[j]][[k]], mean)
              xMax <- lapply(data[[i]][[j]][[k]], max)
              xMin <- lapply(data[[i]][[j]][[k]], min)
              
              if(is.null(timeVals)) {timeVals = seq(1, length(data[[i]][[j]][[k]]))}
              dfParamsList[[i]][[j]][[k]]  <- data.frame(times = timeVals, means = unlist(xMean), sdes = unlist(xSDE), maxs = unlist(xMax), mins = unlist(xMin))
            }
          }
        }
      }
    }
  }
  toc()
  print("Done!")
  
  dfParamsList
}