# Here is the code to construct a Study Data Frame.
setwd("~/Documents/Playground/Test_YR")

cond_orders = c("FO", "SO")
cond_motions = c("F", "M")
conditions = c("FOM", "FOF", "SOF", "SOM")
subjects = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16)
groups = c(3,4)
sessions = c(1,2,3)

total = length(groups) * length(subjects) * length(sessions) * length(conditions)

rep_conditions = rep(conditions, times = length(groups) * length(subjects) * length(sessions))

rep_cond_motions = rep(cond_motions, times = length(groups) * length(subjects) * length(sessions) * length(cond_orders), each = 1)
rep_cond_orders = rep(cond_orders, times = length(groups) * length(subjects) * length(sessions), each = length(cond_motions))

rep_sessions = rep(sessions, times = total / (length(conditions) * length(sessions)) , each = length(conditions))
rep_subjects = rep(subjects, times = length(groups), each = length(rep_conditions) / length(groups) / length(subjects))
rep_groups = rep(groups, times = 1, each = length(rep_conditions) / length(groups))

values = seq(1,total)

fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
#fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))

# Load Matlab Data File.
library(R.matlab)
matlabData <- readMat("export_mpt.mat")

library(doMC)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)

fits <- foreach(j = 1:(400*20), .combine='rbind') %dopar%
{
  for (i in 0:(90 - 1)) 
  {
    fullData$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * 400 * 135 + j, i + 1]
  }

  fit <- aov(values ~ rep_groups*rep_subjects*rep_sessions, data=fullData)
}
