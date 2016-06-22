# Here is the code to construct a Study Data Frame.
#setwd("~/Documents/Playground/Test_YR")

cond_orders_lbl = c("FO", "SO")
cond_motions_lbl = c("F", "M")
conditions_lbl = c("FOM", "FOF", "SOF", "SOM")
subjects_lbl = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16)
groups_lbl = c(3,4)
sessions_lbl = c(1,2,3)

conditions = rep(conditions_lbl, times = length(groups_lbl) * length(subjects_lbl) * length(sessions_lbl))

cond_motions = rep(cond_motions_lbl, times = length(groups_lbl) * length(subjects_lbl) * length(sessions_lbl) * length(cond_orders_lbl), each = 1)
cond_orders = rep(cond_orders_lbl, times = length(groups_lbl) * length(subjects_lbl) * length(sessions_lbl), each = length(cond_motions_lbl))

sessions = rep(sessions_lbl, times = 360 / (length(conditions_lbl) * length(sessions_lbl)) , each = length(conditions_lbl))
subjects = rep(subjects_lbl, times = length(groups_lbl), each = length(conditions) / length(groups_lbl) / length(subjects_lbl))
groups = rep(groups_lbl, times = 1, each = length(conditions) / length(groups_lbl))

values = seq(1,360)

fullData = data.frame(groups, subjects, sessions, cond_orders, cond_motions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
#fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))

# Load Matlab Data File.
library(R.matlab)
matlabData <- readMat("export_mpt.mat")

#============================================
#           == Parallel Stuff ==
#============================================
library(doMC)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)
#stopCluster(cl)

tempfit = list()

fit <- foreach(j = 1:(400*135), .combine='rbind') %dopar%
{
  for (i in 0:(90 - 1)) 
  {
    fullData$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * 400 * 135 + j, i + 1]
    fullData$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * 400 * 135 + j, i + 1]
  }
  
  # Subset
  subData <- subset(fullData, cond_orders == 'FO' & cond_motions == 'F')
  
  tempfit <- aov(values ~ groups, data=subData)
  
  summary(tempfit)
}

a <- foreach(i=1:(400*135), .combine=rbind) %do% fit[[i]]$`Pr(>F)`[1]

#============================================
#           == Sequential Stuff ==
#============================================
# fit = list()
# 
# for(j in 1:(400*135))
# {
#   for (i in 0:(90 - 1)) 
#   {
#     fullData$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * 400 * 135 + j, i + 1]
#     fullData$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * 400 * 135 + j, i + 1]
#     fullData$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * 400 * 135 + j, i + 1]
#     fullData$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * 400 * 135 + j, i + 1]
#   }
# 
#   fit[[j]] <- aov(values ~ groups*subjects*sessions*cond_motions*cond_orders, data=fullData)
# }
