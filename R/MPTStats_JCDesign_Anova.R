# Here is the code to construct a Study Data Frame.
setwd("~/Documents/Playground/Test_YR")

cond_orders = c("FO", "SO")
cond_motions = c("F", "M")
conditions = c("FOM", "FOF", "SOF", "SOM")
subjects = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16)
groups = c(3,4)
sessions = c(1,2,3)

rep_conditions = rep(conditions, times = length(groups) * length(subjects) * length(sessions))

rep_cond_motions = rep(cond_motions, times = length(groups) * length(subjects) * length(sessions) * length(cond_orders), each = 1)
rep_cond_orders = rep(cond_orders, times = length(groups) * length(subjects) * length(sessions), each = length(cond_motions))

rep_sessions = rep(sessions, times = 360 / (length(conditions) * length(sessions)) , each = length(conditions))
rep_subjects = rep(subjects, times = length(groups), each = length(rep_conditions) / length(groups) / length(subjects))
rep_groups = rep(groups, times = 1, each = length(rep_conditions) / length(groups))

values = seq(1,360)

fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
#fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))

library(R.matlab)
matlabData <- readMat("export_mpt.mat")

library(doMC)
# library(doParallel)
# cl <- makeCluster(10)
# registerDoParallel(cl)

fullData = list()
fit = list()

for(j in 1:(10))#(400*135))
{
  fullData[[j]] = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) 
  
  for (i in 0:(90 - 1)) 
  {
    fullData[[j]]$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * 400 * 135 + j, i + 1]
  }
}

# Get condition #1
sub_ax <- lapply(fullData, subset, rep_cond_orders == "FO")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_cond_orders == "SO")
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

# T-Test
sub_ts <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$statistic}, sub_ax, sub_bx)
sub_ps <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$'p.value'}, sub_ax, sub_bx)

# Anova
aFits <- lapply(fullData, FUN = function(x) {aov(values ~ conditions, x)})
pVals <- lapply(aFits, FUN = function(x) {summary(x)[[1]][["Pr(>F)"]][[1]]})
#aFit <- aov(values ~ conditions, data = fullData[[1]])

sub_ps <- unlist(pVals)

# Save Data for MATLAB !
#writeMat("RVals.mat", R_Tvals=sub_ts, R_Pvals=sub_ps)
writeMat("RVals.mat", R_Pvals=sub_ps)


# Manual T-Test !
# sub_c = list()
# for(k in 1:10000)
# {
#   sub_c[[k]] <- sub_ax[[k]] - sub_bx[[k]]
# }
# 
# sub_means <- lapply(sub_c, mean)
# sub_sds <- lapply(sub_c, sd)

## MATLAB t_test !
# function [tval, df] = ttest_cell(a,b)
# 
# if nargin < 1
# help ttest_cell;
# return;
# end;
# 
# if iscell(a), b = a{2}; a = a{1}; end;
# tmpdiff = a-b;
# diff = mymean(tmpdiff,    myndims(a));
# sd   = mystd( tmpdiff,[], myndims(a));
# tval = diff./sd*sqrt(size(a, myndims(a)));
# df   = size(a, myndims(a))-1;
# 
# % check values againg Matlab statistics toolbox
# %[h p ci stats] = ttest(a', b');
# % [ tval stats.tstat' ]  
