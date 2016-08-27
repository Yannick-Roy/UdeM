source("StaR_LoadData.R")

inputfile <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/MPT_exp_ersp_D1.mat"
outputfile <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/StaR PoC/RVals.mat"
outputfile.diffGroups <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/StaR PoC/RVals_DiffGroups.mat"
outputfile.diffConds <- "~/Documents/PhD/Stats Test/mTBI_SubClean_ShortMeasures/MPT_Export/StaR PoC/RVals_DiffConds.mat"

# PoC - Basics for ERSP
print("... StaR PoC Basics for ERSP ...")

# Creates the wide structure with dummy 'value' (y).
fullDataStructure <- staR_prepData()

# Get Matlab data.
matlabData <- staR_fillFromMatlab(inputfile, "MPT", fullDataStructure, bSmallSamples = FALSE, dataType = "ERSP")

# Get Data. (Validation should be made in a real pipeline!)
fullData = matlabData[[1]]
timeData = matlabData[[2]]
freqData = matlabData[[3]]

# Check Data Visually for the first point.
fullData[[1]]

# --------------------------------------------------------
# ----------- Difference Between Conditions --------------
# --------------------------------------------------------
# Get condition #1
sub_ax <- lapply(fullData, subset, conditions == "FOM")
# Subset only the values
sub_ax.values <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, conditions == "SOM")
# Subset only the values
sub_bx.values <- lapply(sub_bx, function(x) x$values)

# Get them combined.
sub_cx <- lapply(fullData, subset, conditions == "FOM" | conditions == "SOM")

# T-Test Paired (for sessions)
ttest_ts <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$statistic}, sub_ax.values, sub_bx.values)
ttest_ps <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$'p.value'}, sub_ax.values, sub_bx.values)

# Anova (should be repeated measures...)
aov_fits <- lapply(sub_cx, FUN = function(x) {aov(values ~ conditions, x)})
aov_ps <- lapply(aov_fits, FUN = function(x) {summary(x)[[1]][["Pr(>F)"]][[1]]})

avals.unlisted <- unlist(sub_ax.values) 
bvals.unlisted <- unlist(sub_bx.values)
avals.unlisted.means <- unlist(lapply(sub_ax.values, FUN=mean))
bvals.unlisted.means <- unlist(lapply(sub_bx.values, FUN=mean))

ttest_ps.unlisted <- unlist(ttest_ps)
aov_ps.unlisted <- unlist(aov_ps)

# Save Data for MATLAB !
writeMat(outputfile.diffConds, aVals=avals.unlisted.means, bVals = bvals.unlisted.means, R_aov_pVals=aov_ps.unlisted, R_ttest_pVals=ttest_ps.unlisted)

# Optional Plotting Option.
library(ggplot2)
require(reshape2)

tmat <- t(matrix(ttest_ps.unlisted, nrow = length(freqData), ncol = length(timeData)))
gg <- ggplot(melt(tmat), aes(x=Var1, y=Var2, fill=value))
gg <- gg + geom_raster()
gg <- gg + scale_fill_gradientn(colours =c("blue", "green", "red"), trans = "log")
grid.arrange(ggplotGrob(gg))


# --------------------------------------------------------
# ------------- Difference Between Groups ----------------
# --------------------------------------------------------
# Get group #1 (for t-test)
sub_ax <- lapply(fullData, subset, groups == "3" & conditions == "FOM")
# Subset only the values
sub_ax.values <- lapply(sub_ax, function(x) x$values)

# Get group #2 (for t-test)
sub_bx <- lapply(fullData, subset, groups == "4" & conditions == "FOM")
# Subset only the values
sub_bx.values <- lapply(sub_bx, function(x) x$values)

# Get them combined. (for the anova)
sub_cx <- lapply(fullData, subset, (groups == "3" & conditions =="FOM") | (groups == "4" & conditions =="FOM"))

# T-Test Paired (for sessions)
ttest_ts <- mapply(function(x,y) {(t.test(x, y, paired = FALSE))$statistic}, sub_ax.values, sub_bx.values)
ttest_ps <- mapply(function(x,y) {(t.test(x, y, paired = FALSE))$'p.value'}, sub_ax.values, sub_bx.values)

# Anova (should be repeated measures...)
aov_fits <- lapply(sub_cx, FUN = function(x) {aov(values ~ groups, x)})
aov_ps <- lapply(aov_fits, FUN = function(x) {summary(x)[[1]][["Pr(>F)"]][[1]]})

ttest_ps.unlisted <- unlist(ttest_ps)
aov_ps.unlisted <- unlist(aov_ps)

# Save Data for MATLAB !
writeMat(outputfile.diffGroups, R_aov_pVals=aov_ps.unlisted, R_ttest_pVals=ttest_ps.unlisted)

# Optional Plotting Option.
library(ggplot2)
require(reshape2)

tmat <- t(matrix(ttest_ps.unlisted, nrow = length(freqData), ncol = length(timeData)))
gg <- ggplot(melt(tmat), aes(x=Var1, y=Var2, fill=value))
gg <- gg + geom_raster()
gg <- gg + scale_fill_gradientn(colours =c("blue", "green", "red"), trans = "log")
grid.arrange(ggplotGrob(gg))









#-----
#Je devrais pouvoir acceder a unique($design field)
#Ensuite faire un sanity check in DESIGN pour confirmer que tout est clean.
#Faire un sanity check sur length est aussi interessant, mais bon, ca ajoute tres peu si tout est 2x2x2
#-----


# -----------------------------------------------------------
# -------------------- Previous PoC -------------------------
# -----------------------------------------------------------
library(R.matlab)
matlabData <- readMat("export_test_mpt.mat")

subjects = seq(1:14)
conditions = rep(c(1,2), 1, each = 14)
values = rep(c(1), 28)

fullData = list()
fit = list()

for(j in 1:(200*50))
{
  fullData[[j]] = data.frame(subjects, conditions, values)
  
  for (i in 1:28) 
  {
    fullData[[j]]$values[i] = cleanData[[i]][[1]][[j]]
  }
}

# Get condition #1
sub_ax <- lapply(fullData, subset, conditions == 1)
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)


# Get condition #2
sub_bx <- lapply(fullData, subset, conditions == 2)
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

sub_c = list()
for(k in 1:10000)
{
  sub_c[[k]] <- sub_ax[[k]] - sub_bx[[k]]
}

sub_means <- lapply(sub_c, mean)
sub_sds <- lapply(sub_c, sd)

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

# --------------------------------------------------------
# ----------------------- Simple Star --------------------
# --------------------------------------------------------
# Here is the code to construct a Study Data Frame.
setwd("~/Documents/PhD/Stats Test/mTBI_SubClean_Measures/MPT_Export/")

cond_orders = c("FO", "SO")
cond_motions = c("F", "M")
conditions = c("FOM", "FOF", "SOF", "SOM")
subjects = c(1,2,3,4)#,5,6,7,8,10,11,12,13,14,15,16)
groups = c(3,4)
sessions = c(1,2,3)

nbRecordings = length(groups) * length(subjects) * length(sessions)
nbSets = nbRecordings * length(conditions)

rep_conditions = rep(conditions, times = length(groups) * length(subjects) * length(sessions))

rep_cond_motions = rep(cond_motions, times = length(groups) * length(subjects) * length(sessions) * length(cond_orders), each = 1)
rep_cond_orders = rep(cond_orders, times = length(groups) * length(subjects) * length(sessions), each = length(cond_motions))

rep_sessions = rep(sessions, times = nbSets / (length(conditions) * length(sessions)) , each = length(conditions))
rep_subjects = rep(subjects, times = length(groups), each = length(rep_conditions) / length(groups) / length(subjects))
rep_groups = rep(groups, times = 1, each = length(rep_conditions) / length(groups))

values = seq(1,nbSets)

fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
#fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))

library(R.matlab)
matlabData <- readMat("MPT_exp_ersp_D1.mat")

if("freqs" %in% names(matlabData))
{  
  ersp.freqs = length(matlabData$freqs)
} else 
{
  ersp.freqs = 1
}

if("freqs" %in% names(matlabData))
{  
  ersp.times = length(matlabData$times)
} else 
{
  ersp.times = 1
}


library(doMC)
# library(doParallel)
# cl <- makeCluster(10)
# registerDoParallel(cl)

fullData = list()
fit = list()

for(j in 1:(ersp.freqs * ersp.times))
{
  fullData[[j]] = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) 
  
  for (i in 0:(nbRecordings - 1)) 
  {
    nbPointsOffset = ersp.times * ersp.freqs
    fullData[[j]]$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * nbPointsOffset + j, i + 1]
    fullData[[j]]$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * nbPointsOffset + j, i + 1]
    fullData[[j]]$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * nbPointsOffset + j, i + 1]
    fullData[[j]]$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * nbPointsOffset + j, i + 1]
  }
}

# Get condition #1
sub_ax <- lapply(fullData, subset, rep_cond_orders == "FO" & rep_cond_motions == "M")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_cond_orders == "SO" & rep_cond_motions == "M")
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

# --------------------------------------------------------
# --------------------------------------------------------

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


# -----------------------------------------------------------
# -------------------- More Exemples ! ----------------------
# -----------------------------------------------------------
# Get condition #1
sub_ax <- lapply(fullData, subset, rep_cond_orders == "FO" & rep_cond_motions == "M")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_cond_orders == "SO" & rep_cond_motions == "F")
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

# -----------------------------------------------------------

# Get condition #1
sub_ax <- lapply(fullData, subset, rep_groups == 3 & rep_cond_orders == "FO" & rep_cond_motions == "F")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_groups == 4 & rep_cond_orders == "FO" & rep_cond_motions == "F")
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

# -----------------------------------------------------------