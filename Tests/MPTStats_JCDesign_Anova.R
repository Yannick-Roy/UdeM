# Here is the code to construct a Study Data Frame.
setwd("~/Documents/Playground/YR_Scripts")

###########################################################################
########################### Prep Structure Data ###########################
###########################################################################

cond_orders = c("FO", "SO")
cond_motions = c("F", "M")
conditions = c("FOM", "FOF", "SOF", "SOM")
subjects = c(seq(from = 1, to = 15), seq(from = 31, to = 45))
groups = c(3,4)
sessions = c(1,2,3)

rep_conditions = rep(conditions, times = length(groups) * (length(subjects) / 2) * length(sessions))

rep_cond_motions = rep(cond_motions, times = length(groups) * (length(subjects)  / 2) * length(sessions) * length(cond_orders), each = 1)
rep_cond_orders = rep(cond_orders, times = length(groups) * (length(subjects) / 2) * length(sessions), each = length(cond_motions))

rep_sessions = rep(sessions, times = 360 / (length(conditions) * length(sessions)) , each = length(conditions))
rep_subjects = rep(subjects, times = length(groups) / 2, each = length(rep_conditions) / length(groups) / (length(subjects)/2))
rep_groups = rep(groups, times = 1, each = length(rep_conditions) / length(groups))

values = seq(1,360)

fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))
#fullData = data.frame(rep_groups, rep_subjects, rep_sessions, rep_conditions, values) #, row.names = c("No", Group", "Subject", "Session", "Condition"))

fullData$rep_subjects <- factor(fullData$rep_subjects)
fullData$rep_groups <- factor(fullData$rep_groups)
fullData$rep_cond_orders <- factor(fullData$rep_cond_orders)
fullData$rep_cond_motions <- factor(fullData$rep_cond_motions)

library(R.matlab)
matlabData <- readMat("export_mpt.mat")

########################
## Parallel Stuff
########################
library(doMC)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)

fullData = list()
fit = list()
 
#foreach(j = 1:(400*135)) %dopar%
for(j in 1:(400 * 135))
{
  fullData[[j]] = data.frame(rep_groups, rep_subjects, rep_sessions, rep_cond_orders, rep_cond_motions, values) 
  
  for (i in 0:(90 - 1)) 
  {
    fullData[[j]]$values[i*4 + 1] = matlabData$linearProjectedMeasure[0 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 2] = matlabData$linearProjectedMeasure[1 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 3] = matlabData$linearProjectedMeasure[2 * 400 * 135 + j, i + 1]
    fullData[[j]]$values[i*4 + 4] = matlabData$linearProjectedMeasure[3 * 400 * 135 + j, i + 1]
  }
  
  if(j %% 100 == 0)
    print(j)
}

# To test on smaller subset !
subFullData = fullData[1:100]

###########################################################################
########################### /Prep Structure Data ##########################
###########################################################################



###########################################################################
############################## Select Data ################################
###########################################################################

# Get condition #1
sub_ax <- lapply(fullData, subset, rep_cond_orders == "FO" & rep_cond_motions == "M")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_cond_orders == "SO" & rep_cond_motions == "F")
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

## ------------------------------------------------------------------------

# Get condition #1
sub_ax <- lapply(fullData, subset, rep_groups == 3 & rep_cond_orders == "FO" & rep_cond_motions == "F")
# Subset only the values
sub_ax <- lapply(sub_ax, function(x) x$values)

# Get condition #2
sub_bx <- lapply(fullData, subset, rep_groups == 4 & rep_cond_orders == "FO" & rep_cond_motions == "F")
# Subset only the values
sub_bx <- lapply(sub_bx, function(x) x$values)

###########################################################################
############################## /Select Data ###############################
###########################################################################



###########################################################################
########################## Stats ANOVA & T-test ###########################
###########################################################################

#############################################
#Don't forget to FACTOR (at least subjects !)
#############################################

# subFullData$rep_subjects <- mclapply(subFullData, FUN = function(x) {x$rep_subjects <- factor(x$rep_subjects)})
# subjectsFactored <- data.frame(lapply(1:100, function(i, subFullData) { subFullData[[1]]$ep_subjects <- factor(subFullData[[i]]$rep_subjects)}, subFullData=subFullData))
# 
# lapply(1:100, function(i, subFullData) {subFullData[[i]]$rep_subjects <- subjectsFactored[[i]]}, subFullData = subFullData)

for(i in 1:54000)
{
  fullData[[i]]$rep_subjects <- factor(fullData[[i]]$rep_subjects)
  fullData[[i]]$rep_groups <- factor(fullData[[i]]$rep_groups)
  fullData[[i]]$rep_sessions <- factor(fullData[[i]]$rep_sessions)
  fullData[[i]]$rep_cond_orders <- factor(fullData[[i]]$rep_cond_orders)
  fullData[[i]]$rep_cond_motions <- factor(fullData[[i]]$rep_cond_motions)
  
  if(i %% 100 == 0)
    print(i)
}

#new_FullData2 <- lapply(subFullData, function(x) {data.frame(factor(x$rep_groups), factor(x$rep_subjects), factor(x$rep_session), factor(x$rep_cond_orders), factor(x$rep_cond_motions), x$value)})
#names(new_subFullData) <- c("group", "suject", "session", "order", "motion", "value")

fullData2 = fullData
library(doMC)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)

foreach(i = 1:54000) %dopar%
{
  fullData[[i]]$rep_sessions <- factor(fullData[[i]]$rep_sessions)
}


# T-Test
sub_ts <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$statistic}, sub_ax, sub_bx)
sub_ps <- mapply(function(x,y) {(t.test(x, y, paired = TRUE))$'p.value'}, sub_ax, sub_bx)

# Anova
#aFits <- lapply(fullData, FUN = function(x) {aov(values ~ rep_cond_orders * rep_cond_motions, x)})
#aFits <- llply(.data = subFullData, .fun = function(x) {aov(values ~ rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions, x)}, .parallel = TRUE)
#pVals <- lapply(aFits, FUN = function(x) {summary(x)[[1]][["Pr(>F)"]][[1]]})
#aFit <- aov(values ~ conditions, data = fullData[[1]])

aFits <- lapply(fullData, FUN = function(x) {aov(values ~ (rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions) + Error(rep_subjects/(rep_sessions * rep_cond_orders * rep_cond_motions)), x)})

#aFitsPVals <- lapply(aFits, FUN = function(x) {summary(x)[[1]][["Pr(>F)"]]})
#aFitsPVals_df <- as.data.frame(sapply(aFitsPVals, rbind))
#names(aFitsPVals_df) <- c("sessions","groups","cond_orders","cond_motions","sessions:groups","sessions:cond_orders","groups:rep_cond_orders","sessions:cond_motions","groups:cond_motions","cond_orders:cond_motions","sessions:groups:cond_orders","sessions:groups:cond_motions","sessions:cond_orders:cond_motions", "groups:cond_orders:cond_motions", "sessions:groups:cond_orders:cond_motions")

#aFitsPVals <- lapply(aFits, FUN = function(x) {summary(x)[[3]][[1]]$`Pr(>F)`[[1]]})

###########################################################################
######################### /Stats ANOVA & T-test ###########################
###########################################################################


###########################################################################
############################# Mixed Models ################################
###########################################################################

# Mixed Models (lme)
library(nlme)
mmFits <- lapply(fullData, FUN = function(x) {lme(values ~ rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions, data = x, random = ~ 1 | rep_subjects)})
mmFits1 <- lapply(fullData, FUN = function(x) {lme(values ~ rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions, data = x, random = ~ rep_sessions | rep_subjects)})
pVals <- lapply(mmFits1, FUN = function(x) {anova(x)[,"p-value"]})

matlab_pVals <- unlist(temp_pVals)
writeMat("RpVals.mat", R_Pvals=matlab_pVals)

#pValsSignifDF <- pValsDF[pValsDF < 0.05]

# Mixed Models (lmer)
library(lme4)
mmFitsFullModel_lmer <- lapply(fullData, FUN = function(x) {lmer(values ~ rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions * (1 | rep_subjects), x)})
mmFitsrestrictedModel_lmer <- lapply(subFullData, FUN = function(x) {lmer(values ~ rep_sessions * rep_groups * rep_cond_orders * rep_cond_motions * (1 | rep_subjects), x)})

KRmodcomp(mmFitsFullModel_lmer, mmFitsrestrictedModel_lmer)


###########################################################################
############################ /Mixed Models ################################
###########################################################################


###########################################################################
############################### Get Data ##################################
###########################################################################
#pValsDF <- lapply(pVals, FUN = function(x) {x <- data.frame(t(x))})
mmFitsPrincipalEffects_1 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[2]]})#'rep_sessions'})
mmFitsPrincipalEffects_2 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[3]]})#x$'rep_groups'})
mmFitsPrincipalEffects_3 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[4]]})#x$'rep_cond_orders'})
mmFitsPrincipalEffects_4 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[5]]})#x$'rep_cond_motions'})

mmFitsInteractions_1 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[6]]})#x$'rep_sessions:rep_groups'})
mmFitsInteractions_2 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[7]]})#x$'rep_sessions:rep_cond_orders'})
mmFitsInteractions_3 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[8]]})#x$'rep_groups:rep_cond_orders'})
mmFitsInteractions_4 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[9]]})#x$'rep_sessions:rep_cond_motions'})
mmFitsInteractions_5 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[10]]})#x$'rep_groups:rep_cond_motions'})

mmFitsInteractions_6 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[11]]})#x$'rep_cond_orders:rep_cond_motions'})

mmFitsInteractions_7 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[12]]})#x$'rep_sessions.rep_groups.rep_cond_motionsM'})
mmFitsInteractions_8 <- lapply(mmFits, FUN = function(x) {anova(x)$'p-value'[[13]]})#x$'rep_sessions.rep_groups.rep_cond_ordersSO'})

temp_ps <- unlist(mmFitsPrincipalEffects_1)
writeMat("RPVals_mm_session.mat", R_Pvals=temp_ps)
temp_ps <- unlist(mmFitsPrincipalEffects_2)
writeMat("RPVals_mm_group.mat", R_Pvals=temp_ps)
temp_ps <- unlist(mmFitsPrincipalEffects_3)
writeMat("RPVals_mm_order.mat", R_Pvals=temp_ps)
temp_ps <- unlist(mmFitsPrincipalEffects_4)
writeMat("RPVals_mm_motion.mat", R_Pvals=temp_ps)

#aFitsPVals_df <- as.data.frame(aFitsPVals, names = c("rep_sessions","rep_groups","rep_cond_orders","rep_cond_motions","rep_sessions:rep_groups","rep_sessions:rep_cond_orders","rep_groups:rep_cond_orders","rep_sessions:rep_cond_motions","rep_groups:rep_cond_motions","rep_cond_orders:rep_cond_motions","rep_sessions:rep_groups:rep_cond_orders","rep_sessions:rep_groups:rep_cond_motions","rep_sessions:rep_cond_orders:rep_cond_motions", "rep_groups:rep_cond_orders:rep_cond_motions", "rep_sessions:rep_groups:rep_cond_orders:rep_cond_motions"))
#aFitsPVals <- lapply(aFits, FUN = function(x) {unlist(summary(x)[[1]][["Pr(>F)"]]), names = c("rep_sessions","rep_groups","rep_cond_orders","rep_cond_motions","rep_sessions:rep_groups","rep_sessions:rep_cond_orders","rep_groups:rep_cond_orders","rep_sessions:rep_cond_motions","rep_groups:rep_cond_motions","rep_cond_orders:rep_cond_motions","rep_sessions:rep_groups:rep_cond_orders","rep_sessions:rep_groups:rep_cond_motions","rep_sessions:rep_cond_orders:rep_cond_motions", "rep_groups:rep_cond_orders:rep_cond_motions", "rep_sessions:rep_groups:rep_cond_orders:rep_cond_motions"))})

aFitsPrincipalEffects = list()

aFitsPrincipalEffects_1 <- lapply(aFits_summary, FUN = function(x) {x[[1]][[1]]$'Pr(>F)'[[1]]}) #group
aFitsPrincipalEffects_2 <- lapply(aFits_summary, FUN = function(x) {x[[2]][[1]]$'Pr(>F)'[[1]]}) #session
aFitsPrincipalEffects_3 <- lapply(aFits_summary, FUN = function(x) {x[[3]][[1]]$'Pr(>F)'[[1]]}) #order
aFitsPrincipalEffects_4 <- lapply(aFits_summary, FUN = function(x) {x[[4]][[1]]$'Pr(>F)'[[1]]}) #motion

aFitsInteractions = list()

aFitsInteractions_1 <- lapply(aFits_summary, FUN = function(x) {x[[2]][[1]]$'Pr(>F)'[[2]]}) #session:group
aFitsInteractions_2 <- lapply(aFits_summary, FUN = function(x) {x[[3]][[1]]$'Pr(>F)'[[2]]}) #group:order
aFitsInteractions_3 <- lapply(aFits_summary, FUN = function(x) {x[[4]][[1]]$'Pr(>F)'[[2]]}) #group:motion
aFitsInteractions_4 <- lapply(aFits_summary, FUN = function(x) {x[[5]][[1]]$'Pr(>F)'[[1]]}) #session:order
aFitsInteractions_5 <- lapply(aFits_summary, FUN = function(x) {x[[5]][[1]]$'Pr(>F)'[[2]]}) #session:group:order
aFitsInteractions_6 <- lapply(aFits_summary, FUN = function(x) {x[[6]][[1]]$'Pr(>F)'[[1]]}) #session:motion
aFitsInteractions_7 <- lapply(aFits_summary, FUN = function(x) {x[[6]][[1]]$'Pr(>F)'[[2]]}) #session:group:motion
aFitsInteractions_8 <- lapply(aFits_summary, FUN = function(x) {x[[7]][[1]]$'Pr(>F)'[[1]]}) #order:motion
aFitsInteractions_9 <- lapply(aFits_summary, FUN = function(x) {x[[7]][[1]]$'Pr(>F)'[[2]]}) #group:order:motion
aFitsInteractions_10 <- lapply(aFits_summary, FUN = function(x) {x[[8]][[1]]$'Pr(>F)'[[1]]}) #session:order:motion
aFitsInteractions_11 <- lapply(aFits_summary, FUN = function(x) {x[[8]][[1]]$'Pr(>F)'[[2]]}) #session:group:order:motion


temp_ps <- unlist(aFitsPrincipalEffects_1)
writeMat("RPVals_group.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsPrincipalEffects_2)
writeMat("RPVals_session.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsPrincipalEffects_3)
writeMat("RPVals_order.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsPrincipalEffects_4)
writeMat("RPVals_motion.mat", R_Pvals=temp_ps)


temp_ps <- unlist(aFitsInteractions_1)
writeMat("RPVals_session_group.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_2)
writeMat("RPVals_group_order.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_3)
writeMat("RPVals_group_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_4)
writeMat("RPVals_session_order.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_5)
writeMat("RPVals_session_group_order.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_6)
writeMat("RPVals_session_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_7)
writeMat("RPVals_session_group_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_8)
writeMat("RPVals_order_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_9)
writeMat("RPVals_group_order_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_10)
writeMat("RPVals_session_order_motion.mat", R_Pvals=temp_ps)
temp_ps <- unlist(aFitsInteractions_11)
writeMat("RPVals_session_group_order_motion.mat", R_Pvals=temp_ps)


##############
#lmerTest
#fitF
#fitR
#KRmodcomp(fitF, fitR)
##############

###########################################################################
############################## Ajust P's ##################################
###########################################################################

ps_full <- rbind(aFitsPrincipalEffects_1, aFitsPrincipalEffects_2, aFitsPrincipalEffects_3, aFitsPrincipalEffects_4)
ps2 <- apply(ps_full, 2, function(x) {p.adjust(x, method="fdr", n = length(x))})
#ps_full_adjusted <- p.adjust(ps_full[,2], method = "fdr", n = length(ps_full[,2]))

temp_ps <- unlist(ps2[1, ])
writeMat("RPVals_group_FDR.mat", R_Pvals=temp_ps)
temp_ps <- unlist(ps2[2, ])
writeMat("RPVals_session_FDR.mat", R_Pvals=temp_ps)
temp_ps <- unlist(ps2[3, ])
writeMat("RPVals_order_FDR.mat", R_Pvals=temp_ps)
temp_ps <- unlist(ps2[4, ])
writeMat("RPVals_motion_FDR.mat", R_Pvals=temp_ps)

###########################################################################
############################# /Ajust P's  #################################
###########################################################################


###########################################################################
############################# Export Data #################################
###########################################################################

sub_ps <- unlist(pVals)

# Save Data for MATLAB !
#writeMat("RVals.mat", R_Tvals=sub_ts, R_Pvals=sub_ps)
writeMat("RVals.mat", R_Pvals=sub_ps)

###########################################################################
############################ /Export Data #################################
###########################################################################


###########################################################################
############################# Import Data #################################
###########################################################################

load("/media/user/Data/fullDataFactor.RData")

###########################################################################
############################# /Import Data ################################
###########################################################################


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
