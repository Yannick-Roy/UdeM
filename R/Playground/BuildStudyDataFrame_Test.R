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

