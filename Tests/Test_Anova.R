data <- read.table(header=TRUE, text='
 subject sex   age before after
                   1   F   old    9.5   7.1
                   2   M   old   10.3  11.0
                   3   M   old    7.5   5.8
                   4   F   old   12.4   8.8
                   5   M   old   10.2   8.6
                   6   M   old   11.0   8.0
                   7   M young    9.1   3.0
                   8   F young    7.9   5.2
                   9   F   old    6.6   3.4
                   10   M young    7.7   4.0
                   11   M young    9.4   5.3
                   12   M   old   11.6  11.3
                   13   M young    9.9   4.6
                   14   F young    8.6   6.4
                   15   F young   14.3  13.5
                   16   F   old    9.2   4.7
                   17   M young    9.8   5.1
                   18   F   old    9.9   7.3
                   19   F young   13.0   9.5
                   20   M young   10.2   5.4
                   21   M young    9.0   3.7
                   22   F young    7.9   6.2
                   23   M   old   10.1  10.0
                   24   M young    9.0   1.7
                   25   M young    8.6   2.9
                   26   M young    9.4   3.2
                   27   M young    9.7   4.7
                   28   M young    9.3   4.9
                   29   F young   10.7   9.8
                   30   M   old    9.3   9.4
                   ')
# Make sure subject column is a factor, so that it's not treated as a continuous
# variable.
data$subject <- factor(data$subject)

# 1-Way Anova
aov1 <- aov(before ~ sex, data=data)
summary(aov1)
model.tables(aov1, "means")

# 2x2 between:
# IV: sex
# IV: age
# DV: after
# These two calls are equivalent
aov2 <- aov(after ~ sex*age, data=data)
aov2 <- aov(after ~ sex + age + sex:age, data=data)
summary(aov2)

library(tidyr)

# Convert it to long format
data_long <- gather(data, time, value, before:after)
# Look at first few rows
head(data_long)

# IV (within): time
# DV:          value
aov_time <- aov(value ~ time + Error(subject/time), data=data_long)
summary(aov_time)

# 2x2 mixed:
# IV between: age
# IV within:  time
# DV:         value
aov_age_time <- aov(value ~ age*time + Error(subject/time), data=data_long)
summary(aov_age_time)
