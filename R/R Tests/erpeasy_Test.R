####################################################################
## Source : https://cran.r-project.org/web/packages/erp.easy/erp.easy.pdf
####################################################################
library(erp.easy)

data(ERPdata)
#grandaverage(ERPdata, electrodes = "V78")
#mosaic(ERPdata, electrodes = "V78")
m.measures(ERPdata, electrodes = "V78", window = c(1000, 1500))

p.measures(ERPdata, electrodes = "V78", window = c(250, 500))

butterfly(ERPdata, electrodes = "V78", stim = 1)

# Calculate a difference wave
Negative = ERPdata[1:6765, ]
Neutral = ERPdata[6766:13530, ]
refactor.neg <- factor(Negative$Stimulus)
refactor.neut <- factor(Neutral$Stimulus)
Negative$Stimulus <- refactor.neg
Neutral$Stimulus <- refactor.neut
difference <- dif.wave(Negative, Neutral, keep = "y")
grandaverage(difference, "V78") # plot the grand average difference wave

library(erp.easy)
data(ERPdata)
grandaverage(ERPdata, electrodes = "V78")
mosaic(ERPdata, electrodes = "V78")
meanData <- m.measures(ERPdata, electrodes = "V78", window = c(1000, 1500))

# Calculate peak latency and amplitude
peakData <- p.measures(ERPdata, electrodes = "V78", window = c(250, 1000))