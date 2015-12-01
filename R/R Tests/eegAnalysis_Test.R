## eegAnalysis Toolbox

##############################################
### plotEEG
##############################################
library(eegAnalysis)

#### Simulating some data
sim<-randEEG()

#### Plot some of the recordings:
#plotEEG(sim$data, sim$classes.Id, sim$rec.Id, which.classes = "ALL", which.rec=list(c(1,2,3),c(2,3,4)), which.channels=c(1,2,3), type ='original', m.a = 10)

#### Plot the spectrum
#plotEEG(sim$data, sim$classes.Id, sim$rec.Id, which.classes = "ALL", which.rec="ALL", which.channels=1, type ='spectrum', m.a = 10)

#### Plot the spectrum
#plotEEG(sim$data, sim$classes.Id, sim$rec.Id, which.classes = "ALL", which.rec="ALL", which.channels=5, type ='spectrum', m.a = c(5,20))


## INTERESSANT !

#### Plot the continuous wavelet transform
plotEEG(sim$data, sim$classes.Id, sim$rec.Id, which.classes = 1, which.rec=list(c(1)), which.channels=c(2), type = 'wavelet',wavelet="gaussian2", abs=TRUE,variance=1)

### Plot the T-value scalogram
plotEEG(sim$data, sim$classes.Id, sim$rec.Id, which.classes = "ALL",which.rec="ALL", which.channels=c(2), type ='T.pvalue',wavelet="gaussian2", abs=TRUE,variance=10)

##############################################
### plotwindows
##############################################
library(eegAnalysis)
### Simulating a data set
sim<-randEEG()

### Plotting the sum of the signals in windows of size 10
plotwindows(sim$data, sim$classes.Id , sim$rec.Id , which.classes = "ALL", which.rec="ALL", which.channels=c(1), win=10, stat="sum", power = 2, abs=FALSE, log=FALSE,complete = FALSE, mintomax=FALSE)

### Plotting the sorted sum of the signals in windows of size 10
plotwindows(sim$data, sim$classes.Id , sim$rec.Id , which.classes = "ALL", which.rec="ALL", which.channels=c(1), win=10, stat="sum", power = 2, abs=FALSE, log=FALSE,complete = FALSE, mintomax=TRUE)
            