####################################################################
## Source : https://cran.rstudio.com/web/packages/erpR/erpR.pdf
####################################################################
library(erpR)

data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
erp(word$Fp1, smo=0, col="blue", startmsec=-200, endmsec=1500, ylim=c(-10,10))

data(ERPsets)
butterfly(base="Exp1_nonword_subj", numbers=1:20, electrode="Fp1", smo=0, startmsec=-200, endmsec=1500, erplist=ERPsets, outline=1, ylim=c(-20,20), out.col="red")

##############################
## WOW ! Nice stuff !
##############################
## Influence of subjects... 
data(ERPsets)
erp.infl(base="Exp1_word_subj", numbers=1:20, electrode="OZ", startmsec=-200, endmsec=1500, erplist=ERPsets)

data(ERPsets)
dat=erp.latency(base="Exp1_word_subj", numbers=1:20,
                win.ini=130, win.end=190, erplist=ERPsets, startmsec=-200, endmsec=1500,
                others=c(condition="word", interval="130-190"),
                name.dep="Ampl", format="long", peak.fun=max)

data(ERPsets)
dat=erp.peak(base="Exp1_word_subj", numbers=1:20,
             win.ini=130, win.end=190, erplist=ERPsets, startmsec=-200, endmsec=1500,
             others=c(condition="word", interval="130-190"), name.dep="Ampl", format="long", peak.fun=max)

##############################
## WOW ! Nice stuff !
##############################
data(ERPsets)
erp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20,
      electrode="Fp1", ylim=c(-10,10),  startmsec=-200,
      endmsec=1500, erplist1=ERPsets, erplist2=ERPsets,
      col=c("blue", "red"))


##############################
## WOW ! Nice stuff !
##############################
data(ERPsets)
word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
nonword=grandaverage("Exp1_nonword_subj", 1:20, erplist=ERPsets)
scalp(list(word), layout=1, ylim=10)
scalp(list(word, nonword), layout=1, ylim=10, legend=TRUE)


data(ERPsets)
#generate an hypothetic external variables
RT=rnorm(20, mean=500, sd=200)
scalp.cor("Exp1_word_subj", 1:20, external = RT, layout=1,
          erplist=ERPsets, ylim=10, legend=TRUE)


data(ERPsets)
# Notice that Subject 1 is clearly particularly influential
# for the average on OZ.
scalp.infl(base="Exp1_word_subj", numbers=1:20, layout=1,
           startmsec=-200, endmsec=1500, erplist=ERPsets)


data(ERPsets)
scalp.t("Exp1_word_subj", "Exp1_nonword_subj", 1:20, 1:20, smo=0,
        layout=1, ylims=10, startmsec=-200, endmsec=1500,
        color.list=c("blue", "red"), erplist1=ERPsets, erplist2=ERPsets)


if(require(akima)){
  data(ERPsets)
  word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
  # check if some electrodes are not present in the list
  # and create an object with these electrode names.
  notfound=topoplot(word, return.notfound=TRUE)
  #make a topoplot excluding not found electrode
  topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
           win.end=600, exclude=notfound)
}


if(require(akima)) {
  data(ERPsets)
  word=grandaverage("Exp1_word_subj", 1:20, erplist=ERPsets)
  # check if some electrodes are not present in the list
  # and create an object with these electrode names.
  notfound=topoplot(word, return.notfound=TRUE)
  #define a layout for
  mat=matrix(c(1,2), 1, 2, byrow=TRUE)
  layout(mat, widths=c(0.8, 0.2))
  #make a topoplot excluding not found electrode
  par(pty="s")
  topo.data=topoplot(word, startmsec=-200, endmsec=1500, win.ini=400,
                     win.end=600, exclude=notfound)
  #draw the palette on a new empty plot.
  par(pty="m", mar=c(0,0,0,0))
  plot.new()
  topoplot.palette(cols=topo.data$palette,
                   palette.lim=topo.data$zlim, p.height=0.6)
}
  
  
  
  # simulate some subjects
  subjRT=rnorm(20, 500, 100)
  #simulate the effects of three experimental conditions for each subject
  condA=rnorm(20, 50, 10)
  condB=rnorm(20, -40, 10)
  condC=rnorm(20, 20, 10)
  #create a data frame
  dat=data.frame(Subject=rep(1:20,3),
                 condition=c(rep("A", 20), rep("B", 20), rep("C", 20)),
                 RT=c(subjRT+condA, subjRT+condB, subjRT+condC ))
  #perform pairwise t.test
  tpairs(dat, "condition", "all", "RT", "Subject", var.equal=TRUE)