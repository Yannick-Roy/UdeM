####################################################################
## Source : https://cran.r-project.org/web/packages/
##            LMERConvenienceFunctions/LMERConvenienceFunctions.pdf
####################################################################

# if("LCFdata" %in% .packages(all.available=TRUE)){
#   library(LCFdata) 
#   library(lme4)
#   # bfFixefLMER -- backfit on t
#   data(eeg)
#   eeg <- eeg[eeg$Time >= 100 & eeg$Time <= 175, , drop = TRUE]
#   
#   # fit initial model
#   m1 = lmer(Fz ~ FreqB * LengthB * WMC + (1 | Subject) + 
#               (1 | Item), data = eeg)
#   
#   # look at model criticism plots
#   mcp.fnc(m1, trim = 2.5)
#   
#   # trim data on initial model residuals
#   eeg.trimmed = romr.fnc(m1, eeg, trim = 2.5)
#   eeg = eeg.trimmed$data
#   
#   # update initial model on trimmed data
#   mB = update(m1)
#   dev.new()
#   mcp.fnc(mB, trim = 2.5)
#   
#   # backfit fixed effects on F
#   m2 = bfFixefLMER_t.fnc(mB, eeg, log.file = FALSE)#, llrt = FALSE)
#   print(m2,corr=FALSE)
#   
#   m3<-ffRanefLMER.fnc(m2, eeg, ran.effects = c("(FreqB | Subject)",
#                                                "(LengthB | Subject)", "(WMC | Item)"))
#   print(m3,corr=FALSE)
#    
#   plotLMER3d.fnc(m3, "LengthB", "WMC")
#   devAskNewPage(ask=FALSE)
# }




if(try(require(LCFdata,quietly=TRUE))){
  ############################################
  #            Load and format data.         #
  ############################################
  require(LCFdata)
  data(eeg)
  
  # restrict to electrode Fz and 80--180 ms window
  eeg <- eeg[eeg$Time >= 80 & eeg$Time <= 180, ]
  eeg <- eeg[, c("Subject", "Item", "Time", "Fz",
                 "FreqB", "LengthB", "WMC")]
  
  # mean center FreqB
  eeg$FreqBc <- eeg$FreqB - mean(eeg$FreqB)
  # split FreqBc into 3 categories. Doesn't make sense, 
  # but it's merely for example
  eeg$FreqBdc <- "high"
  eeg$FreqBdc[eeg$FreqBc<=quantile(eeg$FreqBc)[3]] <- "mid"
  eeg$FreqBdc[eeg$FreqBc<=quantile(eeg$FreqBc)[2]] <- "low"
  eeg$FreqBdc <- as.factor(eeg$FreqBdc)
  eeg$FreqBdc <- relevel(eeg$FreqBdc, "low")
  
  # mean center LengthB
  eeg$LengthBc <- eeg$LengthB - mean(eeg$LengthB)
  
  # mean center WMC
  eeg$WMCc <- eeg$WMC - mean(eeg$WMC)
  
  ############################################
  #      Demonstrate plotDensity3d.fnc.      #
  ############################################
  plotDensity3d.fnc(x = sort(unique(eeg$WMCc)), 
                    y = sort(unique(eeg$LengthBc)))
  
  ############################################
  #        Demonstrate plotRaw3d.fnc.        #
  ############################################
  plotRaw3d.fnc(data = eeg, response = "Fz", pred = "WMCc",
                intr = "LengthBc", plot.type = "persp", theta = 150)
  
  ############################################
  #       Analyze data. Demonstrate model    #
  #       selection, and diagnostic plots.   #
  #       Also demonstrate forward fitting   #
  #       of random effects and back fitting #
  #       of fixed effects. Finally,         #
  #       demonstrate pamer.fnc.             # 
  ############################################
  # fit initial model
  m0 <- lmer(Fz ~ (FreqBdc + LengthBc + WMCc)^2 + (1 | Subject), 
             data = eeg)
  m1 <- lmer(Fz ~ (FreqBdc + LengthBc + WMCc)^2 + (1 | Subject) +
               (1 | Item), data = eeg)
  
  # which model to choose?
  relLik(m0, m1)
  
  # choose m1
  # check model assumptions 
  mcp.fnc(m1)
  
  # remove outliers
  eeg <- romr.fnc(m1, eeg, trim = 2.5)
  eeg$n.removed
  eeg$percent.removed
  eeg<-eeg$data
  
  # update model
  m1 <- lmer(Fz ~ (FreqBdc + LengthBc + WMCc)^2 + (1 | Subject) +
               (1 | Item), data = eeg)
  
  # re-check model assumptions 
  mcp.fnc(m1)
  
  # forward-fit random effect structure (simple for the purposes
  # of the example).
  m2 <- ffRanefLMER.fnc(model = m1, ran.effects = 
                          c("(0 + LengthBc | Subject)", "(0 + WMCc | Item)"), 
                        log.file = FALSE)
  
  # backfit model m2. In this case, could use bfFixefLMER_t.fnc instead.
  m3 <- bfFixefLMER_F.fnc(m2, log.file = FALSE)
  
  # The calls to ffRanefLMER.fnc and bfFixefLMER_F.fnc could 
  # be replaced by a call to fitLMER.fnc. In this latter case, however, 
  # bfFixefLMER_F.fnc would be called first, then the random effect 
  # structure would be forward fitted, and finally teh fixed effects
  # would be backfitted again.
  m3b <- fitLMER.fnc(model = m1, ran.effects = c("(0 + LengthBc | Subject)",
                                                 "(0 + WMCc | Item)"), backfit.on = "F", log.file = FALSE)
  pamer.fnc(m3b)
  # The results are the same. This may not necessarily be the case
  # elsewhere. First forward fitting the random effect structure and
  # then backfitting the fixed effects, potentially pruning irrelevant 
  # random effects, is probably the best approach. Nonetheless, there is 
  # no hard evidence to this effect.
  
  # check model assumptions 
  mcp.fnc(m3)
  
  # check significance of model terms
  pamer.fnc(m3)
  
  ############################################
  #       Demonstrate mcposthoc.fnc and      #
  #       summary.mcposthoc.                 #
  ############################################
  # Only the intercept is significant. For purposes of the 
  # example, let's perform a posthoc analysis on FreqBdc on
  # model m2.
  m2.ph <- mcposthoc.fnc(model = m2, var = list(ph1 = "FreqBdc"))
  
  # Now check if and how the different levels differ between
  # each other. First check high vs mid and high vs low:
  summary(m2.ph, term = "FreqBdchigh") 
  # Then low vs mid (the low vs high row is redundant from the 
  # above summary):
  summary(m2.ph, term = "FreqBdclow")
  # Note that none of the levels differ from each other. Indeed, 
  # the backfitting process indicated that the model only has an 
  # intercept (i.e., the FreqBc factor variable was not significant).
  
  # Just to show how one would look at posthocs for interactions. Let's 
  # look at the effect of Length at each FreqB bin:
  summary(object = m2.ph, term = "LengthBc")
  # Does Length effect different Freq bins? Start with low 
  # versus mid and high
  smry <- summary(object = m2.ph, term = "FreqBdclow:LengthBc")
  # then mid versus low and high
  smry <- summary(object = m2.ph, term = "FreqBdcmid:LengthBc")
  
  ############################################
  #       Demonstrate `revived' version of   #
  #       plotLMER.fnc and plotLMER3d.fnc.   #
  ############################################
  # Generate plot for Length X Freq with function plotLMER.fnc.
  plotLMER.fnc(m2, pred = "LengthBc", intr = list("FreqBdc", 
                                                  levels(eeg$FreqBdc), "beg", list(1 : 3, 1 : 3)))
  
  # Plotting the Length:WMC interaction with plotLMER3d.fnc. It'll
  # take a little bit of time.
  plotLMER3d.fnc(m2,"LengthBc","WMCc")
  # Plot it a second time to demonstrate caching. You can notice the 
  # speed-up.
  plotLMER3d.fnc(m2,"LengthBc","WMCc")
  
  
  ############################################
  #       Demonstrate modeling and           #
  #       backfitting of glmer.              #
  ############################################
  # Split FreqBc into 2 categories.
  eeg$FreqBdc <- "high"
  eeg$FreqBdc[eeg$FreqBc<=median(eeg$FreqBc)] <- "low"
  eeg$FreqBdc <- as.factor(eeg$FreqBdc)
  eeg$FreqBdc <- relevel(eeg$FreqBdc, "low")
  
  # Fit glmer model.
  m4 <- glmer(FreqBdc ~ (Fz + LengthBc + WMCc)^2 + (1 | Subject),
              family = "binomial", data = eeg)
  summary(m4)
  
  # Back fit fixed effects, forward fit random effects, and then
  # re-back fit fixed effects. Need to set argument backfit.on to "t".
  m5 <- fitLMER.fnc(model = m4, ran.effects = "(0 + LengthBc | Subject)",
                    backfit.on = "t", log.file = FALSE)
  summary(m5)
  
  # Plot the 2-way interaction.
  plotLMER.fnc(m5, pred = "Fz", intr = list("LengthBc", 
                                            quantile(eeg$LengthBc), "med",list(1:5,1:5)))
  
  # Look at the same plot, but in 3d.
  plotLMER3d.fnc(m5, pred = "Fz", intr = "LengthBc")
  
  ############################################
  #       Test backfitting on AIC,           #
  #       BIC, llrt, relLik.AIC, and         #
  #       relLik.BIC.                        #
  ############################################
  # AIC
  m.test <- bfFixefLMER_F.fnc(m2, method = "AIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m2, method = "AIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m4, method = "AIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_F.fnc(m4, method = "AIC",
                              log.file = FALSE)
  
  # BIC
  m.test <- bfFixefLMER_F.fnc(m2, method = "BIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m2, method = "BIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m4, method = "BIC",
                              log.file = FALSE)
  
  # llrt
  m.test <- bfFixefLMER_F.fnc(m2, method = "llrt",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m2, method = "llrt",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m4, method = "llrt",
                              log.file = FALSE)
  
  # relLik.AIC
  m.test <- bfFixefLMER_F.fnc(m2, method = "relLik.AIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m2, method = "relLik.AIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m4, method = "relLik.AIC",
                              log.file = FALSE)
  
  # relLik.BIC
  m.test <- bfFixefLMER_F.fnc(m2, method = "relLik.BIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m2, method = "relLik.BIC",
                              log.file = FALSE)
  m.test <- bfFixefLMER_t.fnc(m4, method = "relLik.BIC",
                              log.file = FALSE)
}