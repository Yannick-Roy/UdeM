#================================================================================================================================
###################################  ==================== ANOVAS  ==================== ##########################################
#================================================================================================================================
STATS_DESIGNS <- list()

# Comment pour dire que j'ai un bug avec git!

# Normal Designs
STATS_DESIGNS[[1]] = values ~ (conditions)
STATS_DESIGNS[[2]] = values ~ (sessions)
STATS_DESIGNS[[3]] = values ~ (motions)
STATS_DESIGNS[[4]] = values ~ (orders)
STATS_DESIGNS[[5]] = values ~ (groups)

STATS_DESIGNS[[11]] = values ~ (groups * conditions)
STATS_DESIGNS[[12]] = values ~ (groups * sessions)
STATS_DESIGNS[[13]] = values ~ (groups * motions)
STATS_DESIGNS[[14]] = values ~ (groups * orders)
STATS_DESIGNS[[15]] = values ~ (sessions * motions)
STATS_DESIGNS[[16]] = values ~ (sessions * orders)
STATS_DESIGNS[[17]] = values ~ (groups * sessions * motions)
STATS_DESIGNS[[18]] = values ~ (groups * sessions * orders)
STATS_DESIGNS[[19]] = values ~ (groups * sessions * motions * orders)

# Repeated Measures.
STATS_DESIGNS[[21]] = values ~ (conditions) + Error(subjects/(conditions))
STATS_DESIGNS[[22]] = values ~ (sessions) + Error(subjects/(sessions))
STATS_DESIGNS[[23]] = values ~ (motions) + Error(subjects/(motions))
STATS_DESIGNS[[24]] = values ~ (orders) + Error(subjects/(orders))
STATS_DESIGNS[[25]] = values ~ (groups) + Error(subjects/(groups))

#STATS_DESIGNS[[31]] = values ~ (groups * conditions) + Error(sessions/(groups * conditions))
STATS_DESIGNS[[31]] = values ~ (groups * conditions) + Error(subjects/(groups * conditions))
STATS_DESIGNS[[32]] = values ~ (groups * sessions) + Error(subjects/(groups * sessions))
STATS_DESIGNS[[33]] = values ~ (groups * motions) + Error(subjects/(groups * motions))
STATS_DESIGNS[[34]] = values ~ (groups * orders) + Error(subjects/(groups * orders))
STATS_DESIGNS[[35]] = values ~ (sessions * motions) + Error(subjects/(sessions * motions))
STATS_DESIGNS[[36]] = values ~ (sessions * orders) + Error(subjects/(sessions * orders))
STATS_DESIGNS[[37]] = values ~ (groups * sessions * motions) + Error(subjects/(groups * sessions * motions))
STATS_DESIGNS[[38]] = values ~ (groups * sessions * orders) + Error(subjects/(groups * sessions * orders))
STATS_DESIGNS[[39]] = values ~ (groups * sessions * motions * orders) + Error(subjects/(groups * sessions * motions * orders))
STATS_DESIGNS[[40]] = values ~ (groups * sessions * conditions) + Error(subjects/(groups * sessions * conditions))
#STATS_DESIGNS[[39]] = values ~ (groups * sessions * motions * orders) + Error(subjects/(groups * sessions * motions * orders))

STATS_SUB_DESIGNS <- list()
STATS_SUB_DESIGNS[[11]] = list(values ~ (conditions), values ~ (groups))
STATS_SUB_DESIGNS[[12]] = list(values ~ (sessions), values ~ (groups))
STATS_SUB_DESIGNS[[13]] = list(values ~ (motions), values ~ (groups))
STATS_SUB_DESIGNS[[14]] = list(values ~ (orders), values ~ (groups))
STATS_SUB_DESIGNS[[15]] = list(values ~ (sessions), values ~ (motions))
STATS_SUB_DESIGNS[[16]] = list(values ~ (sessions), values ~ (orders))
STATS_SUB_DESIGNS[[17]] = list(values ~ (sessions), values ~ (motions), values ~ (groups))
STATS_SUB_DESIGNS[[18]] = list(values ~ (sessions), values ~ (orders), values ~ (groups))

STATS_SUB_DESIGNS[[31]] = list(values ~ (conditions) + Error(subjects/conditions), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[32]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[33]] = list(values ~ (motions) + Error(subjects/motions), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[34]] = list(values ~ (orders) + Error(subjects/orders), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[35]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (motions) + Error(subjects/motions))
STATS_SUB_DESIGNS[[36]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (orders) + Error(subjects/orders))
STATS_SUB_DESIGNS[[37]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (motions) + Error(subjects/motions), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[38]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (orders) + Error(subjects/orders), values ~ (groups) + Error(subjects/groups))
STATS_SUB_DESIGNS[[40]] = list(values ~ (sessions) + Error(subjects/sessions), values ~ (conditions) + Error(subjects/conditions), values ~ (groups) + Error(subjects/groups))


#STATS_SUB_DESIGNS_ez[[11]] = ezANOVA(data = x, dv = values, wid = subjects, within = .(subjects), between = .(groups, conditions)) #, within = .(sessions)
#================================================================================================================================
################################  ==================== MIXED MODELS  ==================== #######################################
#================================================================================================================================
STATS_DESIGNS_MM <- list()
STATS_DESIGNS_MM[[1]] = values ~ (conditions) + (1|subjects)
STATS_DESIGNS_MM[[2]] = values ~ (sessions) + (1|subjects)
STATS_DESIGNS_MM[[3]] = values ~ (motions) + (1|subjects)
STATS_DESIGNS_MM[[4]] = values ~ (orders) + (1|subjects)
STATS_DESIGNS_MM[[5]] = values ~ (groups) + (1|subjects)

STATS_DESIGNS_MM[[11]] = values ~ (groups * conditions) + (1|subjects)
STATS_DESIGNS_MM[[12]] = values ~ (groups * sessions) + (1|subjects)
STATS_DESIGNS_MM[[13]] = values ~ (groups * motions) + (1|subjects)
STATS_DESIGNS_MM[[14]] = values ~ (groups * orders)  + (1|subjects)
STATS_DESIGNS_MM[[15]] = values ~ (sessions * motions) + (1|subjects)
STATS_DESIGNS_MM[[16]] = values ~ (sessions * orders) + (1|subjects)
STATS_DESIGNS_MM[[17]] = values ~ (groups * sessions * motions) + (1|subjects)
STATS_DESIGNS_MM[[18]] = values ~ (groups * sessions * orders) + (1|subjects)
STATS_DESIGNS_MM[[19]] = values ~ (groups * sessions * motions * orders) + (1|subjects)

STATS_DESIGNS_MM[[21]] = values ~ (conditions) + (1|subjects)
STATS_DESIGNS_MM[[22]] = values ~ (sessions) + (1|subjects)
STATS_DESIGNS_MM[[23]] = values ~ (motions) + (1|subjects)
STATS_DESIGNS_MM[[24]] = values ~ (orders) + (1|subjects)
STATS_DESIGNS_MM[[25]] = values ~ (groups) + (1|subjects)

STATS_DESIGNS_MM[[31]] = values ~ (groups * conditions) + (1|subjects)
STATS_DESIGNS_MM[[32]] = values ~ (groups * sessions) + (1|subjects)
STATS_DESIGNS_MM[[33]] = values ~ (groups * motions) + (1|subjects)
STATS_DESIGNS_MM[[34]] = values ~ (groups * orders) + (1|subjects)
STATS_DESIGNS_MM[[35]] = values ~ (sessions * motions) + (1|subjects)
STATS_DESIGNS_MM[[36]] = values ~ (sessions * orders) + (1|subjects)
STATS_DESIGNS_MM[[37]] = values ~ (groups * sessions * motions) + (1|subjects)
STATS_DESIGNS_MM[[38]] = values ~ (groups * sessions * orders) + (1|subjects)
STATS_DESIGNS_MM[[39]] = values ~ (groups * sessions * motions * orders) + (1|subjects)

STATS_SUB_DESIGNS_MM <- list()
STATS_SUB_DESIGNS_MM[[1]] = list(values ~ (conditions))
#STATS_SUB_DESIGNS_MM[[11]] = list(values ~ (conditions) + (1|subjects), values ~ (groups) + (1|subjects))
STATS_SUB_DESIGNS_MM[[11]] = list(values ~ (conditions) + (1|sessions), values ~ (groups) + (1|sessions))
STATS_SUB_DESIGNS_MM[[12]] = list(values ~ (sessions) + (1|subjects), values ~ (groups) + (1|subjects))
STATS_SUB_DESIGNS_MM[[13]] = list(values ~ (motions) + (1|subjects), values ~ (groups) + (1|subjects))
STATS_SUB_DESIGNS_MM[[14]] = list(values ~ (orders) + (1|subjects), values ~ (groups) + (1|subjects))
STATS_SUB_DESIGNS_MM[[15]] = list(values ~ (motions) + (1|subjects), values ~ (sessions) + (1|subjects))
STATS_SUB_DESIGNS_MM[[16]] = list(values ~ (orders) + (1|subjects), values ~ (sessions) + (1|subjects))
#STATS_SUB_DESIGNS_MM[[16]] = list(values ~ (sessions) + (1|subjects), values ~ (orders) + (1|subjects))

STATS_DESIGNS_MM_RESTRICTED = values ~ 1 * (1 | subjects)
STATS_DESIGNS_MM[[99]] = values ~ 1 * (1 | subjects) #Bug...

#STATS_DESIGNS_RND = ~1|sessions
STATS_DESIGNS_RND = ~1|subjects

#==================================================================

staR_getDesignName <- function(iDesign, statsFunction)
{
  title = "N/A"
  
  if(statsFunction == "aov")
  {
    title <- paste("ANOVA (aov) :", "\n ", format(STATS_DESIGNS[[iDesign + 20]]))
  }
  
  if(statsFunction == "lme")
  {
    #if(MMfunc == "lmer") {title <- paste("Mixed Models (", MMfunc, ") : ", format(STATS_DESIGNS_MM[[iDesign]]))}
    title <- paste("Mixed Models (lme) :", "\n ", format(STATS_DESIGNS[[iDesign]]), "\n ", format(STATS_DESIGNS_RND))
  }
  
  subTitle <- gsub("[+]", "\n +", title)
  subTitle
}

staR_getDesignMatrix <- function(iDesign)
{
  # Same design with / without random.
  if(iDesign > 20) {iDesign <- iDesign - 20}
  
  if(iDesign == 1) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 4)} # Conditions
  if(iDesign == 2) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 3)} # Sessions
  if(iDesign == 3) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 2)} # Motions
  if(iDesign == 4) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 2)} # Orders
  if(iDesign == 5) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 2)} # Groups
  
  if(iDesign == 11) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 4)} # Groups * Conditions
  if(iDesign == 12) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 3)} # Groups * Sessions
  if(iDesign == 13) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 2)} # Groups * Motions
  if(iDesign == 14) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 2)} # Groups * Orders
  if(iDesign == 15) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 3)} # Sessions * Motions
  if(iDesign == 16) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 2, "nbCol" = 3)} # Sessions * Orders
  
  if(iDesign == 17) {designMatrix = data.frame("nbLayer" = 2, "nbRow" = 2, "nbCol" = 3)} # Groups * Motions * Sessions   //YR: modified the order of the 2 last - please confirm.
  if(iDesign == 18) {designMatrix = data.frame("nbLayer" = 2, "nbRow" = 2, "nbCol" = 3)} # Groups * Orders * Sessions    //YR: modified the order of the 2 last - please confirm.
  if(iDesign == 19) {designMatrix = data.frame("nbLayer" = 1, "nbRow" = 1, "nbCol" = 1)} # Groups * Sessions * Motions * Orders
  
  designMatrix
}
