### http://www.sthda.com/english/wiki/ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page-r-software-and-data-visualization

###########################################################################
###########################       Utils !     #############################
###########################################################################
getRects <- function(y)
{
  dat <- data.frame(y=y)
  cum_sum <- cumsum((dat$y))
  diff_diff <- diff(diff(cum_sum)) # change of the slope
  start <- which(diff_diff== -1)+1 # negative inflection point's
  #if (is.na(dat$y[1])) start <- c(1,start)
  end <- which(diff_diff == 1)+1 # positive inflection point's
  #if (is.na(tail(dat$y,1))) end <- c(end, length(dat$y))
  
  if(dat$y[1] == 0) {start <- c(1, start)}
  
  # Fix if it never ends.
  if(length(end) != length(start)) {end <- c(end, length(dat$y))}
  
  data.frame(xminimums = start, xmaximums = end)
}

###########################################################################
#########################       Plot Data !     ###########################
###########################################################################
plotData_ERP <- function(data, params, timeVals, titles = NULL, bShowPlot = TRUE)
{
  print("plotData() : ...")
  tic()
  
  hDataList <- NULL
  
  if(length(data) > 0)
  {
    #if(length(data) != length(params)) {
    #  print("======= ERROR in plotData() : length(data) != length(params) ========")
    #  } else {
      hDataList <- list()
      for(i in 1:length(data))
      { 
    #    if(length(data[[i]]) != length(params[[i]])) {
    #      print("======= ERROR in plotData() : length(data[[i]]) != length(params[[i]]) ========")
    #    }
        
        hDataList[[i]] <- list()
        for(j in 1:length(data[[i]]))
        {
          hDataList[[i]][[j]] <- list()
          for(k in 1:length(data[[i]][[j]]))
          {
            #hData[[i]][[j]] <- ggplot() + geom_line(aes(y=means, x=times, colour = "sin"), data = params[[i]][[j]]) + theme(legend.position="none")#axis.line=element_blank(),axis.text.x=element_blank(), 
            hDataList[[i]][[j]][[k]] <- ggplot() + geom_line(aes(y=means, x=times, colour = "sin"), data = params[[i]][[j]][[k]]) + 
            theme(legend.position="none", axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
            geom_vline(xintercept = 0, colour="black", linetype = "longdash") + ylim(-0.5, 0.7) + geom_ribbon(aes(x=times, ymin=means-sdes, ymax=means+sdes), alpha = 0.3, fill="orange", data=params[[i]][[j]][[k]])
            #ggtitle(titles[[i]][[j]][[k]], gp=gpar(fontsize=10,font=3))
            #geom_text(aes(1250, 0.65, label = paste(titles[[i]][[j]][[k]])))
            #annotate(geom="text", x = 1250, y = 0.65, label=titles[[i]][[j]][[k]], color="black", size=rel(3.0))
            #scale_x_continuous(limits = c(-500, 2000)) #coord_cartesian(xlim = c(2000, -500)) 
            
            # Empty Plot !
            #           theme(axis.line=element_blank(),axis.text.x=element_blank(),
            #                 axis.text.y=element_blank(),axis.ticks=element_blank(),
            #                 axis.title.x=element_blank(),
            #                 axis.title.y=element_blank(),legend.position="none",
            #                 panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            #                 panel.grid.minor=element_blank(),plot.background=element_blank())
          }      
        }
      }
    #}
  }
    
  toc()
  print("Done!")
  
  hDataList
}

plotData_ERSP <- function(data, params, timeVals, dims = c(400,135), bShowPlot = TRUE)
{
  print("plotData_ERSP() : ...")
  tic()
  
  hDataList <- NULL
  
  if(length(data) != length(params)) {print("======= ERROR in plotData_ERSP() : length(data) != length(params) ========")}
  else
  {
    for(i in 1:length(data))
    { if(length(data[[i]]) != length(params[[i]])) {print("======= ERROR in plotData_ERSP() : length(data[[i]]) != length(params[[i]]) ========")} }
    
    hDataList <- list()
    for(i in 1:length(data))
    {
      hDataList[[i]] <- list()
      for(j in 1:length(data[[i]]))
      {
        hDataList[[i]][[j]] <- list()
        for(k in 1:length(data[[i]][[j]]))
        {
          #hData[[i]][[j]] <- ggplot() + geom_line(aes(y=means, x=times, colour = "sin"), data = params[[i]][[j]]) + theme(legend.position="none")#axis.line=element_blank(),axis.text.x=element_blank(), 
          #hDataList[[i]][[j]] <- ggplot() + geom_line(aes(y=means, x=times, colour = "sin"), data = params[[i]][[j]]) + 
          #  theme(legend.position="none", axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
          #  geom_vline(xintercept = 0, colour="black", linetype = "longdash") + coord_equal()#ylim(-0.5, 0.6)
          
          #hData <- levelplot(t(matrix(unlist(paramsList[[2]][[1]]$means), nrow = 135, ncol = 400)))
          #hData <- ggplot(melt(tmat), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
          tmat <- t(matrix(unlist(params[[i]][[j]][[k]]$means), nrow = dims[[2]], ncol = dims[[1]]))
          tmat2 <- tmat + 5 
          gg <- ggplot(melt(tmat2), aes(x=Var1, y=Var2, fill=value))
          gg <- gg + geom_raster()
          #gg <- gg + coord_equal()
          #gg <- gg + scale_fill_gradient(low="red", high="blue", trans = 'log')
          #gg <- gg + scale_fill_gradientn(limits = c(-3,3), colours = c("blue","green","red")) #low="red", high="yellow")
          gg <- gg + scale_fill_gradientn(colours =c("blue", "green", "red"), trans = "log")
          #gg <- gg + scale_fill_gradient() 
          #gg <- gg + scale_colour_gradient(trans = "log")
          gg <- gg + scale_x_continuous(expand = c(0, 0))
          gg <- gg + scale_y_continuous(expand = c(0, 0))
          #gg <- gg + theme_bw()
          gg <- gg + theme(legend.position="none", axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),panel.grid.major=element_blank(), panel.grid.minor=element_blank())
          
          hDataList[[i]][[j]][[k]] <- gg
          
          #scale_x_continuous(limits = c(-500, 2000)) #coord_cartesian(xlim = c(2000, -500)) 
          
          # Empty Plot !
          #           theme(axis.line=element_blank(),axis.text.x=element_blank(),
          #                 axis.text.y=element_blank(),axis.ticks=element_blank(),
          #                 axis.title.x=element_blank(),
          #                 axis.title.y=element_blank(),legend.position="none",
          #                 panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          #                 panel.grid.minor=element_blank(),plot.background=element_blank())
      
        }
      }
    }
    
    #hData1 <- cbind(ggplotGrob(hData[[1]]), ggplotGrob(hData[[2]]), ggplotGrob(hData[[3]]), size = "last")
    #hData2 <- cbind(ggplotGrob(hData[[5]]), ggplotGrob(hData[[6]]), ggplotGrob(hData[[7]]), ggplotGrob(hData[[8]]), size = "last")
    #hData3 <- cbind(ggplotGrob(hData[[9]]), ggplotGrob(hData[[10]]), ggplotGrob(hData[[11]]), ggplotGrob(hData[[12]]), size = "last")
    
    #if(bShowPlot == TRUE) {grid.arrange(hDataList)}
  }
  
  toc()
  print("Done!")
  
  hDataList
}

###########################################################################
#########################       Plot Stats !     ###########################
###########################################################################
plotStats_ERP <- function(pVals, timeVals, titles = NULL, bShowPlot = TRUE)
{
  print("Plotting Stats...")
  tic()  
  
  if(is.null(timeVals)) {timeVals = seq(1, 1536)}
  if(is.null(titles)){titles = lapply(1:3, FUN = function(x) {rep("N/A", 3)})}
  
  hPlot <- list()
  hLayers <- list()
  if(length(pVals) > 0)
  {
    for(i in 1:length(pVals))
    { 
      hPlot[[i]] <- list()
      hLayers[[i]] <- list()
      if(length(pVals[[i]]) > 0)
      {
        for(j in 1:length(pVals[[i]]))
        {
          hPlot[[i]][[j]] <- list()
          hLayers[[i]][[j]] <- list()
          for(k in 1:length(pVals[[i]][[j]]))
          {
            yVal <- unlist(pVals[[i]][[j]][[k]])
            
            hResults <- plotStats_ERP_Graph(yVal, titles[[i]][[j]][[k]], timeVals)
            
            hLayers[[i]][[j]][[k]] <- hResults[[1]]
            hPlot[[i]][[j]][[k]] <- hResults[[2]]
          }
        }
      }
    }
  }
  
  #if(bShowPlot == TRUE) {grid.arrange(hPlot)}
  
  toc()
  print("Done!")
  
  PlotRes <- list(hPlot, hLayers)
  PlotRes
}

plotStats_ERP_Graph <- function(yVal, yTitle, xTimeVals)
{
  print(paste("ERP_Graph -", yTitle))
  
  pMasks <- getSignifMasks(yVal)
  
  # From pMasks Indexes to real value indices (e.g. [-1000, 2000] vs [1, 1536])
  if(!is.null(pMasks$xminimums) && !is.null(pMasks$xmaximums))
  {
    pMasks$xminimums <- xTimeVals[pMasks$xminimums]
    pMasks$xmaximums <- xTimeVals[pMasks$xmaximums]
  }
  else
  {
    print("... pMasks - NULL ... ")
  }
  
  # Keep that dataframe... seems to have a bug with nested loops!
  curDF <- data.frame(yVals = yVal, xVals = xTimeVals)
  
  hLayer <- geom_line(aes(y = yVals, x = xVals, colour = "sin"), data = curDF)
  hPlot <- ggplot() + hLayer + theme(legend.position = "none", plot.title = element_text(lineheight=.8, face="bold"), axis.title.x=element_blank(), axis.title.y=element_blank()) + ggtitle(yTitle)
  
  # If at least 1 mask, paint it. (to avoid crash...)
  if(length(pMasks$xminimums) > 0)
  {
    hLayer <- geom_rect(data = pMasks, alpha = 0.15, aes(xmin = xminimums, xmax = xmaximums, ymin = -Inf, ymax = Inf), fill = "blue")
    hPlot <- hPlot + hLayer
  }
  
  list(hLayer, hPlot)
}


plotStats_ERSP <- function(pVals, pTitles, xTimeVals, yFreqVals, dims = c(400,135), bShowPlot = TRUE)
{
  print("Plotting ERSP Stats...")
  tic()  
  
  if(is.null(xTimeVals)) {xTimeVals = seq(1, 400)}
  if(is.null(yFreqVals)) {yFreqVals = seq(1, 135)}
  
  hPlot <- list()
  for(curDim in 1:length(pVals))
  { 
    hPlot[[curDim]] <- list()
    for(i in 1:length(pVals[[curDim]]))
    {
      hPlot[[curDim]][[i]] <- list()
      for(j in 1:length(pVals[[curDim]][[i]]))
      {
        yVal <- unlist(pVals[[curDim]][[i]][[j]])
        if(!is.null(yVal))
        {
          #       pMasks <- getSignifMasks(yVal)
          #       
          #       # From pMasks Indexes to real value indices (e.g. [-1000, 2000] vs [1, 1536])
          #       if(!is.null(pMasks$xminimums) && !is.null(pMasks$xmaximums))
          #       {
          #         pMasks$xminimums <- timeVals[pMasks$xminimums]
          #         pMasks$xmaximums <- timeVals[pMasks$xmaximums]
          #       }
          #       else
          #       {
          #         print("... pMasks - NULL ... ")
          #       }
          
          print(i)
          print(j)
          tmat <- t(matrix(yVal, nrow = dims[[2]], ncol = dims[[1]]))
          gg <- ggplot(melt(tmat), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
          gg <- gg + geom_raster()
          #gg <- gg + coord_equal()
          gg <- gg + scale_fill_gradient(low="red", high="blue")
          #gg <- gg + scale_fill_gradientn(limits = c(-3,3), colours = c("blue","green","red")) #low="red", high="yellow")
          #gg <- gg + scale_fill_gradientn(colours =c("blue", "red"))#, trans = "log")
          #gg <- gg + scale_colour_gradient(trans = "log")
          #gg <- gg + scale_x_continuous(expand = c(0, 0))
          #gg <- gg + scale_y_continuous(expand = c(0, 0))
          gg <- gg + theme_bw()
          gg <- gg + theme(legend.position="none", plot.title = element_text(lineheight=.8, face="bold"), axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + ggtitle(pTitles[[curDim]][[i]][[1]])
          
          
          hPlot[[curDim]][[i]][[j]] <- gg
          #hPlot[[i]][[j]] <- ggplot() + geom_line(aes(y = yVal, x = timeVals, colour = "sin")) + theme(legend.position = "none") + geom_rect(data =  pMasks, alpha = 0.3, aes(xmin = xminimums, xmax = xmaximums, ymin = -Inf, ymax = Inf), fill = "blue")
        }
      }
    }
  }
  
  #if(bShowPlot == TRUE) {grid.arrange(hPlot)}
  
  toc()
  print("Done!")
  
  hPlot
}

plotStats_ERSP_Graph <- function(pVals, pTitle, xTimeVals, yFreqVals, dims = c(400,135), bShowPlot = TRUE)
{
  print("Plotting ERSP Stats Graph...")

  if(is.null(xTimeVals)) {xTimeVals = seq(1, 400)}
  if(is.null(yFreqVals)) {yFreqVals = seq(1, 135)}
  
  if(!is.null(pVals))
  {
        #       pMasks <- getSignifMasks(yVal)
        #       
        #       # From pMasks Indexes to real value indices (e.g. [-1000, 2000] vs [1, 1536])
        #       if(!is.null(pMasks$xminimums) && !is.null(pMasks$xmaximums))
        #       {
        #         pMasks$xminimums <- timeVals[pMasks$xminimums]
        #         pMasks$xmaximums <- timeVals[pMasks$xmaximums]
        #       }
        #       else
        #       {
        #         print("... pMasks - NULL ... ")
        #       }
        
    tmat <- t(matrix(pVals, nrow = dims[[2]], ncol = dims[[1]]))
    gg <- ggplot(melt(tmat), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
    gg <- gg + geom_raster()
        #gg <- gg + coord_equal()
    gg <- gg + scale_fill_gradient(low="red", high="blue")
        #gg <- gg + scale_fill_gradientn(limits = c(-3,3), colours = c("blue","green","red")) #low="red", high="yellow")
        #gg <- gg + scale_fill_gradientn(colours =c("blue", "red"))#, trans = "log")
        #gg <- gg + scale_colour_gradient(trans = "log")
        #gg <- gg + scale_x_continuous(expand = c(0, 0))
        #gg <- gg + scale_y_continuous(expand = c(0, 0))
    gg <- gg + theme_bw()
    gg <- gg + theme(legend.position="none", plot.title = element_text(lineheight=.8, face="bold"), axis.line=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + ggtitle(pTitle)
        
        #hPlot[[i]][[j]] <- gg
        #hPlot[[i]][[j]] <- ggplot() + geom_line(aes(y = yVal, x = timeVals, colour = "sin")) + theme(legend.position = "none") + geom_rect(data =  pMasks, alpha = 0.3, aes(xmin = xminimums, xmax = xmaximums, ymin = -Inf, ymax = Inf), fill = "blue")
  }
      #}

  #if(bShowPlot == TRUE) {grid.arrange(hPlot)}
  
  return(gg)
}

getAnalysisTitles <- function(dirPlots, iDesign, iDomain, statsFunction, statsCorrectionOn, statsCorrectionFun, statsThreshold, bSaveOnDiskImages)
{
  # ------------------------------------------------------------------------
  # ORIGINAL
  # ------------------------------------------------------------------------
  #           hAnalysisTitles <- list()
  #           hAnalysisTitles[[1]] <- textGrob(staR_getDesignName(iDesign, stats.function), gp=gpar(fontsize=20))
  #           hAnalysisTitles[[2]] <- textGrob(paste("Domain: #", data.domain), gp=gpar(fontsize=20))
  #           if(stats.bCorrection){
  #             hAnalysisTitles[[3]] <- textGrob(paste("Corrected with:", stats.correctionFunction), gp=gpar(fontsize=20))
  #           } else {
  #             hAnalysisTitles[[3]] <- textGrob("Not Corrected!", gp=gpar(fontsize=20))
  #           }
  #           hAnalysisTitles[[4]] <- textGrob(paste("Threshold (pValue):", sigthreshold), gp=gpar(fontsize=20))
  #           grid.arrange(hAnalysisTitles[[1]], hAnalysisTitles[[2]], hAnalysisTitles[[3]], hAnalysisTitles[[4]], nrow = 4, ncol = 1)
  #           if(bSaveOnDiskImages)
  #           {
  #             dev.copy2pdf(file = paste(dirPlots, "/Title_", iDesign, ".pdf", sep = ""))
  #             dev.off()
  #             Sys.sleep(5)
  #           }
  # ------------------------------------------------------------------------
  
  hAnalysisTitles <- list()
  hAnalysisTitles[[1]] <- textGrob(staR_getDesignName(iDesign, statsFunction), gp=gpar(fontsize=20))
  hAnalysisTitles[[2]] <- textGrob(paste("Domain: #", iDomain), gp=gpar(fontsize=20))
  
  if(statsCorrectionOn){
    hAnalysisTitles[[3]] <- textGrob(paste("Corrected with:", statsCorrectionFun), gp=gpar(fontsize=20))
  } else {
    hAnalysisTitles[[3]] <- textGrob("Not Corrected!", gp=gpar(fontsize=20))
  }
  hAnalysisTitles[[4]] <- textGrob(paste("Threshold (pValue):", statsThreshold), gp=gpar(fontsize=20))
  grid.arrange(hAnalysisTitles[[1]], hAnalysisTitles[[2]], hAnalysisTitles[[3]], hAnalysisTitles[[4]], nrow = 4, ncol = 1)
  
  if(bSaveOnDiskImages)
  {
    dev.copy2pdf(file = paste(dirPlots, "/Title_", iDesign, ".pdf", sep = ""))
    dev.off()
    Sys.sleep(5)
  }
  
  return (hAnalysisTitles)
}

doRawPlots <- function(bFullStats, fullPVals, fullPTitles, bSubStats, subPVals, subTitles, sigThreshold, bSaveOnDiskImages, iDesign, dirPlots)
{
  ###  --- Full Analysis ---
  if(bFullStats)
  {
    for(i in 1:length(fullPVals))
    {
      plot(unlist(fullPVals[[i]]), type="l", log="y", ylim = c(0.001, 1))
      title(main = fullPTitles[[i]])
      abline(h = sigThreshold)
      
      if(bSaveOnDiskImages)
      {
        dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
        dev.off()
        Sys.sleep(8)
      }
    }
  }
  
  if(bSubStats)
  {
    for(curDim in 1:length(subPVals))
    {
      for(i in 1:length(subPVals[[curDim]]))
      {
        for(j in 1:length(subPVals[[curDim]][[i]]))
        {
          plot(unlist(subPVals[[curDim]][[i]][[j]]), type="l", log="y", ylim = c(0.001, 1))
          title(main = subTitles[[curDim]][[i]][[j]])
          abline(h = sigThreshold)
          
          if(bSaveOnDiskImages)
          {
            dev.copy2pdf(file = paste(dirPlots, "/PVals_D", iDesign, "_", i, ".pdf", sep = ""))
            dev.off()
            Sys.sleep(8)
          }
        }
      }
    }
  }
}


###########################################################################
####################       Get Significant Masks !     ####################
###########################################################################
getSignifMasks <- function(pVals, bSignif = TRUE)
{
  sigRect<- getRects(unlist(pVals))
  
  sigRect
}


# ###########################################################################
# ########################       Plot Stats !     ###########################
# ###########################################################################
# hpValsA <- list()
# hpValsA[[1]] <- list()
# hpValsA[[2]] <- list()
# hpValsA[[3]] <- list()
# hpValsA[[1]][[1]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[1]][[1]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[1]][[1]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[1]][[2]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[1]][[2]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[1]][[2]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[2]][[1]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[2]][[1]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[2]][[1]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[2]][[2]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[2]][[2]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[2]][[2]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[3]][[1]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[3]][[1]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[3]][[1]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[3]][[2]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[3]][[2]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[3]][[2]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# 
# hpValsA[[4]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[4]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[4]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[5]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[5]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[5]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[6]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[6]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.pvals.sigrect[[6]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# hpValsA[[7]] <- ggplot() + geom_line(aes(y=unlist(anovas.pSignif[[7]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") #+ geom_rect(data = anovas.pvals.sigrect[[7]], alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "blue")
# 
# hpValsA.full <- list()
# hpValsA.full[[1]] <- ggplot() + geom_line(aes(y=unlist(anovas.full.pSignif[[1]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.full.pvals.sigrect, alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "orange")
# hpValsA.full[[2]] <- ggplot() + geom_line(aes(y=unlist(anovas.full.pSignif[[1]]), x=seq(1,1536), colour = "sin")) + theme(legend.position = "none") + geom_rect(data = anovas.full.pvals.sigrect, alpha = 0.3, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "orange")
# 
# hpValsAV <- cbind(ggplotGrob(hpValsA[[4]]),ggplotGrob(hpValsA[[5]]),ggplotGrob(hpValsA[[6]]),ggplotGrob(hpValsA[[7]]), size = "last")
# 
# #hpValsAN <- rbind(ggplotGrob(hpValsA[[1]]), ggplotGrob(hpValsA[[2]]), size = "last")
# #grid.arrange(hpValsAN)
# 
# r1 <- cbind(hData1, ggplotGrob(hpValsA[[1]][[1]]), ggplotGrob(hpValsA[[1]][[2]]), size = "last")
# r2 <- cbind(hData2, ggplotGrob(hpValsA[[2]][[1]]), ggplotGrob(hpValsA[[2]][[2]]), size = "last")
# r3 <- cbind(hData3, ggplotGrob(hpValsA[[3]][[1]]), ggplotGrob(hpValsA[[3]][[2]]), size = "last")
# r4 <- cbind(hpValsAV, ggplotGrob(hpValsA.full[[1]]), ggplotGrob(hpValsA.full[[2]]), size = "last")
# #============================
# #== Master Plot !
# #============================
# grid.arrange(r1, r2, r3, r4, nrow = 4, ncol = 1)

