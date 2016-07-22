## We want to put the 1st D (points) in 3th D and keep Layer x Rows x Cols (i x j x k)
staR_InvertDimensions3D <- function(array3D)
{ 
  b <- list()
  if(length(array3D) > 0)
  {
  for(p in 1:length(array3D))
  {
      if(length(array3D[[p]]) > 0)
      {
    for(i in 1:length(array3D[[p]]))
    {
          if(length(array3D[[p]][[i]]) > 0)
          {
      for(j in 1:length(array3D[[p]][[i]]))
      {
        #print(paste(p,i,j))
        if(length(b) < i) {b[[i]] <- list()}
        if(length(b[[i]]) <  j) {b[[i]][[j]] <- list()} 
        
        b[[i]][[j]][[p]] <- array3D[[p]][[i]][[j]]
      }
    }
  }  
      }
    }
  }
  return (b)
}

flip1 <- function()
{
#stats.fullAnalysis.aov.retVal <- staR_aov(fullData, iDesign)
hDataset <- list()
vDataset <- list()
for(p in 1:nbPoints)  # Points
{
  ## -----------------------
  ## -- Horizontal (Rows) --
  ## -----------------------
  hDataset[[p]] <- list()
  if(length(subData) > 0)
  {
    for(i in 1:length(subData)) # Layers
    {
      hDataset[[p]][[i]] <- list()
      if(length(subData[[i]]) > 0)
      {
        for(j in 1:length(subData[[i]])) # Rows
        {
          hDataset[[p]][[i]][[j]] <- list()
          if(length(subData[[i]][[j]]) > 0)
          {
            for(k in 1:length(subData[[i]][[j]])) # Cols
            { 
              #print(paste(p, " (h) - ", i, j, k))
              # -- Horizontal --
              #a[[k]][[i]] <- rbind(subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]], subData[[i]][[j]][[k]])
              if(k == 1) {hDataset[[p]][[i]][[j]] <- subData[[i]][[j]][[k]][[p]]}
              else {hDataset[[p]][[i]][[j]] <- rbind(hDataset[[p]][[i]][[j]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    }
  }
  
  ## -----------------------
  ## -- Vertival (Cols) --
  ## -----------------------
  vDataset[[p]] <- list()
  if(length(subData) > 0)
  {
    for(i in 1:length(subData)) # Layers
    {
      vDataset[[p]][[i]] <- list()
      if(length(subData[[i]]) > 0)
      {
        for(k in 1:length(subData[[i]][[1]])) # Cols
        {
          vDataset[[p]][[i]][[k]] <- list()
          for(j in 1:length(subData[[i]])) # Rows
          {
            #print(paste(p, " (v) - ", i, j, k))
            # -- Vertical --
            if(j == 1) {vDataset[[p]][[i]][[k]] <- subData[[i]][[j]][[k]][[p]]}
            else {vDataset[[p]][[i]][[k]] <- rbind(vDataset[[p]][[i]][[k]], subData[[i]][[j]][[k]][[p]])}
          }
        }
      }
    }
  }
}
}

flip2 <- function()
{
#stats.fullAnalysis.lme.retVal <- staR_lme(fullData, iDesign)
hDataset <- list()
vDataset <- list()
for(p in 1:nbPoints)  # Points
{
  ## -----------------------
  ## -- Horizontal (Rows) --
  ## -----------------------
  hDataset[[p]] <- list()
  if(length(subData) > 0)
  {
    for(i in 1:length(subData)) # Layers
    {
      hDataset[[p]][[i]] <- list()
      if(length(subData[[i]]) > 0)
      {
        for(j in 1:length(subData[[i]])) # Rows
        {      
          hDataset[[p]][[i]][[j]] <- list()
          if(length(subData[[i]][[j]]) > 0)
          {
            for(k in 1:length(subData[[i]][[j]])) # Cols
            {               
              #print(paste(p, " (v) - ", i, j, k))
              # -- Horizontal --
              hDataset[[p]][[i]][[j]][[k]] <- list()
              if(k == 1) {hDataset[[p]][[i]][[j]][[k]] <- subData[[i]][[j]][[k]][[p]]}
              else {hDataset[[p]][[i]][[j]][[k]] <- rbind(hDataset[[p]][[i]][[j]][[k]], subData[[i]][[j]][[k]][[p]])}
            }
          }
        }
      }
    }
  }
  
  ## -----------------------
  ## -- Vertival (Cols) --
  ## -----------------------
  vDataset[[p]] <- list()
  if(length(subData) > 0)
  {
    for(i in 1:length(subData)) # Layers
    {
      vDataset[[p]][[i]] <- list()
      if(length(subData[[i]]) > 0)
      {
        for(k in 1:length(subData[[i]][[1]])) # Cols
        {
          vDataset[[p]][[i]][[k]] <- list()
          for(j in 1:length(subData[[i]])) # Rows
          {
            #print(paste(p, " (v) - ", i, j, k))
            # -- Vertical --
            vDataset[[p]][[i]][[k]][[j]] <- list()
            if(j == 1) {vDataset[[p]][[i]][[k]][[j]] <- subData[[i]][[j]][[k]][[p]]}
            else {vDataset[[p]][[i]][[k]][[j]] <- rbind(vDataset[[p]][[i]][[k]][[j]], subData[[i]][[j]][[k]][[p]])}
          }
        }
      }
    }
  }
}
}

## We want to put the 1st D (points) in 3th D and keep Layer x Rows x Cols (i x j x k)
# staR_InvertDimensions3D <- function(array3D)
# { 
#   b <- list()
#   for(p in 1:length(array3D))
#   {
#     for(i in 1:length(array3D[[p]]))
#     {
#       for(j in 1:length(array3D[[p]][[i]]))
#       {
#         for(k in 1:length(array3D[[p]][[i]][[j]]))
#         {          
#           #print(paste(p,i,j))
#           if(length(b) < i) {b[[i]] <- list()}
#           if(length(b[[i]]) <  j) {b[[i]][[j]] <- list()} 
#           if(length(b[[i]][[j]]) <  k) {b[[i]][[j]][[k]] <- list()} 
#           
#           b[[i]][[j]][[k]][[p]] <- array3D[[p]][[i]][[j]][[k]]
#         }
#       }
#     }
#   }  
#   return (b)
# }