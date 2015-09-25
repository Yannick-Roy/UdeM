#Create Dataset
set.seed(1)
x <- runif(100)
y <- runif(100)
z <- y*x

#Assign colors, based on z vector    
Data <- data.frame(Order=1:length(z),z=z)
Data <- Data[order(Data$z),]
Data$col <- rainbow(length(z))
orderedcolors <- Data[order(Data$Order),'col']

#Plot x vs y, colored by z
plot(x,y,col = rainbow(length(z))[rank(z)])
plot(x,y,col=orderedcolors)


#====================================
library('ggplot2')
library('parallel')
library('data.table')

sizes = c(1000, 5000, 10000, 50000, 1e5, 5e5, 1e6)
testFun <- function(x, size){
  set.seed(1)
  sum(rnorm(size))
}
results = data.frame(size=c(), type=c(), time=c())

cl <- makeCluster(4)

for(size in sizes)
{
  ## parallel computing
  beg = Sys.time()
  z=clusterApply(cl, 1:1000, testFun, size=size)
  td = as.numeric(Sys.time() - beg, "secs")
  results = rbind(results, data.frame(size=size, type="quad_core", time=td))
  
  ## single threaded computing (to compare times and code)
  beg = Sys.time()
  z=lapply(1:1000, testFun, size=size)
  td = as.numeric(Sys.time() - beg, "secs")
  results = rbind(results, data.frame(size=size, type="single_core", time=td))
}

stopCluster(cl)

ggplot(results, aes(x=size, y=time, group=type, color=type))+
  geom_point()+
  scale_x_log10()
dt=data.table(results)

results_ratio = as.data.frame(dt[,list(ratio = time[type == "single_core"]/time[type == "quad_core"]), by=c("size")])
ggplot(results_ratio, aes(x=size, y=ratio, group=1))+
  geom_point()+
  scale_y_continuous(limits=c(0,4))+
  scale_x_log10()