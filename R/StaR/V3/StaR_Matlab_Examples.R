d1 = 1:2    # Variable 1
d2 = 1:3   # Variable 2
d3 = 1:4   # Variable 3
d4 = sin(1:50) + runif(50, -0.2, 0.2)   # Signal or Value. (e.g ERP - 1536 points)

a <- lapply(d1, FUN= function(x) x <- lapply(d2, FUN= function(x) x <- lapply(d3, FUN= function(x) x <- d4)))

b <- sapply(a, FUN=unlist)
c <- unlist(as.list(b))

writeMat("R_SinTest.mat", cData=c)