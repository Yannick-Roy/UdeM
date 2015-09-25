library(pwr)
# Set variables
p <- seq(.2, .9, .01)
np <- length(p)

d <- seq(.4, 2.0, .2)
nd <- length(d)

samsize <- array(numeric(np*nd), dim=c(np, nd))

for (i in 1:nd) {
  for (j in 1:np) {
    result <- power.t.test(n = NULL, 
                           d = d[i], 
                           sig.level = 0.05, 
                           power = p[j], 
                           alternative = "two.sided", 
                           type = "paired"
    )
    samsize[j, i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(p)
yrange <- round(range(samsize))
colors <- rainbow(length(d))
plot(xrange, yrange, type="n",
     xlab="Power (1 - beta)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:nd){
  lines(p, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend) 
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Binaural Beat t-tests\n
      Sig=0.05 (Two-tailed)")
legend("topleft", title="Effect Size", as.character(d),
       fill=colors)

# Save the plot
dev.copy(png, "/home/yannick/powersamsize.png")
dev.off()