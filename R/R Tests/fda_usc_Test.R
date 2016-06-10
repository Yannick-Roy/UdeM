# Test of fda.usc
# Derivates of signals!

library("fda.usc")
data("tecator")
names(tecator)
absorp <- tecator$absorp.fdata

Fat20 <- ifelse(tecator$y$Fat < 20, 0, 1) * 2 + 2
plot(tecator$absorp.fdata, col = Fat20)

absorp.d1 <- fdata.deriv(absorp, nderiv = 1)
plot(absorp.d1, col = Fat20)