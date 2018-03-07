dataUN <- read.csv(file.choose(), header=T)
attach(dataUN)
loggdp <- log(ppgdp)
m.mlr <- lm(fertility~loggdp + pctUrban)
summary(m.mlr)
predict(m.mlr, interval='confidence')
confint(m.mlr, level = 0.95)
par(mfrow=c(2,2))
plot(m.mlr)
#bootstrap
library(alr3)
n.obs<- dim(dataUN)[1]
sgma.hat <- summary(m.mlr)$sigma #estimated steDev
f.vals <- fitted(m.mlr) # fitted values of m.mlr regression
nt<-120
sim.intercoefs <- matrix(rep(0,2*nt), ncol=2, nrow=nt)
sim.beta1coefs <- matrix(rep(0,2*nt), ncol=2, nrow=nt)
sim.beta2coefs <- matrix(rep(0,2*nt), ncol=2, nrow=nt)
for (i in 1:nt) {
  UN.new <- f.vals + rnorm(n.obs, 0, sgma.hat)
  m.new <- lm(UN.new~loggdp+pctUrban)
  sim.intercoefs[i,] <- summary(m.new)$coef[1, c(1,2)]
  sim.beta1coefs[i,] <- summary(m.new)$coef[2, c(1,2)]
  sim.beta2coefs[i,] <- summary(m.new)$coef[3, c(1,2)]
}

#intervals
# for beat0
z.val <- qnorm(0.975, 0, 1)
lower.bnds0 <- sim.intercoefs[,1] - z.val*sim.intercoefs[,2]
upper.bnds0 <- sim.intercoefs[,1] + z.val*sim.intercoefs[,2]
#for beta1
lower.bnds1 <- sim.beta1coefs[,1] - z.val*sim.beta1coefs[,2]
upper.bnds1 <- sim.beta1coefs[,1] + z.val*sim.beta1coefs[,2]
#for beta2
lower.bnds2 <- sim.beta2coefs[,1] - z.val*sim.beta2coefs[,2]
upper.bnds2 <- sim.beta2coefs[,1] + z.val*sim.beta2coefs[,2]