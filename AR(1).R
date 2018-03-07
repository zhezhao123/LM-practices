library(nlme)
boxdata <- read.table(file.choose(), header=T)
attach(boxdata)
year1975 = year - 1975
m.sl <- lm(GrossBoxOffice~year1975)
summary(m.sl)
plot(m.sl)
st.red = rstandard(m.sl)
par(mfrow=c(2,2))
plot(GrossBoxOffice*year1975, ylab="Gross Box Office",xlab = "year Since 1975")
abline(m.sl)
plot(st.red*year1975, ylab = "Standardized Residuals", xlab = "Year since 1975")
acf(st.red, main="Standardized Residuals lag")
m.gls <- gls(GrossBoxOffice~year1975, correlation = corAR1(form =~year1975), method = "ML")
summary(m.gls)

#transform to LS
rho <- 0.8782065
x <- model.matrix(m.sl)
Sigma <- diag(length(year1975))
Sigma <- rho^abs(row(Sigma)-col(Sigma))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x # matrix multiplication
ystar <- smi %*% GrossBoxOffice
mltls <- lm(ystar~xstar-1) #why-1?
summary(mltls)
plot(mltls)
# model adjust
xstarsq <- (xstar-1)^2
xstarcube <- (xstar-1)^3
transmltls <- lm(ystar~xstar-1 + xstarsq+xstarcube) 
plot(transmltls)
st.redtran <- rstandard(transmltls)
na.omit(st.redtran)
acf(na.omit(st.redtran), main="Standardized Residuals lag")
#model adjust 2
yearsq <- year1975^2
g <- lm(GrossBoxOffice~year1975+yearsq)
plot(g)
st.res.g <- rstandard(g)
acf(st.res.g)
m.gls.sq <- gls(GrossBoxOffice~year1975+yearsq, correlation = corAR1(form =~year1975), method = "ML")
summary(m.gls.sq)
rhosq <- 0.7663632 
sqx <- model.matrix(g)
Sigma.sq <- diag(length(year1975))
Sigma.sq <- rhosq^abs(row(Sigma)-col(Sigma))
sm.sq <- chol(Sigma.sq)
smi.sq <- solve(t(sm.sq))
xsqstar <- smi.sq %*% sqx
ysqstar <- smi.sq %*% GrossBoxOffice
m.sqx <- lm(ysqstar~xsqstar-1)
summary(m.sqx)
sqres <- rstandard(m.sqx)
acf(sqres)
plot(m.sqx)
# predict when year1975 is 33
p = 33
psq = p^2
predict.value <- 67.1148 + 10.269*p + 0.5359*psq
predict.value