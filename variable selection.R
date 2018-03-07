data1 <- read.csv(file.choose(), header=T)
attach(data1)
#get trainnnig sample
training <- data1[sample(nrow(data1), 98),]
#get test sample
test <- data1[sample(nrow(data1), 98),]
detach(data1)
#select best subsets
attach(training)
lprize = log(PrizeMoney)
X <- cbind(DrivingAccuracy, GIR, PuttingAverage, BirdieConversion, SandSaves, Scrambling, PuttsPerRound)
b <- regsubsets(as.matrix(X), lprize, data = training)
rs <-summary(b)
par(mfrow=c(1,1))
plot(1:7, rs$adjr2, xlab = "subset size", ylab="Rsquared Adjusted")
library(car)
subsets(b, las = 0, legend="interactive", statistic=c("adjr2"))
#fit best subsets
m1 <- lm(lprize~PuttingAverage, data = training)
m2 <- lm(lprize~GIR+PuttsPerRound, data = training)
m3 <- lm(lprize~GIR+BirdieConversion+PuttsPerRound, data = training)
m4 <- lm(lprize~GIR+BirdieConversion+SandSaves+PuttsPerRound, data = training)
m5 <- lm(lprize~GIR+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, data = training)
m6 <- lm(lprize~DrivingAccuracy+GIR+BirdieConversion+SandSaves+Scrambling+PuttsPerRound, data = training)
m7 <- lm(lprize~DrivingAccuracy+GIR+BirdieConversion+SandSaves+Scrambling+PuttsPerRound+PuttingAverage, data = training)
#subset1, size == 1
n1 <- length(m1$residuals)
npar1 <- length(m1$coefficients) + 1
AIC1 <- extractAIC(m1, k =2) #AIC
AICc1 <- extractAIC(m1,k=2)+2*npar1*(npar1+1)/(n1-npar1-1) #AICc
BIC1 <- extractAIC(m1,k=log(n1))
Radj1 <- summary(m1)$adj.r.squared
#subset2, size == 2
n2 <- length(m2$residuals)
npar2 <- length(m2$coefficients) + 1
AIC2 <- extractAIC(m2, k =2) #AIC
AICc2 <- extractAIC(m2,k=2)+2*npar2*(npar2+1)/(n2-npar2-1) #AICc
BIC2 <- extractAIC(m2,k=log(n2))
Radj2 <- summary(m2)$adj.r.squared
#subset3, size == 3
n3 <- length(m3$residuals)
npar3 <- length(m3$coefficients) + 1
AIC3 <- extractAIC(m3, k =2) #AIC
AICc3 <- extractAIC(m3,k=2)+2*npar3*(npar3+1)/(n3-npar3-1) #AICc
BIC3 <- extractAIC(m3,k=log(n3))
Radj3 <- summary(m3)$adj.r.squared
#subset4, size == 4
n4 <- length(m4$residuals)
npar4 <- length(m4$coefficients) + 1
AIC4 <- extractAIC(m4, k =2) #AIC
AICc4 <- extractAIC(m4,k=2)+2*npar4*(npar4+1)/(n4-npar4-1) #AICc
BIC4 <- extractAIC(m4,k=log(n4))
Radj4 <- summary(m4)$adj.r.squared
#subset5, size == 5
n5 <- length(m5$residuals)
npar5 <- length(m5$coefficients) + 1
AIC5 <- extractAIC(m5, k =2) #AIC
AICc5 <- extractAIC(m5,k=2)+2*npar5*(npar5+1)/(n5-npar5-1) #AICc
BIC5 <- extractAIC(m5,k=log(n5))
Radj5 <- summary(m5)$adj.r.squared
#subset6, size == 6
n6 <- length(m6$residuals)
npar6 <- length(m6$coefficients) + 1
AIC6 <- extractAIC(m6, k =2) #AIC
AICc6 <- extractAIC(m6,k=2)+2*npar6*(npar6+1)/(n6-npar6-1) #AICc
BIC6 <- extractAIC(m6,k=log(n6))
Radj6 <- summary(m6)$adj.r.squared
#subset7, size == 7
n7 <- length(m7$residuals)
npar7 <- length(m7$coefficients) + 1
AIC7 <- extractAIC(m7, k =2) #AIC
AICc7 <- extractAIC(m7,k=2)+2*npar7*(npar7+1)/(n7-npar7-1) #AICc
BIC7 <- extractAIC(m7,k=log(n7))
Radj7 <- summary(m7)$adj.r.squared
# backward AIC, BIC
backAIC <- step(m7, direction = 'backward', data=training)
backBIC <- step(m7, direction = 'backward', data=training, k=log(n7))
#forward AIC, BIC
m0 <- lm(lprize~1)
forwardAIC <- step(m0, scope=list(lower=~1,upper=~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound),direction="forward", data=training)
forwardBIC <- step(m0, scope=list(lower=~1,upper=~DrivingAccuracy+GIR+PuttingAverage+BirdieConversion+SandSaves+Scrambling+PuttsPerRound),direction = "forward", data=training, k = log(n7))
detach(training)
#choosing best model
attach(test)
tlprize = log(PrizeMoney)
testm2 <- lm(tlprize~GIR+PuttsPerRound, data = test)
testm3 <- lm(tlprize~GIR+BirdieConversion+SandSaves+PuttsPerRound, data = test)
testm4 <- lm(tlprize ~ PuttingAverage + GIR + PuttsPerRound + BirdieConversion + SandSaves, data = test)
testm5 <- lm(tlprize ~ PuttingAverage + GIR + PuttsPerRound, data = test)
summary(testm2)
summary(testm3)
summary(testm4)
summary(testm5)
detach(test)



