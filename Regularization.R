#1
data1 <- read.csv(file.choose(), header=T)
attach(data1)
X <- data.matrix(data1)
Xt <- t(X)
XtX <- Xt%*%X
eigen <- svd(XtX)
Emax <- max(eigen$d)
Emin <- min(eigen$d)
lamda <- ceiling(runif(1000,0,10^13))
k <- (Emax+lamda) / (Emin+lamda)
logk <- log(k)
loglam <- log(lamda)
plot(loglam, logk)

#2
swiss <- datasets::swiss
attach(swiss)
x <- model.matrix(Fertility~., swiss)[,-1]
y <- Fertility
lambda <- 10^seq(10, -2, length = 100)
# form the train and test vector
set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
#b
m.ols <- lm(Fertility~., data = swiss, subset = train)
#ridge regression
install.packages("glmnet")
library(glmnet)
ridge.m <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlamridge <- cv.out$lambda.min
ridge.pred <- predict(ridge.m, s = bestlamridge, newx = x[test,])
datasetlm <- lm(Fertility~., data = swiss, subset = train)
ols.pred <- predict(datasetlm, newdata = swiss[test,])
#MSE
mean((ols.pred-ytest)^2)
mean((ridge.pred-ytest)^2)
#Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1) 
bestlamlasso <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlamlasso, newx = x[test,])
mean((lasso.pred-ytest)^2) #lasso MSE
#coefficients
coef(m.ols)
predict(ridge.m, type = "coefficients", s = bestlamridge)[1:6,]
lasso.coef <- predict(lasso.mod, type = 'coefficients', s = bestlamlasso)[1:6,]
