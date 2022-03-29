#Libraries
library(lmtest)
library(glmnet)

#Data loading
data <- read_excel("datasetQ02.xlsx")
attach(data)


set.seed(1)
train <- 1:78
test <- 79:104
y <- log(data$sales_1) 
x <- as.matrix(data[,-1]) #Moeten nog log van de prijzen doen

cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y[test])^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)
