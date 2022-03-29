#Libraries
library(glmnet)

#Data loading
data <- read_excel("datasetQ02.xlsx")
attach(data)

#log all of the prices
data[,2:17] <- log(data[,2:17])

set.seed(1)
y <- data$sales_1[2:104]
x <- as.matrix(data[2:104,10:25])
x.1 <- as.matrix(data[1:103,-1])
colnames(x.1) <- paste0(colnames(x.1),'_lag1')

#Combine x and x_lag
x_total <- cbind(x, x.1)
x_train <- x_total[1:77]
x_test <- x_total[78:103]
y_train <- y[1:77]
y_test <- y[78:103]

#CV
cv.out <- cv.glmnet(x_train, log(y_train), alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.reg <- glmnet(x_train, log(y_train), alpha = 0, lambda=bestlam)
ridge.pred <- predict(ridge.reg, s = bestlam, newx = x_test)
