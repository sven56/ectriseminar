#Libraries
library(glmnet)

#Data loading
data <- read_excel("datasetQ02.xlsx")
attach(data)

#log all of the prices
data[,2:17] <- log(data[,2:17])

set.seed(1)
<<<<<<< HEAD
y <- data$sales_1[2:104]
x <- as.matrix(data[2:104,10:25])
x.1 <- as.matrix(data[1:103,-1])
colnames(x.1) <- paste0(colnames(x.1),'_lag1')
=======
train <- 1:78
test <- 79:104
y <- log(data$sales_1) 
x <- as.matrix(data[,-1]) #Moeten nog log van de prijzen doen
>>>>>>> 7155abf96461046c6e6207e6443205a32f0cba52

#Combine x and x_lag
x_total <- cbind(x, x.1)

#CV
cv.out <- cv.glmnet(x_total[1:75], log(y), alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.reg <- glmnet(x_total, log(y), alpha = 0, lambda=bestlam)
ridge.pred <- predict(ridge.reg, s = bestlam, newx = x_total[])
