#Libraries
set.seed(1)
library(glmnet)

#Data loading
data <- read_excel("datasetQ02.xlsx")
attach(data)

###Data manipulation
#Seasonality
data$season <- NA
data$season[1:13] <- 0; data$season[53:65] <- 0
data$season[14:26] <- 1; data$season[66:78] <- 1
data$season[27:39] <- 2; data$season[79:91] <- 2
data$season[39:52] <- 3; data$season[92:104] <- 3

#log all of the prices
data[,2:17] <- log(data[,2:17])

###Data selection
#Full data range
y <- data$sales_1[2:104]

colselect <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","sales_f","promo_f","sales_s","promo_s","season")
x <- as.matrix(data[2:104,colselect])

#Lagged data range
x.1 <- as.matrix(data[1:103,c(-1,-32)])
colnames(x.1) <- paste0(colnames(x.1),'_lag1')

#Combining original and lagged data
x_total <- cbind(x, x.1)

#Combine x and x_lag
x_total <- cbind(x, x.1)
x_train <- x_total[1:77,]
x_test <- x_total[78:103,]
y_train <- y[1:77]
y_test <- y[78:103]

#CV
cv.out <- cv.glmnet(x_train, y_train, alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.reg <- glmnet(x_train, y_train, alpha = 0, lambda=bestlam)
ridge.pred <- predict(ridge.reg, s = bestlam, newx = x_test)

#RMSE
sqrt(mean((ridge.pred - y_test)^2))



TIME = T+1
prices & promo & seasonality voor T+1 => X
alles behalve seaonality voor T => X.1

Y(T+1) <- X(T+1) + X.1(T)



