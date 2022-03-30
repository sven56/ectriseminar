#Libraries
set.seed(1)
library(glmnet)

#Data loading
data <- read_excel("datasetQ02.xlsx")

###Data manipulation
#Seasonality
data$season_2 <- 0; data$season_3 <- 0; data$season_4 <- 0
data$season_2[14:26] <- 1; data$season_2[66:78] <- 1
data$season_3[27:39] <- 1; data$season_3[79:91] <- 1
data$season_4[39:52] <- 1; data$season_4[92:104] <- 1


#log all of the prices and sales
data[,2:17] <- log(data[,2:17])

###Data selection
#Full data range (CHANGE SALES_X)
y <- data$sales_1[3:104]

colselect0 <-c("price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","promo_f","promo_s","season_2","season_3","season_4") 
  c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","sales_f","promo_f","sales_s","promo_s","season")
x.0 <- as.matrix(data[3:104,colselect0])

#Lagged data range
colselect1 <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","sales_f","promo_f","sales_s","promo_s")
x.1 <- as.matrix(data[2:103,colselect1])
colnames(x.1) <- paste0(colnames(x.1),'_lag1')

colselect2 <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","sales_f","sales_s")
x.2 <- as.matrix(data[1:102,colselect2])
colnames(x.2) <- paste0(colnames(x.2),'_lag2')

#Combining original and lagged data
x_total <- cbind(x.0, x.1, x.2)

#Combine x and x_lag
x_train <- x_total[1:76,]
x_test <- x_total[77:102,]
y_train <- y[1:76]
y_test <- y[77:102]

#CV
cv.out <- cv.glmnet(x_train, y_train, alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.reg <- glmnet(x_train, y_train, alpha = 0, lambda=bestlam)
ridge.pred <- predict(ridge.reg, s = bestlam, newx = x_test)

#RMSE
sqrt(mean((ridge.pred - y_test)^2))


#Init coefficient matrix
#matrix <- matrix(data=NA, nrow=59, ncol=9)
#matrix[,1] <- colnames(x_total)
#matrix[,9] <- as.vector(ridge.reg$beta)


