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
y <- data$sales_1[3:104]

colselect0 <-c("price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","promo_f","promo_s","season") 
  c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","sales_f","promo_f","sales_s","promo_s","season")
x.0 <- as.matrix(data[3:104],colselect0)

#Lagged data range
colselect1 <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","price_1","price_2","price_3","price_4","price_5","price_6","price_7","price_8","promo_1","promo_2","promo_3","promo_4","promo_5","promo_6","promo_7","promo_8","sales_f","promo_f","sales_s","promo_s")
x.1 <- as.matrix(data[2:103],colselect1)
colnames(x.1) <- paste0(colnames(x.1),'_lag1')

colselect2 <- c("sales_1","sales_2","sales_3","sales_4","sales_5","sales_6","sales_7","sales_8","sales_f","sales_s")
x.2 <- as.matrix(data[data[1:102],colselct2])

#Combining original and lagged data
x_total <- cbind(x, x.1, x.2)

#Combine x and x_lag
x_train <- x_total[1:76,]
x_test <- x_total[77:102,]
y_train <- y[1:76,]
y_test <- y[77:102,]

#CV
cv.out <- cv.glmnet(x_train, y_train, alpha = 0, nfolds=6)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.reg <- glmnet(x_train, y_train, alpha = 0, lambda=bestlam)
ridge.pred <- predict(ridge.reg, s = bestlam, newx = x_test)

#RMSE
sqrt(mean((ridge.pred - y_test)^2))


      
#0_lag <- price, promo, seasonality: [3:104]
#1_lag <- alles behalve seasonality: [2:103]
#2_lag <- alles behalve seasonality, price, promo: [1:102]


