library(tidyverse)

model <- lm(sales_1 ~ price_1 + promo_1 + sales_s + sales_f + price_s + price_f, data = data)
summary(model)