library("bsts")
library("xlsx")
library("forecast")
# 导出数据
gdp <- read.xlsx("GDP.xlsx", 1, header = F, stringsAsFactors = F, 
                 encoding = "UTF-8")

# 绘制时间趋势图
gdp <- ts((0.001 * (gdp[, 2])), frequency = 4, start = c(1992, 1))
plot(gdp)
# 每年每季度图
monthplot(gdp, xlab="", ylab="")
seasonplot(gdp, s = 4, year.labels = T, col = rainbow(12))
# 滞后12阶图，4 8 12最细
lag.plot(gdp, 4, do.lines = F)
Acf(gdp, type = "correlation")
Acf(gdp, type = "partial")

plot(diff(gdp))
Acf(diff(gdp), 100, type = "correlation")
Acf(diff(gdp), 60, type = "partial")

### 序列差异(1 - L)(1 - L^4)Y
y <- vector()
for (i in 6:length(gdp)) {
    y[i] <- (gdp[i] - gdp[i - 1] - gdp[i - 4] + gdp[i - 5])
}
plot(y, type = "l")
Acf(y, 100, type = "correlation")
Acf(y, 60, type = "partial")

###################################################################
Y.train <- window(gdp, end = c(2014, 4))
Y.test <- window(gdp, start = c(2015, 1))

# ETS Analysis and Forecasts
ets <- ets(Y.train, "ZZZ")
ets
plot(ets)

qqnorm(ets$residuals)
qqline(ets$residuals)
Box.test(ets$residuals,type="Ljung-Box")

# h-step ahead forecasts
for1 <- forecast(ets, h = 11)
for1
plot(for1)
lines(Y.test, col = "red")
accuracy(for1,Y.test)

###########################################################
# ARIMA Analysis and Forecasts
arma <- auto.arima(Y.train)
arma
qqnorm(arma$residuals)
qqline(arma$residuals)
Box.test(arma$residuals,type="Ljung-Box")

# h-step ahead forecasts
for2 <- forecast(arma, h = 11)
for2
plot(for2)
lines(Y.test)
accuracy(for2, Y.test)

#####################################################################
# Using bsts package
ss <- AddLocalLinearTrend(list(), Y.train)
ss <- AddSeasonal(ss, Y.train, nseasons = 4)

# model estimation
model <- bsts(Y.train, state.specification = ss, niter = 1000)
names(model)
dim(model$state.contributions)

plot(model)
plot(model, "components", same.scale = F)
plot(model, "residuals")
plot(model, "seasonal")

# prediction
pred <- predict(model, horizon = 11, burn = 100)
plot(pred, plot.original = 36)

# redefine the ts of pred for evaluation
for3 <- ts(pred$mean, start = c(2015, 1), frequency = 4)
for3
accuracy(for3, Y.test)
