#################
#HW3 GINI Regression
#################

y <- read.table('worldbank.csv',header = TRUE, sep = ',', stringsAsFactors = F)
head(y)
summary(y)
dim(y)
y$Key<- NULL
data <- data.frame(y$X2006,y$X2007,y$X2008,y$X2009,y$X2010,y$X2011,y$X2012)

#Replace missing value with mean of row
for(i in 1:nrow(data)){
  data[i,is.na(data[i,])] <- rowMeans(data[i,], na.rm = T, dims = 1)
  }
data$Series <-y$Series.Code
data$Country <- y$Country.Code
head(data)

#convert columns and rows in python

#open the final file
y <- read.table('finalhw3.csv',header = TRUE, sep = ',', stringsAsFactors = F)
head(y)
# GINI is caculated from income, thus income shoule be excluded from this study
y$Income.lowest.20.<-NULL
y$Income.highest.20. <- NULL
#remove missing value
dim(y)
y <- na.omit(y)
summary(y)

# scatterplot matrix

pairs(y[2:9], col="firebrick")

#histgram for all variables
par(mfrow=c(2,4))
hist(y$Electric.consumption)
hist(y$GINI)
hist(y$Air.passengers.carried)
hist(y$GDP.growth)
hist(y$Natural.resources)
hist(y$unemployment)
hist(y$Mortality.rate)
hist(y$Fertility.rate)

#Transform some skewed variables
par(mfrow=c(2,5))
y$log.air.passengers <- log(y$Air.passengers.carried)
hist(y$log.air.passengers)
y$log.Mortality.rate <-log(y$Mortality.rate)
hist(y$log.Mortality.rate)

#split training and texting data
#randomly selected training and test sets
require(leaps)
train <- rep(1, times=nrow(y))
train[sample(1:nrow(y), size=25, replace=FALSE)] <- 0
y <- data.frame(y, train)


# forward stepwise
model4 <- regsubsets(GINI~Electric.consumption+log.air.passengers+
                       unemployment+log.Mortality.rate+Fertility.rate, 
                     data=y[y$train==1,], method="forward")
summary(model4)

names(summary(model4))


# compare C_p values
round(summary(model4)$cp, digits=1)

######################
# find the final model
######################
Final <- lm(GINI~Electric.consumption+
              unemployment+log.Mortality.rate+Fertility.rate, 
            data=y[y$train==1,])
summary(Final)

Final2 <- lm(GINI~
               unemployment+log.Mortality.rate+Fertility.rate, 
             data=y[y$train==1,])
summary(Final2)

#check regression assumption

#VIF is less than 5 which is accepted.
require(Rcpp)
require(usdm)
vif(y[c('unemployment','log.Mortality.rate','Fertility.rate')])

#time series:#residual by row and Durbin-Watson test 
model.year <- lm(y$GINI~y$year)
require(lmtest)
dwtest(model.year, alternative="greater")

#Model form plots: ^?? vs ^y 
par(mfrow=c(2,2))
plot(Final2,las=TRUE)


#Model form plots: ^?? vs xj
x <- y[y$train == 1,]
names(y)
head(x)

par(mfrow=c(1,3))

plot(x$unemployment, Final2$residuals, las=TRUE,
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(Final2)$sigma*2, -summary(Final2)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) # 0 line, +/2 2 RMSE
plot(x$log.Mortality.rate, Final2$residuals, las=TRUE,
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(Final2)$sigma*2, -summary(Final2)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) # 0 line, +/2 2 RMSE
plot(x$Fertility.rate, Final2$residuals, las=TRUE,
     cex.main=1.4, cex.lab=1.4, cex.axis=1.4)
abline(h=c(0, summary(Final2)$sigma*2, -summary(Final2)$sigma*2) , lty=2, lwd=2,
       col=c("gray50", "darkolivegreen4", "darkolivegreen4")) # 0 line, +/2 2 RMSE

#model validation
# R^2 for model
names(summary(Final2))
summary(Final2)$r.squared

# test data R^2_prediction
temp <- predict(Final2, newdata=y[y$train==0,])
# numerator
a.1 <- sum((y[y$train==0,"GINI"] - temp)^2)
# denominator
a.2 <- sum((y[y$train==0,"GINI"] - mean(y[y$train==1,"GINI"]))^2)
# putting components together for R^2_prediction
1 - (a.1/a.2)
