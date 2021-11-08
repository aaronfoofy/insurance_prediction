
library(data.table)
library(rpart)
library(rpart.plot)
library(corrplot)

library(car)
library(caTools) #for logreg analysis?
library(nnet) 
library(Metrics)

setwd('C:/Users/aaroncode/Desktop/RE 6013 Biz analytics/AY21S1 CBA Paper/AY21S1 CBA Paper')
health_data <- fread('premium2.csv',stringsAsFactors = T)
summary(health_data)

sum(is.na(health_data)) # no null values in dataset, is all gd
# BMI = weight / H^2
health_data$BMI <- (health_data$Weight / ((health_data$Height / 100)^2))
head(health_data)
summary(health_data)

round(cor(health_data),2)
corrplot(cor(health_data), type = 'lower', cex.main = 0.9,main = "Correlation plot of health_data" ,mar = c(0,0,2,0))

# lets try lin reg, CART pruned/unpruned ,RF?, neural net, multiple lin reg ,

# Linreg

set.seed(2021)

# 70% train test split 
train <- sample.split(Y = health_data$Premium, SplitRatio = 0.7)
trainset <- subset(health_data, train == T)
testset <- subset(health_data, train == F)

summary(trainset$Premium)
summary(testset$Premium)

# Develop model on trainset
m_all <- lm(Premium ~ ., data = trainset)
summary(m_all)
residuals(m_all) 
vif(m_all)
# backwards elimination
m1 <- step(m_all) 
summary(m1)

mape(trainset$Premium, predict(m1, newdata = trainset))
mape(testset$Premium, predict(m1, newdata = testset))

rmse(trainset$Premium, predict(m1, newdata = trainset))
rmse(testset$Premium, predict(m1, newdata = testset))
####################################################################################

# CART 
m2 <- rpart(Premium ~ ., data = trainset, method = 'anova',
            control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(m2, nn= T, main = "Maximal Tree")

# prints the maximal tree m2 onto the console.
print(m2)

# prints out the pruning sequence and 10-fold CV errors, as a table.
printcp(m2)

# Display the pruning sequence and 10-fold CV errors, as a chart.
plotcp(m2, main = "Subtrees")


# [Optional] Extract the Optimal Tree via code instead of eye power ------------
# Compute min CVerror + 1SE in maximal tree m2.
CVerror.cap <- m2$cptable[which.min(m2$cptable[,"xerror"]), "xerror"] + m2$cptable[which.min(m2$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m2.
i <- 1; j<- 4
while (m2$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
i

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(m2$cptable[i,1] * m2$cptable[i-1,1]), 1)
# cp.opt = 0.02321695

# Prune the max tree using a particular CP value
cart2 <- prune(m2, cp = cp.opt)
printcp(cart2, digits = 3)


print(cart2)

rpart.plot(cart2, nn = T, main = "Optimal Tree")
## The number inside each node represent the mean value of Y.

cart2$variable.importance
## Age >nummajsurgeries>weight 

mape(trainset$Premium, predict(m2, newdata = trainset))
mape(testset$Premium, predict(m2, newdata = testset))

rmse(trainset$Premium, predict(m2, newdata = trainset))
rmse(testset$Premium, predict(m2, newdata = testset))
# extremely low error values

####################################################################################



