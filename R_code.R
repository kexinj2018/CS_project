# Import data to R
train <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")

# Rename column names
colnames(train) <- c("ID", "delin90", "balanceR", "age", "t30", "debtR", "income", "loans", "t90", "mortgage", "t60", "dependents")
colnames(test) <- c("ID", "delin90", "balanceR", "age", "t30", "debtR", "income", "loans", "t90", "mortgage", "t60", "dependents")

# Display the structure of the dataset
str(train)
str(test)

library(caret)
library(MASS) # LDA
library(class) # knn
library(e1071) # Naive Bayes; SVM
library(rpart) # decision tree
library(randomForest) # rf
library(gbm) # gradient boost

#----------------------------------------------------------------
# Missing data imputation
#----------------------------------------------------------------

# Missing data percentage: 20%
round((dim(train)[1] - dim(train[complete.cases(train),])[1])/dim(train)[1], 2)

# There are missing data in explanatory variables: income and dependents 
# Impute missing income with the median income
train$income[is.na(train$income)] <- median(train$income[!is.na(train$income)])
test$income[is.na(test$income)] <- median(test$income[!is.na(test$income)])

# Impute dependents with most frequent element: 0
sort(table(train$dependents),decreasing=TRUE)[1]
#     0 
# 86902 
train$dependents[is.na(train$dependents)] <- 0

sort(table(test$dependents),decreasing=TRUE)[1]
#     0 
# 58618 
test$dependents[is.na(test$dependents)] <- 0


#----------------------------------------------------------------
# exploratory analysis of features
#----------------------------------------------------------------

# Total balance on credit cards and personal lines of credit except real estate 
# divided by the sum of credit limits
#----------
hist(train$balanceR)
quantile(train$balanceR)
boxplot(train$balanceR)
nrow(train[which(train$balanceR>1), ])/nrow(train) # 2% of population > 1
# [1] 0.02214 

# age 
#----------
hist(train$age)

# Number Of Time 30-59 Days Past Due Not Worse in the last 2 years
#----------
hist(train$t30)
quantile(train$t30)
boxplot(train$t30)
nrow(train[which(train$t30 > 40), ])/nrow(train) # 0.18% of population > 40
# [1] 0.001793333

# Monthly debt payments, alimony,living costs divided by monthy gross income
#----------
hist(train$debtR)
quantile(train$debtR)
boxplot(train$debtR)

# MonthlyIncome
#----------
hist(train$income)
quantile(train$income)
boxplot(train$income)

# Number of Open loans and Lines of credit
#----------
hist(train$loans)
quantile(train$loans)
boxplot(train$loans)

# Number of times borrower has been 90 days or more past due.
#----------
hist(train$t90)
quantile(train$t90)
boxplot(train$t90)

# Number of mortgage and real estate loans including home equity lines of credit
#----------
hist(train$mortgage)
quantile(train$mortgage)
boxplot(train$mortgage)

# Number of times borrower has been 60-89 days past due but no worse in the last 2 years.
#----------
hist(train$t60)
quantile(train$t60)
boxplot(train$t60)

# Number of dependents in family excluding themselves (spouse, children etc.)
#----------
hist(train$dependents)
quantile(train$dependents)
boxplot(train$dependents)

#----------------------------------------------------------------
# Split training dataset into training, validation and test sets
#----------------------------------------------------------------

set.seed(1)
ind <- sample(nrow(train), nrow(train))
tr <- train[ind[1:(0.7*nrow(train))], -1] # training set
va <- train[ind[(0.7*nrow(train)+1):(0.9*nrow(train))], -1] # validation set
te <- train[ind[(0.9*nrow(train)+1):nrow(train)], -1] # test set

#----------------------------------------------------------------
# Collinearity checks
#----------------------------------------------------------------

# Check collinearity among covariates in the training set
#--------------------
round(cor(tr), 3)
#            delin90 balanceR    age    t30  debtR income  loans    t90 mortgage    t60 dependents
# delin90      1.000   -0.002 -0.115  0.132 -0.008 -0.019 -0.028  0.123   -0.008  0.109      0.047
# balanceR    -0.002    1.000 -0.005 -0.001  0.002  0.005 -0.013 -0.001    0.005 -0.001      0.000
# age         -0.115   -0.005  1.000 -0.063  0.023  0.032  0.149 -0.060    0.034 -0.057     -0.214
# t30          0.132   -0.001 -0.063  1.000 -0.006 -0.010 -0.056  0.984   -0.030  0.987     -0.005
# debtR       -0.008    0.002  0.023 -0.006  1.000 -0.019  0.047 -0.008    0.108 -0.007     -0.041
# income      -0.019    0.005  0.032 -0.010 -0.019  1.000  0.097 -0.012    0.132 -0.011      0.071
# loans       -0.028   -0.013  0.149 -0.056  0.047  0.097  1.000 -0.080    0.435 -0.071      0.075
# t90          0.123   -0.001 -0.060  0.984 -0.008 -0.012 -0.080  1.000   -0.045  0.993     -0.012
# mortgage    -0.008    0.005  0.034 -0.030  0.108  0.132  0.435 -0.045    1.000 -0.039      0.129
# t60          0.109   -0.001 -0.057  0.987 -0.007 -0.011 -0.071  0.993   -0.039  1.000     -0.013
# dependents   0.047    0.000 -0.214 -0.005 -0.041  0.071  0.075 -0.012    0.129 -0.013      1.000


# Covariates t30, t60 and t90 are highly correlated
# Since t30 and response has the largest correlation, 0.132 compared to 0.106 and 0.123,
# we'll keep t30 into the model and not inlclude the other two covariates, t60 and t90

# New training data excluding covariates t60 and t90
tr <- tr[,c(-8, -10)]

# Double check correlation of the new training set
round(cor(tr), 3)
#            delin90 balanceR    age    t30  debtR income  loans mortgage dependents
# delin90      1.000   -0.002 -0.115  0.132 -0.008 -0.019 -0.028   -0.008      0.047
# balanceR    -0.002    1.000 -0.005 -0.001  0.002  0.005 -0.013    0.005      0.000
# age         -0.115   -0.005  1.000 -0.063  0.023  0.032  0.149    0.034     -0.214
# t30          0.132   -0.001 -0.063  1.000 -0.006 -0.010 -0.056   -0.030     -0.005
# debtR       -0.008    0.002  0.023 -0.006  1.000 -0.019  0.047    0.108     -0.041
# income      -0.019    0.005  0.032 -0.010 -0.019  1.000  0.097    0.132      0.071
# loans       -0.028   -0.013  0.149 -0.056  0.047  0.097  1.000    0.435      0.075
# mortgage    -0.008    0.005  0.034 -0.030  0.108  0.132  0.435    1.000      0.129
# dependents   0.047    0.000 -0.214 -0.005 -0.041  0.071  0.075    0.129      1.000


#----------------------------------------------------------------
# Feature Scaling & Outlier issues
#----------------------------------------------------------------

# A helper function to help determine outliers, return threshold
thre <- function(vector) {
  return(unname(quantile(vector, 0.75)) + 1.5*IQR(vector))
}

# Scaling - balanceR
#----------
tr[which(tr$balanceR>thre(tr$balanceR)), 2] <- 1
tr[which(tr$balanceR<=thre(tr$balanceR)), 2] <- (tr[which(tr$balanceR<=thre(tr$balanceR)), 2] - min(tr[which(tr$balanceR<=thre(tr$balanceR)), 2]))/(max(tr[which(tr$balanceR<=thre(tr$balanceR)), 2]) - min(tr[which(tr$balanceR<=thre(tr$balanceR)), 2]))

va[which(va$balanceR>thre(va$balanceR)), 2] <- 1
va[which(va$balanceR<=thre(va$balanceR)), 2] <- (va[which(va$balanceR<=thre(va$balanceR)), 2] - min(va[which(va$balanceR<=thre(va$balanceR)), 2]))/(max(va[which(va$balanceR<=thre(va$balanceR)), 2]) - min(va[which(va$balanceR<=thre(va$balanceR)), 2]))

te[which(te$balanceR>thre(te$balanceR)), 2] <- 1
te[which(te$balanceR<=thre(te$balanceR)), 2] <- (te[which(te$balanceR<=thre(te$balanceR)), 2] - min(te[which(te$balanceR<=thre(te$balanceR)), 2]))/(max(te[which(te$balanceR<=thre(te$balanceR)), 2]) - min(te[which(te$balanceR<=thre(te$balanceR)), 2]))

# Scaling - age
#----------
tr$age <- (tr$age - mean(tr$age))/sd(tr$age)
va$age <- (va$age - mean(va$age))/sd(va$age)
te$age <- (te$age - mean(te$age))/sd(te$age)


# Scaling - t30
#----------
tr[which(tr$t30>thre(tr$t30)), 4] <- 1
# tr[which(tr$t30<=thre(tr$t30)), 4] <- (tr[which(tr$t30<=thre(tr$t30)), 4] - min(tr[which(tr$t30<=thre(tr$t30)), 4]))/(max(tr[which(tr$t30<=thre(tr$t30)), 4]) - min(tr[which(tr$t30<=thre(tr$t30)), 4]))
tr[which(tr$t30<=thre(tr$t30)), 4] <- 0

va[which(va$t30>thre(va$t30)), 4] <- 1
# va[which(va$t30<=thre(va$t30)), 4] <- (va[which(va$t30<=thre(va$t30)), 4] - min(va[which(va$t30<=thre(va$t30)), 4]))/(max(va[which(va$t30<=thre(va$t30)), 4]) - min(va[which(va$t30<=thre(va$t30)), 4]))
va[which(va$t30<=thre(va$t30)), 4] <- 0

te[which(te$t30>thre(te$t30)), 4] <- 1
# te[which(te$t30<=thre(te$t30)), 4] <- (te[which(te$t30<=thre(te$t30)), 4] - min(te[which(te$t30<=thre(te$t30)), 4]))/(max(te[which(te$t30<=thre(te$t30)), 4]) - min(te[which(te$t30<=thre(te$t30)), 4]))
te[which(te$t30<=thre(te$t30)), 4] <- 0

# Scaling - debtR
#----------
tr[which(tr$debtR>thre(tr$debtR)), 5] <- 1
tr[which(tr$debtR<=thre(tr$debtR)), 5] <- (tr[which(tr$debtR<=thre(tr$debtR)), 5] - min(tr[which(tr$debtR<=thre(tr$debtR)), 5]))/(max(tr[which(tr$debtR<=thre(tr$debtR)), 5]) - min(tr[which(tr$debtR<=thre(tr$debtR)), 5]))

va[which(va$debtR>thre(va$debtR)), 5] <- 1
va[which(va$debtR<=thre(va$debtR)), 5] <- (va[which(va$debtR<=thre(va$debtR)), 5] - min(va[which(va$debtR<=thre(va$debtR)), 5]))/(max(va[which(va$debtR<=thre(va$debtR)), 5]) - min(va[which(va$debtR<=thre(va$debtR)), 5]))

te[which(te$debtR>thre(te$debtR)), 5] <- 1
te[which(te$debtR<=thre(te$debtR)), 5] <- (te[which(te$debtR<=thre(te$debtR)), 5] - min(te[which(te$debtR<=thre(te$debtR)), 5]))/(max(te[which(te$debtR<=thre(te$debtR)), 5]) - min(te[which(te$debtR<=thre(te$debtR)), 5]))


# Scaling - MonthlyIncome
#----------
tr[which(tr$income>thre(tr$income)), 6] <- 1
tr[which(tr$income<=thre(tr$income)), 6] <- (tr[which(tr$income<=thre(tr$income)), 6] - min(tr[which(tr$income<=thre(tr$income)), 6]))/(max(tr[which(tr$income<=thre(tr$income)), 6]) - min(tr[which(tr$income<=thre(tr$income)), 6]))

va[which(va$income>thre(va$income)), 6] <- 1
va[which(va$income<=thre(va$income)), 6] <- (va[which(va$income<=thre(va$income)), 6] - min(va[which(va$income<=thre(va$income)), 6]))/(max(va[which(va$income<=thre(va$income)), 6]) - min(va[which(va$income<=thre(va$income)), 6]))

te[which(te$income>thre(te$income)), 6] <- 1
te[which(te$income<=thre(te$income)), 6] <- (te[which(te$income<=thre(te$income)), 6] - min(te[which(te$income<=thre(te$income)), 6]))/(max(te[which(te$income<=thre(te$income)), 6]) - min(te[which(te$income<=thre(te$income)), 6]))



# Scaling - loans
#----------
tr[which(tr$loans>thre(tr$loans)), 7] <- 1
tr[which(tr$loans<=thre(tr$loans)), 7] <- (tr[which(tr$loans<=thre(tr$loans)), 7] - min(tr[which(tr$loans<=thre(tr$loans)), 7]))/(max(tr[which(tr$loans<=thre(tr$loans)), 7]) - min(tr[which(tr$loans<=thre(tr$loans)), 7]))

va[which(va$loans>thre(va$loans)), 7] <- 1
va[which(va$loans<=thre(va$loans)), 7] <- (va[which(va$loans<=thre(va$loans)), 7] - min(va[which(va$loans<=thre(va$loans)), 7]))/(max(va[which(va$loans<=thre(va$loans)), 7]) - min(va[which(va$loans<=thre(va$loans)), 7]))

te[which(te$loans>thre(te$loans)), 7] <- 1
te[which(te$loans<=thre(te$loans)), 7] <- (te[which(te$loans<=thre(te$loans)), 7] - min(te[which(te$loans<=thre(te$loans)), 7]))/(max(te[which(te$loans<=thre(te$loans)), 7]) - min(te[which(te$loans<=thre(te$loans)), 7]))


# Scaling - mortgage
#----------
tr[which(tr$mortgage>thre(tr$mortgage)), 8] <- 1
tr[which(tr$mortgage<=thre(tr$mortgage)), 8] <- (tr[which(tr$mortgage<=thre(tr$mortgage)), 8] - min(tr[which(tr$mortgage<=thre(tr$mortgage)), 8]))/(max(tr[which(tr$mortgage<=thre(tr$mortgage)), 8]) - min(tr[which(tr$mortgage<=thre(tr$mortgage)), 8]))

va[which(va$mortgage>thre(va$mortgage)), 8] <- 1
va[which(va$mortgage<=thre(va$mortgage)), 8] <- (va[which(va$mortgage<=thre(va$mortgage)), 8] - min(va[which(va$mortgage<=thre(va$mortgage)), 8]))/(max(va[which(va$mortgage<=thre(va$mortgage)), 8]) - min(va[which(va$mortgage<=thre(va$mortgage)), 8]))

te[which(te$mortgage>thre(te$mortgage)), 8] <- 1
te[which(te$mortgage<=thre(te$mortgage)), 8] <- (te[which(te$mortgage<=thre(te$mortgage)), 8] - min(te[which(te$mortgage<=thre(te$mortgage)), 8]))/(max(te[which(te$mortgage<=thre(te$mortgage)), 8]) - min(te[which(te$mortgage<=thre(te$mortgage)), 8]))


# Scaling - dependents 
#----------
tr[which(tr$dependents>thre(tr$dependents)), 9] <- 1
tr[which(tr$dependents<=thre(tr$dependents)), 9] <- (tr[which(tr$dependents<=thre(tr$dependents)), 9] - min(tr[which(tr$dependents<=thre(tr$dependents)), 9]))/(max(tr[which(tr$dependents<=thre(tr$dependents)), 9]) - min(tr[which(tr$dependents<=thre(tr$dependents)), 9]))

va[which(va$dependents>thre(va$dependents)), 9] <- 1
va[which(va$dependents<=thre(va$dependents)), 9] <- (va[which(va$dependents<=thre(va$dependents)), 9] - min(va[which(va$dependents<=thre(va$dependents)), 9]))/(max(va[which(va$dependents<=thre(va$dependents)), 9]) - min(va[which(va$dependents<=thre(va$dependents)), 9]))

te[which(te$dependents>thre(te$dependents)), 9] <- 1
te[which(te$dependents<=thre(te$dependents)), 9] <- (te[which(te$dependents<=thre(te$dependents)), 9] - min(te[which(te$dependents<=thre(te$dependents)), 9]))/(max(te[which(te$dependents<=thre(te$dependents)), 9]) - min(te[which(te$dependents<=thre(te$dependents)), 9]))

#----------------------------------------------------------------
# Preliminary Analysis on validation set
#----------------------------------------------------------------


# logistic regresison
glm.tr <- glm(factor(delin90) ~ ., family = binomial, data = glm.tr)
summary(glm.tr)
vif(glm.tr)
# Step wise model selection
step.newTr <- step(glm.tr)

## FINAL LOGISTIC MODEL WITH COVARIATES
## age, debtR, income, t30, mortgage, dependents
glm.final <- glm(factor(delin90) ~ age + debtR + income + t30 + mortgage + dependents, family = binomial, data = tr)
summary(glm.final)
glm.va <- predict(glm.final, newdata = va, type = "response")
glm.tab <- table(as.numeric(glm.va > 0.5), va$delin90)
confusionMatrix(glm.tab) #  Accuracy : 0.9315       

# LDA
lda.tr <- lda(factor(delin90) ~ ., data = tr)
lda.prob <- predict(lda.tr, newdata = va, type = response)
lda.tab <- table(lda.prob$class, va$delin90)
confusionMatrix(lda.tab) #  Accuracy : 0.9207   

# knn
set.seed (1)
knn.tr <- knn(train = tr, test = va[,c(-8, -10)], cl = factor(tr$delin90), k = 3)
knn.tab <- table(knn.tr, va$delin90)
confusionMatrix(knn.tab)  # Accuracy : 0.999 This accuracy is questionable based on my previous runs.      

# Naive Bayes
nBayes.tr <- naiveBayes(factor(delin90) ~ ., data = tr)
nBayes.prob <- predict(nBayes.tr, newdata = va, type = "class")
nBayes.tab <- table(nBayes.prob, va$delin90)
confusionMatrix(nBayes.tab) # Accuracy : 0.8228          

# Decision tree
# cp: complexity parameter - need to tune
tree.tr <- rpart(factor(delin90) ~ ., data = tr, cp = 0.001)
tree.prob <- predict(tree.tr, newdata = va, type = "class")
tree.tab <- table(tree.prob, va$delin90)
confusionMatrix(tree.tab) # Accuracy : 0.9317 

# Random Forest
set.seed (1)
rf.tr <- randomForest(factor(delin90) ~ ., data = tr, ntree=500)
rf.prob <- predict(rf.tr, newdata = va)
rf.tab <- table(rf.prob, va$delin90)
confusionMatrix(rf.tab) # Accuracy : 0.9318

# Gradient Boosting
# set.seed (1)
# gb.tr <- gbm(factor(delin90) ~ ., data = tr, distribution = "gaussian", n.trees=5000, interaction.depth=4)
# gb.pred <- predict(gb.tr, newdata = va, n.trees=5000)
# gb.tab <- table(gb.pred$class, va$delin90)
# confusionMatrix(gb.tab)

# SVM
svm.tr <- svm(factor(delin90) ~ ., data = tr, kernel = "linear")
svm.pred <- predict(svm.tr, newdata = va)
svm.tab <- table(svm.pred, va$delin90)
confusionMatrix(svm.tab) # Accuracy : 0.9315 

#----------------------------------------------------------------
# Tune hyperparameters for Random Forest on validation set
# Due to time constraint, will not tune hyperparameters in other models
#----------------------------------------------------------------

# Plot variable importance
importance(rf.tr)
varImpPlot(rf.tr)

# Tune two parameters in random forest: 
# mtry Number of variables randomly sampled as candidates at each split.

# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
# set.seed(1)
# tunegrid <- expand.grid(.mtry=c(1:8))
# rf_gridsearch <- train(factor(delin90)~., data=va, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
# print(rf_gridsearch)
# plot(rf_gridsearch)

bestmtry <- tuneRF(va[,-1], va[,1], stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)
# mtry   OOBError
# 2    2 0.05071232
# 3    3 0.05152021
# 4    4 0.05217398

#----------------------------------------------------------------
# Final model accuracy on test set
#----------------------------------------------------------------

# logistic regresison
glm.te <- predict(glm.final, newdata = te, type = "response")
glm.tab.te <- table(as.numeric(glm.te > 0.5), te$delin90)
confusionMatrix(glm.tab.te) #  Accuracy : 0.9357       

# LDA
lda.prob.te <- predict(lda.tr, newdata = te, type = response)
lda.tab.te <- table(lda.prob.te$class, te$delin90)
confusionMatrix(lda.tab.te) #  Accuracy : 0.9173   

# knn
set.seed (1)
knn.tr.te <- knn(train = tr, test = te[,c(-8, -10)], cl = factor(tr$delin90), k = 3)
knn.tab.te <- table(knn.tr.te, te$delin90)
confusionMatrix(knn.tab.te)  # Accuracy : 0.9994 This accuracy is questionable based on my previous runs.      

# Naive Bayes
nBayes.prob.te <- predict(nBayes.tr, newdata = te, type = "class")
nBayes.tab.te <- table(nBayes.prob.te, te$delin90)
confusionMatrix(nBayes.tab.te) # Accuracy : 0.8235          

# Decision tree
# cp: complexity parameter - need to tune
tree.prob.te <- predict(tree.tr, newdata = te, type = "class")
tree.tab.te <- table(tree.prob.te, te$delin90)
confusionMatrix(tree.tab.te) # Accuracy : 0.9334 

# Random Forest
set.seed (1)
rf.tr.te <- randomForest(factor(delin90) ~ ., data = tr, ntree=500, mtry = 2)
rf.prob.te <- predict(rf.tr.te, newdata = te)
rf.tab.te <- table(rf.prob.te, te$delin90)
confusionMatrix(rf.tab) # Accuracy : 0.9318

# SVM
svm.pred.te <- predict(svm.tr, newdata = te)
svm.tab.te <- table(svm.pred.te, te$delin90)
confusionMatrix(svm.tab.te) # Accuracy : 0.9359 
