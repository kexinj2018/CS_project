train <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")

colnames(train) <- c("ID", "default", "balanceR", "age", "t30", "debtR", "income", "loans", "t90", "mortgage", "t60", "dependents")

#----------------------------------------------------------------
# naive analysis
# delete all rows with missing values
# split the remaining data according to 7/3 rule 

# missing data
dim(train[!complete.cases(train),])
# [1] 29731    12

# delete rows with NA 
try <- na.omit(train)
# [1] 120269     12

# split the data according to 7/3 rule 
set.seed(1)
ind <- sample(nrow(try), 84188) # randomly select 70% of data
t1 <- try[ind, -1] # new training data
t2 <- try[-ind, -1] # new test data

# summary of data
summary(t1)

#----------------------------------------------------------------
# exploratory statistical analysis
#----------------------------------------------------------------


####### Total balance on credit cards and personal lines of credit except real estate 
# divided by the sum of credit limits
#----------
# need to deal with outlier issues
hist(t1[,2])
range(t1[,2])
# [1]     0 50708
balanceR <- sort(t1[,2])
balanceR[84160:84188]
# [1]  4133  4556  4666  4724  4881  5015  5118  5451  5649  5781  5994  6190  6726
# [14]  7096  7394  7809  7907  8228  8710  9340  9684 10209 11843 12369 13498 17441
# [27] 18300 29110 50708

balanceR[84090:84188]
# [1]    14.54092    29.78488    50.00000    61.00000    73.84615    85.00000
# [7]   112.00000   206.00000   227.00000   250.58333   361.00000   429.00000
# [13]   452.00000   456.00000   470.00000   502.00000   594.00000   638.00000
# [19]   689.00000   742.00000   760.00000   765.00000   904.00000   941.00000
# [25]   946.00000   957.00000   958.00000   979.00000  1090.00000  1100.00000
# [31]  1143.00000  1194.00000  1563.00000  1567.00000  1570.00000  1655.00000
# [37]  1708.00000  1761.00000  1785.00000  1803.00000  1819.00000  1897.00000
# [43]  1917.00000  1924.00000  1982.00000  2012.00000  2019.00000  2175.00000
# [49]  2231.00000  2340.00000  2492.00000  2619.00000  2664.00000  2773.00000
# [55]  2990.00000  2996.00000  3046.00000  3264.00000  3383.00000  3448.00000
# [61]  3484.00000  3538.00000  3572.00000  3617.00000  3655.00000  3666.00000
# [67]  3677.00000  3746.00000  3845.00000  3878.00000  4133.00000  4556.00000
# [73]  4666.00000  4724.00000  4881.00000  5015.00000  5118.00000  5451.00000
# [79]  5649.00000  5781.00000  5994.00000  6190.00000  6726.00000  7096.00000
# [85]  7394.00000  7809.00000  7907.00000  8228.00000  8710.00000  9340.00000
# [91]  9684.00000 10209.00000 11843.00000 12369.00000 13498.00000 17441.00000
# [97] 18300.00000 29110.00000 50708.00000

hist(balanceR[1:84050])
hist(balanceR[84050:84188])

# age 
#----------
hist(t1[,3])

# Number Of Time 30-59 Days Past Due Not Worse
#----------
# need to deal with outlier issues
t30 <- t1[,4]
hist(sort(t30)[83090:84188])
hist(t30)
unique(t30)
#  [1]  0  2  1  3  4  8  5  6 98  7  9 13 10 96 12
hist(sort(t30)[1:84000])
hist(sort(t30)[84000:84188])
sort(t30)[84000:84188]
# [1]  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6  6
# [27]  6  6  6  6  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7  7
# [53]  7  7  7  7  7  7  7  7  7  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  9
# [79]  9  9  9  9  9 10 12 13 96 96 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [105] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [131] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [157] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [183] 98 98 98 98 98 98 98

sort(t30)[84086:84188]
# [1] 96 96 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [27] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [53] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [79] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98


# DebtRatio
#----------
debtR <- t1[,5]
hist(debtR)
hist(sort(debtR)[84000:84188])
hist(sort(debtR)[1:82000])

sort(debtR)[84100:84188]
# [1]  4193.0  4211.0  4221.0  4245.0  4317.0  4326.0  4334.0  4351.0  4364.0  4396.5
# [11]  4398.0  4439.0  4460.0  4481.0  4483.0  4493.0  4519.0  4538.0  4564.0  4594.0
# [21]  4645.0  4656.0  4658.0  4685.0  4685.0  4690.0  4717.0  4834.0  4857.0  4896.0
# [31]  4898.0  4942.0  4957.0  4977.0  4985.0  5013.0  5027.0  5181.0  5200.0  5202.0
# [41]  5264.0  5323.0  5340.0  5342.0  5373.0  5435.0  5478.0  5483.0  5519.0  5562.0
# [51]  5587.0  5696.0  5714.0  5794.0  5821.0  5857.0  6002.0  6164.0  6243.0  6430.0
# [61]  6530.0  6591.0  6784.0  6888.0  6997.0  7030.0  7145.0  7331.0  7622.0  8151.0
# [71]  8416.0  8500.0  8795.0  8901.0  8960.0  9260.0  9908.0  9958.0 11195.0 11887.0
# [81] 12018.0 12778.0 14969.0 20000.0 20809.0 25033.0 49112.0 60212.0 61106.5

# MonthlyIncome
#----------
income <- t1[,6]
hist(income)
hist(sort(income)[84100:84188])
# > sort(income)[84100:84188]
# [1]   78500   79166   80000   80000   80000   80176   80416   80600   81000   82000
# [11]   82000   82000   82083   83000   83700   84279   85000   85000   85000   86537
# [21]   86980   88000   88333   88430   90000   90000   90833   96000   96500   96668
# [31]   97000   97336  100000  100000  100000  100000  100000  100000  101056  101167
# [41]  105496  105591  107400  110000  110775  112100  120000  120000  120000  121000
# [51]  121200  125000  125000  128000  131350  131800  133000  140000  141500  142000
# [61]  143000  148000  150000  150000  150300  151855  160000  166666  166700  173000
# [71]  203500  208333  218674  235000  237490  250000  304000  408333  440000  562466
# [81]  582369  629000  649587  699530  835040 1072500 1560100 1794060 3008750

hist(sort(income)[1:83000])

# Number of Open loans and Lines of credit
#----------
loans <- t1[,7]
hist(loans)

range(loans)
# [1]  0 58

unique(loans)
# [1]  5 17  6 10  9 11  3  8  2 15  4 14  7 12 16  0 28  1 13 22 18 19 23 21 20 26
# [27] 25 24 31 27 45 57 34 35 39 29 30 36 33 32 48 40 43 50 37 47 49 42 46 54 38 52
# [53] 44 51 58 56

as.data.frame(table(loans))


# Number of times borrower has been 90 days or more past due.
#----------
# need to deal with outlier issues
t90 <- t1[,8]

hist(t90)
as.data.frame(table(t90))
hist(sort(t90)[1:84000])


# Number of mortgage and real estate loans including home equity lines of credit
#----------
# need to deal with outlier issues
mortgage <- t1[,9]

hist(mortgage)
range(mortgage)
# [1]  0 54
unique(mortgage)
# [1]  3  2  0  1  4  5  6  8  7 14 10  9 23 25 11 13 16 12 21 19 32 15 54 17 20 18
as.data.frame(table(mortgage))

sort(mortgage)[84100:84188]

hist(sort(mortgage)[84100:84188])
hist(sort(mortgage)[1:84000])


# Number of times borrower has been 60-89 days past due but no worse in the last 2 years.
#----------
# need to deal with outlier issues
t60 <- t1[,10]

hist(t60)
range(t60)
# [1]  0 98
unique(t60)
# [1]  0  1  2  3  4 98  8  5  6  7 96 11

as.data.frame(table(t60))

hist(sort(t60)[1:84000])
hist(sort(t60)[84000:84188])

sort(t60)[84000:84188]
# [1]  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
# [27]  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4
# [53]  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5  6  6  6  6  6  6  6
# [79]  6  6  7  7  7  7  8 11 96 96 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [105] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [131] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [157] 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98 98
# [183] 98 98 98 98 98 98 98

# Number of dependents in family excluding themselves (spouse, children etc.)
#----------
# need to deal with outlier issues
dependents <- t1[,11]

hist(dependents)
range(dependents)
# [1]  0 20
unique(dependents)
# [1]  2  0  1  3  4  5  6  7  8  9 20 10 13
as.data.frame(table(dependents))

hist(sort(dependents)[84100:84188])
hist(sort(dependents)[1:84100])


#----------------------------------------------------------------
# Model/covariate selection? 
#----------------------------------------------------------------


#----------------------------------------------------------------
# logistic regresison
#----------------------------------------------------------------

# question: as.factor() for categorical variables?
t1.glm <- glm(factor(default) ~ ., data = t1, family = binomial)
summary(t1.glm)
# Call:
#   glm(formula = factor(default) ~ ., family = binomial, data = t1)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.0181  -0.3960  -0.3243  -0.2626   5.0906  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.455e+00  5.548e-02 -26.221  < 2e-16 ***
#   balanceR    -5.504e-05  1.004e-04  -0.548   0.5837    
# age         -2.571e-02  1.117e-03 -23.015  < 2e-16 ***
#   t30          5.034e-01  1.438e-02  34.996  < 2e-16 ***
#   debtR       -1.258e-04  5.624e-05  -2.236   0.0254 *  
#   income      -3.988e-05  3.918e-06 -10.180  < 2e-16 ***
#   loans       -5.214e-03  3.292e-03  -1.584   0.1132    
# t90          4.333e-01  1.984e-02  21.842  < 2e-16 ***
#   mortgage     6.987e-02  1.318e-02   5.299 1.16e-07 ***
#   t60         -9.012e-01  2.301e-02 -39.172  < 2e-16 ***
#   dependents   9.888e-02  1.161e-02   8.520  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 42088  on 84187  degrees of freedom
# Residual deviance: 38841  on 84177  degrees of freedom
# AIC: 38863
# 
# Number of Fisher Scoring iterations: 6

# Performs stepwise model selection by AIC.
library(MASS)
t1.glm.step <- stepAIC(t1.glm)
# Start:  AIC=38863.06
# factor(default) ~ balanceR + age + t30 + debtR + income + loans + 
#   t90 + mortgage + t60 + dependents
# 
# Df Deviance   AIC
# - balanceR    1    38841 38861
# <none>             38841 38863
# - loans       1    38844 38864
# - debtR       1    38847 38867
# - mortgage    1    38868 38888
# - dependents  1    38911 38931
# - income      1    38970 38990
# - t90         1    39314 39334
# - age         1    39400 39420
# - t30         1    39931 39951
# - t60         1    40309 40329
# 
# Step:  AIC=38861.44
# factor(default) ~ age + t30 + debtR + income + loans + t90 + 
#   mortgage + t60 + dependents
# 
# Df Deviance   AIC
# <none>             38841 38861
# - loans       1    38844 38862
# - debtR       1    38848 38866
# - mortgage    1    38868 38886
# - dependents  1    38912 38930
# - income      1    38970 38988
# - t90         1    39315 39333
# - age         1    39401 39419
# - t30         1    39932 39950
# - t60         1    40309 40327
t1.glm.step$ANOVA
# NULL # why?

# training error
table(t1$default, predict(t1.glm)>0)
# FALSE  TRUE
# 0 78250   164
# 1  5556   218

# prediction error
table(t2$default, predict(t1.glm, newdata = t2, type = "response")>0)
# TRUE
# 0 33498
# 1  2583

t1.glm1 <- glm(factor(default) ~ age + t30 + debtR + income + t90 + mortgage + t60 + dependents, data = t1, family = binomial)
summary(t1.glm1)

t1.glm.step1 <- stepAIC(t1.glm1)
t1.glm.step1$ANOVA

# training error
table(t1$default, predict(t1.glm1)>0)

# FALSE  TRUE
# 0 78250   164
# 1  5557   217

table(t2$default, predict(t1.glm1, newdata = t2, type = "response")>0)


#----------------------------------------------------------------
# linear discriminant analysis
#----------------------------------------------------------------
t1.lda <- lda(factor(default) ~ ., data = t1)
summary(t1.lda)

lda.test <- predict(t1.lda, newdata = t2, type = response)$class

# misclassification rate on the test data
# mis.lda.test <- sum(abs(as.numeric(lda.test) - 1 - t2$default))/36081
# [1] 0.07036945

table(t2$default, lda.test)
# lda.test
#       0     1
# 0 33304   194
# 1  2345   238

# precision = P(default | prediction default) = 33304/(33304+194) = 0.9942086
# recall = sensitivity = P(prediction default | default) = 33304/(33304+2345) = 0.9342198

.Last.value/dim(t2)[1]
# lda.test
#             0           1
# 0 0.923034284 0.005376791
# 1 0.064992655 0.006596270

# misclassification rate = round(0.005376791 + 0.064992655, 5) = 0.07037
#----------------------------------------------------------------
# smoothing spline
#----------------------------------------------------------------
library(mgcv)
t1.gam <- gam(factor(default) ~ s(balanceR) + s(age) + s(t30) + s(debtR) + s(income) + s(loans) + s(t90) + s(mortgage) + s(t60) + s(dependents), data = t1, family = binomial)
summary(t1.gam)

# doesn't work?
# gam.test <- predict(t1.gam, newdata = t2, type="response")$class

#----------------------------------------------------------------
# k-nearest-neighbor
#----------------------------------------------------------------
library(class)
# how to properly select k?
set.seed (1)
t1.knn <- knn(train = t1, test = t2, cl = factor(t1$default), k = 3)

table(t2$default, t1.knn)
# t1.knn
#       0     1
# 0 33244   254
# 1  2469   114
.Last.value/dim(t2)[1]
# t1.knn
#             0           1
# 0 0.921371359 0.007039716
# 1 0.068429367 0.003159558

# misclassification rate = round(0.007039716 + 0.068429367, 5) = 0.07547

#----------------------------------------------------------------
# Naive Bayes
#----------------------------------------------------------------
library(e1071)
t1.nBayes <- naiveBayes(factor(default) ~ ., data = t1)
summary(t1.nBayes)

nBayes.test <- predict(t1.nBayes, newdata = t2, type = "class")
table(t2$default, nBayes.test)
# nBayes.test
#       0     1
# 0 33403    95
# 1  2528    55
.Last.value/dim(t2)[1]
# nBayes.test
#             0           1
# 0 0.925778110 0.002632965
# 1 0.070064577 0.001524348

# misclassification rate = round(0.002632965 + 0.070064577, 5) = 0.0727

#----------------------------------------------------------------
# Decision Tree
#----------------------------------------------------------------
library(rpart)
set.seed(1)
tree.t1 <- rpart(factor(default) ~ ., data = t1, cp = 0.001)
summary(tree.t1)

tree.test <- predict(tree.t1, newdata = t2, type = "class")
table(t2$default, tree.test)
# tree.test
#       0     1
# 0 33191   307
# 1  2129   454
.Last.value/dim(t2)[1]
# tree.test
#             0           1
# 0 0.919902442 0.008508633
# 1 0.059006125 0.012582800

# misclassification rate = round(0.008508633 + 0.059006125, 5) = 0.06751

#----------------------------------------------------------------
# Random Forest
#----------------------------------------------------------------
library(randomForest)
set.seed (1)
rf.t1 <- randomForest(factor(default) ~ ., data = t1)

# Plot variable importance
importance(rf.t1)
varImpPlot(rf.t1)

rf.test <- predict(rf.t1, newdata = t2)
table(t2$default, rf.test)
#       0     1
# 0 33174   324
# 1  2128   455
.Last.value/dim(t2)[1]
# rf.test
#             0           1
# 0 0.919431280 0.008979795
# 1 0.058978410 0.012610515

# misclassification rate = round(0.008979795 + 0.058978410, 5) = 0.06796

#----------------------------------------------------------------
# Gradient Boosting
#----------------------------------------------------------------
library(gbm)
set.seed(1)
# A gradient boosted model with gaussian loss function.
# 5000 iterations were performed.
# interaction depth is the total splits we want to do.
# So here each tree is a small tree with only 4 splits.
boost.t1 <- gbm(factor(default) ~ ., data = t1, distribution = "gaussian", n.trees=5000, interaction.depth=4)

# The summary of the Model gives a feature importance plot.
# In the above list is on the top is the most important variable and 
# at last is the least important variable.
summary(boost.t1)
# var   rel.inf
# balanceR     balanceR 25.115956
# t90               t90 15.601753
# debtR           debtR 14.102289
# income         income 12.614034
# age               age  8.412283
# t30               t30  7.390833
# t60               t60  6.534672
# loans           loans  5.816142
# dependents dependents  2.542060
# mortgage     mortgage  1.869979

boost.pred <- predict(boost.t1, newdata = t2, n.trees=5000)
# mean((boost.pred - )^2)


#----------------------------------------------------------------
# Extreme Boosting
#----------------------------------------------------------------
