# Import data to R
train <- read.csv("cs-training.csv")
test <- read.csv("cs-test.csv")

# Rename column names
colnames(train) <- c("ID", "delin90", "balanceR", "age", "t30", "debtR", "income", "loans", "t90", "mortgage", "t60", "dependents")
colnames(test) <- c("ID", "delin90", "balanceR", "age", "t30", "debtR", "income", "loans", "t90", "mortgage", "t60", "dependents")

# Display the structure of the dataset
str(train)
str(test)
#----------------------------------------------------------------
# missing data imputation
#----------------------------------------------------------------

# There are missing data in explanatory variables: income and dependents 

# Impute missing income with the median income
train$income[is.na(train$income)] <- median(train$income[!is.na(train$income)])
test$income[is.na(test$income)] <- median(test$income[!is.na(test$income)])

# Impute dependents with most frequent element 
# The most frequent number of dependents is 0
sort(table(train$dependents),decreasing=TRUE)[1]
#     0 
# 86902 
train$dependents[is.na(train$dependents)] <- 0

sort(table(test$dependents),decreasing=TRUE)[1]
#     0 
# 58618 
test$dependents[is.na(test$dependents)] <- 0

#----------------------------------------------------------------
# Split training dataset into training, validation and test sets
#----------------------------------------------------------------

set.seed(1)
ind <- sample(nrow(train), nrow(train))
tr <- train[ind[1:75000], -1] # training set
va <- train[ind[75001:112500], -1] # validation set
te <- train[ind[112501:150000], -1] # test set


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
# Collinearity checks
#----------------------------------------------------------------

# Check collinearity among covariates in the training set
#--------------------
round(cor(tr), 3)
#            delin90 balanceR    age    t30  debtR income  loans    t90 mortgage    t60 dependents
# delin90      1.000   -0.002 -0.114  0.133 -0.008 -0.018 -0.027  0.126   -0.005  0.112      0.048
# balanceR    -0.002    1.000 -0.002 -0.001  0.003  0.004 -0.012 -0.001    0.006 -0.001      0.000
# age         -0.114   -0.002  1.000 -0.063  0.022  0.035  0.148 -0.060    0.032 -0.057     -0.214
# t30          0.133   -0.001 -0.063  1.000 -0.006 -0.011 -0.058  0.985   -0.031  0.988     -0.004
# debtR       -0.008    0.003  0.022 -0.006  1.000 -0.017  0.041 -0.007    0.096 -0.007     -0.036
# income      -0.018    0.004  0.035 -0.011 -0.017  1.000  0.097 -0.013    0.130 -0.012      0.071
# loans       -0.027   -0.012  0.148 -0.058  0.041  0.097  1.000 -0.082    0.432 -0.074      0.074
# t90          0.126   -0.001 -0.060  0.985 -0.007 -0.013 -0.082  1.000   -0.045  0.993     -0.010
# mortgage    -0.005    0.006  0.032 -0.031  0.096  0.130  0.432 -0.045    1.000 -0.040      0.126
# t60          0.112   -0.001 -0.057  0.988 -0.007 -0.012 -0.074  0.993   -0.040  1.000     -0.011
# dependents   0.048    0.000 -0.214 -0.004 -0.036  0.071  0.074 -0.010    0.126 -0.011      1.000


# Covariates t30, t60 and t90 are highly correlated
# Since t30 and response has the largest correlation, 0.133 compared to 0.112 and 0.126,
# we'll keep t30 into the model and not inlclude the other two covariates, t60 and t90

# New training data excluding covariates t60 and t90
newTr <- tr[,c(-8, -10)]

# Double check correlation of the new training set
round(cor(newTr), 3)
#            delin90 balanceR    age    t30  debtR income  loans mortgage dependents
# delin90      1.000   -0.002 -0.114  0.133 -0.008 -0.018 -0.027   -0.005      0.048
# balanceR    -0.002    1.000 -0.002 -0.001  0.003  0.004 -0.012    0.006      0.000
# age         -0.114   -0.002  1.000 -0.063  0.022  0.035  0.148    0.032     -0.214
# t30          0.133   -0.001 -0.063  1.000 -0.006 -0.011 -0.058   -0.031     -0.004
# debtR       -0.008    0.003  0.022 -0.006  1.000 -0.017  0.041    0.096     -0.036
# income      -0.018    0.004  0.035 -0.011 -0.017  1.000  0.097    0.130      0.071
# loans       -0.027   -0.012  0.148 -0.058  0.041  0.097  1.000    0.432      0.074
# mortgage    -0.005    0.006  0.032 -0.031  0.096  0.130  0.432    1.000      0.126
# dependents   0.048    0.000 -0.214 -0.004 -0.036  0.071  0.074    0.126      1.000

#----------------------------------------------------------------
# logistic regresison
#----------------------------------------------------------------

# Run logistic regresson on the new training dataset
# covariates balanceR and loans are insignificant - should delete them from the final model
glm.newTr <- glm(factor(delin90) ~ ., family = binomial, data = newTr)
summary(glm.newTr)

# Call:
#   glm(formula = factor(delin90) ~ ., family = binomial, data = newTr)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0832  -0.4144  -0.3378  -0.2746   4.8933  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.154e+00  5.757e-02 -20.049  < 2e-16 ***
#   balanceR    -3.209e-05  9.022e-05  -0.356   0.7221    
#   age         -2.945e-02  1.144e-03 -25.745  < 2e-16 ***
#   t30          3.899e-02  2.235e-03  17.448  < 2e-16 ***
#   debtR       -3.117e-05  1.492e-05  -2.089   0.0367 *  
#   income      -3.649e-05  4.400e-06  -8.292  < 2e-16 ***
#   loans        6.316e-04  3.412e-03   0.185   0.8531    
#   mortgage     5.985e-02  1.423e-02   4.205 2.61e-05 ***
#   dependents   1.191e-01  1.248e-02   9.546  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 36697  on 74999  degrees of freedom
# Residual deviance: 35101  on 74991  degrees of freedom
# AIC: 35119
# 
# Number of Fisher Scoring iterations: 6


# Check collinearity on the new training data
vif(glm.newTr)
# balanceR        age        t30      debtR     income      loans   mortgage dependents 
# 1.000521   1.101507   1.011154   1.058354   1.211233   1.343081   1.392122   1.068982 

# Perform stepwise model selection
# results agree with my suspision 
# final model contains 6 covariates: age, debtR, income, t90, mortgage, and dependents
step.newTr <- step(glm.newTr)
# Start:  AIC=35119.06
# factor(delin90) ~ balanceR + age + t30 + debtR + income + loans + 
#   mortgage + dependents
# 
# Df Deviance   AIC
# - loans       1    35101 35117
# - balanceR    1    35101 35117
# <none>             35101 35119
# - debtR       1    35106 35122
# - mortgage    1    35118 35134
# - income      1    35185 35201
# - dependents  1    35188 35204
# - t30         1    35521 35537
# - age         1    35808 35824
# 
# Step:  AIC=35117.09
# factor(delin90) ~ balanceR + age + t30 + debtR + income + mortgage + 
#   dependents
# 
# Df Deviance   AIC
# - balanceR    1    35101 35115
# <none>             35101 35117
# - debtR       1    35106 35120
# - mortgage    1    35121 35135
# - income      1    35185 35199
# - dependents  1    35188 35202
# - t30         1    35524 35538
# - age         1    35826 35840
# 
# Step:  AIC=35115.24
# factor(delin90) ~ age + t30 + debtR + income + mortgage + dependents
# 
# Df Deviance   AIC
# <none>             35101 35115
# - debtR       1    35107 35119
# - mortgage    1    35121 35133
# - income      1    35186 35198
# - dependents  1    35188 35200
# - t30         1    35524 35536
# - age         1    35826 35838

## FINAL LOGISTIC MODEL WITH COVARIATES
## age, debtR, income, t30, mortgage, dependents
glm.final <- glm(factor(delin90) ~ age + debtR + income + t30 + mortgage + dependents, family = binomial, data = newTr)
summary(glm.final)

# Call:
#   glm(formula = factor(delin90) ~ age + debtR + income + t30 + 
#         mortgage + dependents, family = binomial, data = newTr)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0830  -0.4144  -0.3378  -0.2745   4.8904  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.153e+00  5.683e-02 -20.283  < 2e-16 ***
#   age         -2.941e-02  1.124e-03 -26.161  < 2e-16 ***
#   debtR       -3.117e-05  1.492e-05  -2.089   0.0367 *  
#   income      -3.642e-05  4.373e-06  -8.328  < 2e-16 ***
#   t30          3.896e-02  2.228e-03  17.484  < 2e-16 ***
#   mortgage     6.083e-02  1.317e-02   4.618 3.87e-06 ***
#   dependents   1.192e-01  1.246e-02   9.567  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 36697  on 74999  degrees of freedom
# Residual deviance: 35101  on 74993  degrees of freedom
# AIC: 35115
# 
# Number of Fisher Scoring iterations: 6

# Consider perfom woe on covariates, will skip this time
# library(woe)
# woemodel <- woe(Data = newTrain, "age", TRUE, "default", 5, Bad = 0, Good = 1)

# Run the final model on testing set, te
glm.prob <- predict(glm.final, newdata = te, type = "response")
# ROC curve
library(pROC)
glm.roc <- roc(factor(delin90) ~ glm.prob, data = te)
plot(glm.roc)

table(te$delin90, glm.prob > 0)
#   TRUE
# 0 35005
# 1  2495

# Probability on the real test set
glm.pred <- predict(glm.final, newdata = test, type = "response")

#----------------------------------------------------------------
# linear discriminant analysis
#----------------------------------------------------------------
# Don't need to tune hyperparameter in this case

library(MASS)
lda.tr <- lda(factor(delin90) ~ ., data = newTr)
lda.tr
# Call:
#   lda(factor(delin90) ~ ., data = newTr)
# 
# Prior probabilities of groups:
#   0       1 
# 0.93344 0.06656 
# 
# Group means:
#   balanceR      age       t30    debtR   income    loans  mortgage dependents
# 0 5.699010 52.83956 0.2783825 364.4883 6433.843 8.490658 1.0191121  0.7160896
# 1 4.306585 46.05028 2.6067708 289.4354 5617.839 7.935897 0.9959936  0.9294872
# 
# Coefficients of linear discriminants:
#   LD1
# balanceR   -4.760292e-05
# age        -3.910581e-02
# t30         1.708365e-01
# debtR      -9.360063e-06
# income     -7.892434e-06
# loans      -8.078000e-03
# mortgage    2.028115e-02
# dependents  1.512661e-01

lda.prob <- predict(lda.tr, newdata = te, type = response)
lda.tab <- table(lda.prob$class, te$delin90)

library(caret)
confusionMatrix(lda.tab)
# Confusion Matrix and Statistics
# 
# 
#       0     1
# 0 34972  2464
# 1    33    31
# 
# Accuracy : 0.9334          
# 95% CI : (0.9308, 0.9359)
# No Information Rate : 0.9335          
# P-Value [Acc > NIR] : 0.5218          
# 
# Kappa : 0.021           
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.99906         
#             Specificity : 0.01242         
#          Pos Pred Value : 0.93418         
#          Neg Pred Value : 0.48438         
#              Prevalence : 0.93347         
#          Detection Rate : 0.93259         
#    Detection Prevalence : 0.99829         
#       Balanced Accuracy : 0.50574         
#                                           
#        'Positive' Class : 0          


# Probability on the real test set
lda.pred <- predict(lda.tr, newdata = test, type = response)$x

#----------------------------------------------------------------
# smoothing spline
#----------------------------------------------------------------
# library(mgcv)
# t1.gam <- gam(factor(default) ~ s(balanceR) + s(age) + s(t30) + s(debtR) + s(income) + s(loans) + s(t90) + s(mortgage) + s(t60) + s(dependents), data = t1, family = binomial)
# summary(t1.gam)

# doesn't work?
# gam.test <- predict(t1.gam, newdata = t2, type="response")$class

#----------------------------------------------------------------
# k-nearest-neighbor
#----------------------------------------------------------------
library(class)
# how to properly select k?
set.seed (1)
knn.tr <- knn(train = newTr, test = te[,c(-8, -10)], cl = factor(newTr$delin90), k = 3)

knn.tab <- table(knn.tr, te$delin90)
confusionMatrix(knn.tab)
# Confusion Matrix and Statistics
# 
# 
# knn.tr     0     1
#      0 34713  2383
#      1   292   112
# 
# Accuracy : 0.9287         
# 95% CI : (0.926, 0.9313)
# No Information Rate : 0.9335         
# P-Value [Acc > NIR] : 0.9999         
# 
# Kappa : 0.0598         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.99166        
# Specificity : 0.04489        
# Pos Pred Value : 0.93576        
# Neg Pred Value : 0.27723        
# Prevalence : 0.93347        
# Detection Rate : 0.92568        
# Detection Prevalence : 0.98923        
# Balanced Accuracy : 0.51827        
# 
# 'Positive' Class : 0    

knn.roc <- roc(factor(delin90) ~ knn.prob, data = te)
plot(knn.roc)

#----------------------------------------------------------------
# Naive Bayes
#----------------------------------------------------------------
library(e1071)
nBayes.tr <- naiveBayes(factor(delin90) ~ ., data = newTr)
nBayes.tr

nBayes.prob <- predict(nBayes.tr, newdata = te, type = "class")
nBayes.tab <- table(nBayes.prob, te$delin90)
confusionMatrix(nBayes.tab)
# Confusion Matrix and Statistics
# 
# 
# nBayes.prob     0     1
#           0 34629  2372
#           1   376   123
# 
# Accuracy : 0.9267         
# 95% CI : (0.924, 0.9293)
# No Information Rate : 0.9335         
# P-Value [Acc > NIR] : 1              
# 
# Kappa : 0.0613         
# Mcnemar's Test P-Value : <2e-16         
# 
# Sensitivity : 0.9893         
# Specificity : 0.0493         
# Pos Pred Value : 0.9359         
# Neg Pred Value : 0.2465         
# Prevalence : 0.9335         
# Detection Rate : 0.9234         
# Detection Prevalence : 0.9867         
# Balanced Accuracy : 0.5193         
# 
# 'Positive' Class : 0     

#----------------------------------------------------------------
# Decision Tree
#----------------------------------------------------------------
library(rpart)
set.seed(1)
# cp: complexity parameter - need to tune
tree.tr <- rpart(factor(delin90) ~ ., data = newTr, cp = 0.001)
summary(tree.tr)

tree.prob <- predict(tree.tr, newdata = te, type = "class")
tree.tab <- table(tree.prob, te$delin90)
confusionMatrix(tree.tab)
# Confusion Matrix and Statistics
# 
# 
# tree.prob     0     1
#         0 34856  2331
#         1   149   164
# 
# Accuracy : 0.9339          
# 95% CI : (0.9313, 0.9364)
# No Information Rate : 0.9335          
# P-Value [Acc > NIR] : 0.383           
# 
# Kappa : 0.1035          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.99574         
# Specificity : 0.06573         
# Pos Pred Value : 0.93732         
# Neg Pred Value : 0.52396         
# Prevalence : 0.93347         
# Detection Rate : 0.92949         
# Detection Prevalence : 0.99165         
# Balanced Accuracy : 0.53074         
# 
# 'Positive' Class : 0     
#----------------------------------------------------------------
# Random Forest
#----------------------------------------------------------------
library(randomForest)
set.seed (1)
rf.tr <- randomForest(factor(default) ~ ., data = newTr)

# Plot variable importance
importance(rf.tr)
varImpPlot(rf.tr)

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
# XGBoost  
#----------------------------------------------------------------
library(xgboost)
