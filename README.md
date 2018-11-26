# CS_project for Scotiabank Interview

- Overall train of thought: 
  1) Fit the (scaled) data to an array of methods on the training set;
  2) choose the best model with the lowest validation error on the validation set;
  3) tune hyperparameters for the chosen model from step 2 on the validation set, and 
  4) finally, run all the methods on the test set and obtain the test error, recall and precision; and select the best model.
  Precision in this context is particularly important. 

- Before any modelling, first inspect the features in the following ways:
  1) range and outliers (histogram or boxplot)
  2) correlations among different features
  3) feature scaling of feature X, i.e. (X-mean)/std. Since some of the outliers has relative large value, will set them to be        1 and will not include them into the standard deviation calculation, see R code for details.
 
- Crude methods: include all major machine learning methods.
  1) logistic regression
  2) linear discriminant analysis (LDA)
  3) k-nearest-neighbor
  4) Naive Bayes
  5) Decision Tree
  6) Random Forest
  7) Support Vector Machine (SVM)
  8) Gradient Boosting
  
  Comments: 
  
  - There're three major boosting methods: AdaBoost, Gradient Boosting, and XGBoost. I chose Gradient boost because it's most representative one compared to the other two. AdaBoost is considered to be a primative version of Gradient Boost; and I ran out of time to do XGBoost.
  - Due to time constraints, I won't run both Gaussian SVM and linear SVM. 
  - Hyperparameter tuning: Due to time constraints, I won't tune hyperparameters in all models, and only tune that with the lowest validation error. For the rest of the models, I use the most commonly/popular/default used values for hyperparameter. 
  - Left out: flexible discrimant analysis, bagging, MARS among others due to time constraints.
  
- Data spliting: 
  - Split the given training set according to 70/20/10 for training, validation and test sets.
  - Initially, I split the set by 50/25/25; but changed my mind due to fear that the training set is too small to train adaquate model.

- Feature selection: 
  - There are 10 initial features. Since 3 features are highly correlatd, I removed 2 from the model, see comments in R code.
  - Concerns over number of features: since the number of features is relatively small, it runs of the risk of underfitting. So it's best to compare the training errors and validation errors - if both errors are high, it suggest underfitting; if training error is low but validation error is high, then it suggests overfitting. 
  
  
  

  
