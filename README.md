# CS_project for Scotiabank Interview

- Overall train of thought: 
  1) Fit the (scaled) data to an array of methods on the training set;
  2) choose the best model with the lowest validation error on the validation set;
  3) tune hyperparameters for the chosen model from step 2 on the validation set, and 
  4) finally, run all the methods on the test set and obtain the test error, recall and precision; and select the best model.
  Precision in this context is particularly impoIrtant. 

- Before any modelling, first inspect the features in the following ways:
  - Range and outliers (histogram or boxplot)
  - Missing data: The dataset contains about 20% missing data if deleting rows with NA. Since the number is significant, I imputed the missing data, see R code for details, before any scaling.
  - Correlations among different features
  - Feature scaling of feature X: (X-mean)/std for age and (X - min(X))/(max(X) - min(X)) for the rest of features. Since some of the outliers has relative large value, will set them to be 1 and will not include them into the denominator calculations, see R code for details.
  - Binning: lead to loss of information, not considered here
 
- Crude methods: include all major machine learning methods.
  1) logistic regression
  2) linear discriminant analysis (LDA)
  3) k-nearest-neighbor
  4) Naive Bayes
  5) Decision Tree
  6) Random Forest
  7) Support Vector Machine (SVM)
  
  Comments: 
  
  - Hyperparameter tuning: Due to time constraints, I won't tune hyperparameters in all models, and only tune that with the lowest validation error. For the rest of the models, I use the most commonly/popular/default used values for hyperparameter. 
  - Due to time constraints, only considered linear SVM; should have considered Gaussian SVM as well if time allowed. 
  - Left out: gradient boosting (half finished under comments in R), flexible discrimant analysis, bagging, MARS among others due to time constraints.
  
- Data spliting: 
  - Split the given training set according to 70/20/10 for training, validation and test sets.
  - Initially, I split the set by 50/25/25; but changed my mind due to fear that the training set is too small to train adaquate model.

- Feature selection: 
  - There are 10 initial features. Since 3 features are highly correlatd, I removed 2 from the model, see comments in R code.
  - Concerns over number of features: since the number of features is relatively small, it runs of the risk of underfitting. So it's best to compare the training errors and validation errors - if both errors are high, it suggest underfitting; if training error is low but validation error is high, then it suggests overfitting. 
  
  
  

  
