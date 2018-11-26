# CS_project for Scotiabank Interview

- Overall train of thought: 
  1) Fit the (scaled) data to an array of methods on the training set;
  2) choose the best model with the lowest validation error on the validation set;
  3) tune the hyperparameters for the chosen model on the validation set, and 
  4) finally, run all the methods on the test set and obtain the test error, recall and precision. 
  Precision in this context is particularly important. 

- Before any modelling, first inspect the features in the following ways:
  1) range and outliers (histogram or boxplot)
  2) correlations among different features
  3) feature scaling of feature X, i.e. (X-mean)/std. Since some of the outliers has relative large value, will set them to be        1 and will not include them into the standard deviation calculation.
 
  
