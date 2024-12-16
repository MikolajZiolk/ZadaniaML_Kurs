This repository conatain a sets of exercises completed as part of the course "Remote Repositories and Version Control Systems in Machine Learning Projects." 

**EXERCISES:**

1. Develop a model for forecasting O3 levels based on the airquality dataset. Then, use this model to make predictions for any other dataset. Check if months have an influence on the forecast. Remove missing data. Consider how to transform the variables. Do not include the variable day in the forecasts. Do not consider interactions between variables. Use functions from the GGally package to visualize the data. Apply only the method of least squares.
2. We will examine the mydata dataset from the openair package (Carslaw and Ropkins, 2012). Based on this dataset, we will attempt to build a classification model. The model will predict whether ozone concentration was high or low. Before defining what “high” and “low” mean, we will analyze the dataset.
3. Apply resampling methods (CV, V-fold CV, and bootstrap) to Exercise 2. Perform these procedures for a logistic regression model and a random forest. Analyze the results and write a few short conclusions.
4. & 5. Optimize the hyperparameters for the random forest model created in Exercise 3. Adjust the number of parameters in the hyperparameter grid.
6. Using the mydata dataset (1 year), propose a model to forecast O3 concentration levels (regression models). Apply three methods:
- Simple linear regression (glmnet),
- Decision trees (rpart),
- Random forest (ranger).
Key points:
- Transform wind direction into a categorical variable by defining 16 wind directions.
- Create a validation set without resampling.
- Prepare recipes for each model and verify their requirements.
- Perform hyperparameter optimization.
- Build the final models.
- Determine which model performed best.
- The best graphical method for comparing the evaluation results on the test set is a scatter plot with the ideal model line.
