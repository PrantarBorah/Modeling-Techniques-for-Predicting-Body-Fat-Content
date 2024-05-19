# Modeling-Techniques-for-Predicting-Body-Fat-Content

# Data Preparation and Cleaning:

Multiple datasets were read and relevant variables were selected.

Missing values were omitted to ensure clean data.

Gender and snoring variables were recoded for better interpretation.

**Variable Selection**:

Best subsets regression identified key variables: age, gender, sleep, snore, weight, BMI, and waist circumference.

Both forward and backward selection methods agreed on these seven variables.

# Model Fitting

Multiple linear regression models were fitted using the selected variables.

Model diagnostics indicated issues with normality and constant variance of residuals.

**Box-Cox Transformation**:

Applied to address non-normality and non-constant variance of errors.

Improved the model fit and residual diagnostics.

**Ridge Regression**:

Performed to address multicollinearity.

Identified optimal lambda through cross-validation.

Ridge regression coefficients were computed.

# Model Evaluation:

Both Box-Cox transformed model and Ridge regression model were evaluated.

Ridge regression showed better performance in terms of error metrics and residual analysis.

# Conclusions:

The combination of Box-Cox transformation and Ridge regression provided the best predictive model for visceral adipose tissue mass (VAT).

Key predictors of VAT were age, gender, sleep duration, snoring frequency, weight, BMI, and waist circumference.

Addressing normality and collinearity issues significantly improved model performance.
