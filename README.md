# MetroTransitPrediction
Metro Transit Project: R code for PBD model training

## Introduction
This is a project instructed by Prof. Qie He in ISyE, aiming to help Metro Transit, the largest public transportation company in Twin Cities, to optimize their workforce 
management. My duty includes building day-ahead and season-ahead statistical prediction model for absenteeism of bus operators. The main challenge of this project is the 
incompatibility between response and features: individual-level associations are inferred from labeled data available only at the aggregate level. Regular predictive statistical 
methods (such as logistic regression, SVM etc.) are not suitable for the application directly. 

We followed the idea from Poisson Binomial Distribution (PBD) model with normal 
proximation technique, and implemented a novel algorithm by BFGS. The cross validation based on historical data shows it has the superior performance among traditional methods 
with feature engineering, including  GLM ,SVM, Tree, RandomForest and Spline. The results receive very good feedback, and are considered valuable for bus operation's schedule 
and strategy design as well.

This folder contains the R code for training the PBD model. Due to the data privacy regulation of Metro Transit, no data information will be disclosed in the code. 

## PBD Model
The PBD model is a noval creation based on PBD distribution in GLM framework. It describes the distribution of aggregated binary responses, for which is linked to multiple predictors via
a linear link function (i.e. logistic link). We also use normal approximation techniques to approach the true distribution. After setting up the MLE algorithm framework for the distribution,
we utilized BFGS and Newton's method to compute the estimator. More details could be found in the [PBD Vignette]https://github.com/TYtianyang/MetroTransitPrediction/blob/master/PBD.pdf[/embed].
