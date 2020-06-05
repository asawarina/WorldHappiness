install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("rpart")
library(rpart)

train <- read.csv("C:/Users/asawari/Desktop/WorldHappiness/train_data.csv")
test <- read.csv("C:/Users/asawari/Desktop/WorldHappiness/test_data.csv")

#LINEAR REGRESSION MODEL

#First initial run with all variables
linreg_fit1 <- lm(happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+generosity+freedom+trust_government,data=train)
summary(linreg_fit1)

#checking multi-collinearity before removing any variable
rsq_fit1 = summary(linreg_fit1)$r.squared
vif_fit1 = 1/(1 - rsq_fit1)
vif_fit1

#predicting score for first run
predict_score_linreg_fit1 <- predict(linreg_fit1, newdata = test)
predict_score_linreg_fit1

#calculating RMSE
linreg_rmse_1 <- lm(test$happiness_score,predict_score_linreg_fit1)
linreg_rmse_1


#Second Run: running linear regression again without Generosity variable
linreg_fit2 <- lm(happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+freedom+trust_government,data=train)
summary(linreg_fit2)

#checking multi-multicollinearity before removing any variable
rsq_fit2 = summary(linreg_fit2)$r.squared
vif_fit2 = 1/(1 - rsq_fit2)
vif_fit2
##RESULT: will be choosing the second linear model run

#calculating RMSE for second model run
linreg_rmse <- rmse(test$happiness_score,predict_score_linreg)
linreg_rmse



#running linear regression again without Generosity & trust in Government variable
##RESULT: NOT much difference between R-squared values
linreg_fit3 <- lm(happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+freedom,data=train)
summary(linreg_fit3)

#checking multi-collinearity
rsq_fit3= summary(linreg_fit3)$r.squared
vif_fit3 = 1/(1 - rsq_fit3)
vif_fit3


#predicting score for selected model Second Run
predict_score_linreg <- predict(linreg_fit2, newdata = test)
predict_score_linreg

compare_values_linreg<- as.data.frame(cbind(Predicted = predict_score_linreg, Actual = test$happiness_score))
compare_values_linreg

#plotting second model run
linreg_plot <- ggplot(compare_values_linreg,aes(Actual, Predicted))+geom_point()+geom_abline() +labs(title = "Mutliple Linear Regression", x = "Actual happiness score", y = "Predicted happiness score")
linreg_plot

#calculating RMSE for second model run
linreg_rmse <- rmse(test$happiness_score,predict_score_linreg)
linreg_rmse




#  DECISION TREES: decision_trees.R

install.packages("rpart")
library(rpart)
library(rpart.plot)
install.packages("dplyr")
library(dplyr)
library(tidyverse)


setwd("C:/Users/mehtaa4/Desktop/World Happiness R Project")
train <- read.csv("train_data.csv", header = TRUE, sep = ",")
test <- read.csv("test_data.csv",header = TRUE, sep = ",")

decision_tree_fit <- rpart(happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+generosity+freedom+trust_government,data=train,control=rpart.control(minsplit=10))
summary(decision_tree_fit)

prp(decision_tree_fit)

predicted_dt <- predict(decision_tree_fit,newdata = test)
compare_values_dt<- as.data.frame(cbind(Predicted = predicted_dt, Actual = test$happiness_score))
dec_tree <- ggplot(compare_values_dt,aes(Actual, Predicted))+geom_point()+geom_abline() +labs(title = "Decision Tree", x = "Actual happiness score", y = "Predicted happiness score")

dec_tree



#  SUPPORT VECTOR REGRESSION: support_vector_regression.R

install.packages("e1071")
library(e1071)

install.packages("Metrics")
library(Metrics)

setwd("C:/Users/mehtaa4/Desktop/World Happiness R Project")
getwd()

train <- read.csv("train_data.csv", header = TRUE, sep = ",")
test <- read.csv("test_data.csv",header = TRUE, sep = ",")

#model run using all variables
regressor_svr_fit = svm(formula = happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+generosity+freedom+trust_government,data = train,type = 'eps-regression',kernel = 'radial')
summary(regressor_svr_fit)

# Predicting a new result
predict_score_svr = predict(regressor_svr_fit, newdata = test)
predict_score_svr

compare_values_svr <- as.data.frame(cbind(Prediction = predict_score_svr, Actual = test$happiness_score))
compare_values_svr

#Pred_Actual_lm.versus.svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$Happiness.Score)


svr_plot <- ggplot(compare_values_svr, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
svr_plot


#error <- test$happiness_score - predict_score_svr
svr_rmse <- rmse(test$happiness_score,predict_score_svr)
svr_rmse


#model run without generosity
#model run using all variables
regressor_svr_fit2 = svm(formula = happiness_score ~ economy_gdp_per_capita+family+health_life_expectancy+freedom+trust_government,data = train,type = 'eps-regression',kernel = 'radial')
summary(regressor_svr_fit2)

# Predicting a new result
predict_score_svr_2 = predict(regressor_svr_fit2, newdata = test)
predict_score_svr_2

#calculating rmse
svr_rmse2 <- rmse(test$happiness_score,predict_score_svr_2)
svr_rmse2

compare_values_svr_2 <- as.data.frame(cbind(Prediction = predict_score_svr_2, Actual = test$happiness_score))
compare_values_svr_2

svr_plot_2 <- ggplot(compare_values_svr_2, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
svr_plot_2






