#Marisa Acierno's Choose Your Own Project for Harvard edX Data Science Capstone
#install and load necessary packages as needed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "https://cran.us.r-project.org")
library(dplyr)
library(gghighlight)
library(tidyverse)
####Data Wrangling####
#Get the data from the UCI Machine Learning Repository
reds <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv')
whites <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv')
#Add new column designating type of white (i.e. white and red)
reds <- reds %>% mutate("type" = c("Red"))
whites <- whites %>% mutate("type" = c("White"))
#Merge the two data tables into one data set
wines <- data.table(merge(reds, whites, all = TRUE))
#Create train and test sets
set.seed(1)
test_index <- createDataPartition(wines$quality, times = 1, p = 0.5, list = FALSE)
train_set <- wines[-test_index,] %>% as.data.frame(.)
test_set <- wines[test_index] %>% as.data.frame(.)

####Data Exploration and Visualization####
train_set %>% ggplot(aes(quality)) + geom_histogram(color = "black", fill = "honeydew1", bins = 8) + 
  ggtitle("Wine Quality Frequency") + labs(x = "Wine Quality", y = "Frequency") + theme_bw()

train_set %>% group_by(pH) %>% summarise(m = mean(quality)) %>% ggplot(aes(pH, m)) + 
  geom_line(color = "deepskyblue1", size = 1.5) + geom_smooth() + theme_bw() + labs(title = "Average Wine Quality per pH")
train_set %>% group_by(alcohol) %>% summarise(m = mean(quality)) %>% ggplot(aes(alcohol, m)) + 
  geom_line(color = "aquamarine", size = 1.5) + geom_smooth() + theme_bw() + labs(title = "Average Wine Quality per Alcohol Level")
train_set %>% group_by(sulphates) %>% summarise(m = mean(quality)) %>% ggplot(aes(sulphates, m)) + 
  geom_line(color = "dodgerblue", size = 1.5) + geom_smooth() + theme_bw() + labs(title = "Average Wine Quality per Sulphates Level")

#M###Models####
##Model 1: Just the average
sd(train_set$quality)
mu <- mean(train_set$quality)
#determine the RMSE of the model
RMSE_mu <- RMSE(round(mu, digits=0), test_set$quality)
#determine the accuracy, i.e. how often does the predicted value = the test set value
acc_mu <- mean(round(mu, digits=0) == test_set$quality)

##Model 2: Linear regression
fit_lm <- lm(quality ~ ., data = train_set)
predict_lm <- predict(fit_lm, test_set) %>% round(., digits = 0)
RMSE_lm <- RMSE(predict_lm, test_set$quality)
acc_lm <- mean(predict_lm == test_set$quality)

##Model 3: knn model
#run the knn model with a range of differen k values to find best tune
set.seed(1)
fit_knn <- train(quality ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(1, 40, 2)))
fit_knn$results$k[which.min(fit_knn$results$RMSE)]
fit_knn$results %>% ggplot(aes(k, RMSE)) + geom_point(color = "red") + 
  labs(title = "Model 3: k-nearest neighbor", subtitle = "k-Values and Corresponding RMSE", x = "k-value", y = "RMSE") +
  gghighlight(k==29, unhighlighted_colour = "black", use_direct_label = T, label_key = k) + 
  theme_bw()
k = as.numeric(fit_knn$bestTune)
#create the model with k = the best tune value (29) and use this to generate predictions on the test set
fit_knn29 <- train(quality ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = 29))
predict_knn29 <- predict(fit_knn29, test_set) %>% round(., digits = 0)
RMSE_knn29 <- RMSE(predict_knn29, test_set$quality)
acc_knn29 <- mean(predict_knn29 == test_set$quality)

##Model 4: Random forest - FYI this takes awhile
fit_rf <- train(quality ~ ., method = "rf", data = train_set)
predict_rf <- predict(fit_rf, test_set) %>% round(., digits = 0)
RMSE_rf <- RMSE(predict_rf, test_set$quality)
acc_rf <- mean(predict_rf == test_set$quality)

fit_rf$results

####Results####
#Graph distribution of predicted vs. actual quality
prediction <- data.frame("predict_best" = predict_rf) %>% mutate("true_quality" = as.numeric(test_set$quality))
q <- seq(3,9,1)
freq_bestmodel <- sapply(q, function(q){
  model <- sum(prediction$predict_best == q)})
freq_true <- sapply(q, function(q){
  true <- sum(test_set$quality == q)})
freq <- data.frame("q" = q, "best_model" = freq_bestmodel, "true" = freq_true)  
t <- data.frame("q" = c(q,q), "freq" = c(freq_bestmodel, freq_true), 
                "Legend" = c("Predicted","Predicted","Predicted","Predicted","Predicted","Predicted","Predicted","Actual","Actual","Actual","Actual","Actual","Actual","Actual"))
t %>% ggplot(aes(q, freq)) + geom_line(aes(color = Legend), size = 1.5) + 
    labs(title = "Frequencies of Wine Quality: Actual vs. Predicted", x = "Wine Quality Score", y = "Frequency")

#standard deviation and mean of predicted vs. actual quality
p_sd <- sd(prediction$predict_best) %>% round(digits = 3)  
t_sd <- sd(prediction$true_quality) %>% round(digits = 3)
p_avg <- mean(prediction$predict_best) %>% round(digits = 3) 
t_avg <- mean(prediction$true_quality) %>% round(digits = 3)
compare_avg <- data.frame("Predicted Mean" = p_avg, "True Mean" = t_avg)
compare_sd <- data.frame("Predicted SD" = p_sd, "True SD" = t_sd)

#How often is the prediction within one point of the true value?
within_one <- ifelse(abs(prediction$predict_best - prediction$true_quality) <= 1, "0", "1") %>% as.numeric(.)
1 - (sum(within_one)/length(within_one))
#I got the right quality score or was within one point in either direction 97% of the time
#Given how personal wine tastings and perceptions of quality are, I would consider this a successful model to inform wine choices

