library(readr)

set.seed(1)

Speed_Dating_Data <- read_csv("~/Cornell Junior/Spring 2017/ORIE 4740/Final Project/Speed Dating Data.csv")
# View(Speed_Dating_Data)

columns_kept <- c("gender", "match", "int_corr", "samerace", "age_o", "race_o", "race", "imprace")
filtered_speed_dating <- Speed_Dating_Data[, columns_kept]


sum(is.na(filtered_speed_dating))
filtered_speed_dating <- filtered_speed_dating[complete.cases(filtered_speed_dating), ]
# filtered_speed_dating[filtered_speed_dating[,""]]
sum(is.na(filtered_speed_dating))

# Check what fraction of couples actually matched
sum(filtered_speed_dating$match == 1) / nrow(filtered_speed_dating)

sapply(filtered_speed_dating, class)
filtered_speed_dating$race_o <- as.factor(filtered_speed_dating$race_o)
filtered_speed_dating$race <- as.factor(filtered_speed_dating$race)
sapply(filtered_speed_dating, class)
levels(filtered_speed_dating$race_o)
levels(filtered_speed_dating$race)


# Validation on 20% of the data
threshold <- 0.5
train_ind <- sample(1:nrow(filtered_speed_dating), .8*nrow(filtered_speed_dating))
train_set <- filtered_speed_dating[train_ind, ]
test_set <- filtered_speed_dating[-train_ind, ]
log_reg_model <- glm(match~., data = train_set, family = binomial)
summary(log_reg_model)

train_preds <- predict(log_reg_model, train_set, type="response")
train_bin_preds <- as.numeric(train_preds >= threshold)
train_accuracy <- mean(train_bin_preds==train_set["match"])
train_accuracy
table(train_bin_preds, train_set$match)


test_preds <- predict(log_reg_model, test_set, type="response")
test_bin_preds <- as.numeric(test_preds >= threshold)
test_accuracy <- mean(test_bin_preds==test_set["match"])
test_accuracy
table(test_bin_preds, test_set$match)


library(pROC)
g <- roc(match ~ test_preds, data=test_set)
plot(g)





# Cross Validation for different threshold values
thresholds <- seq(0, 1, 0.05)
cv.error = rep(0, length(thresholds))

for (i in c(0:length((thresholds)))) {
  threshold <- thresholds[i]
  glm.fit = glm(match~., data = filtered_speed_dating, family = binomial)
  cost_fcn = function (observed, predicted) {
    bin_preds <- as.numeric(predicted >= threshold)
    return(mean(bin_preds!=observed))
  }
  cv.error[i] = cv.glm(filtered_speed_dating, glm.fit, cost=cost_fcn, K=10)$delta[1]
}
best_threshold <- thresholds[which.min(cv.error)]
best_accuracy <- 1 - cv.error[which.min(cv.error)]
cv.error
best_threshold
best_accuracy


