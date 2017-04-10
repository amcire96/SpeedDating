library(readr)

set.seed(1)

Speed_Dating_Data <- read_csv("~/Cornell Junior/Spring 2017/ORIE 4740/Final Project/Speed Dating Data.csv")
# View(Speed_Dating_Data)

# columns_kept <- c("gender", "match", "int_corr", "samerace", "age_o", "race_o", "race", "imprace")
columns_kept <- c("gender", "match", "int_corr", "samerace", "age_o", "race_o", "race", "imprace",
                  "dec_o", "goal", "date", "go_out")
test_speed_dating <- Speed_Dating_Data[, columns_kept]


sum(is.na(test_speed_dating))
test_speed_dating <- test_speed_dating[complete.cases(test_speed_dating), ]
dim(test_speed_dating)
test_speed_dating = test_speed_dating[test_speed_dating[,"dec_o"] == 1,!names(test_speed_dating) %in% 'dec_o']
dim(test_speed_dating)
sum(is.na(test_speed_dating))

# Check what fraction of couples actually matched
sum(test_speed_dating$match == 1) / nrow(test_speed_dating)

sapply(test_speed_dating, class)
test_speed_dating$race_o <- as.factor(test_speed_dating$race_o)
test_speed_dating$race <- as.factor(test_speed_dating$race)
test_speed_dating$goal <- as.factor(test_speed_dating$goal)
sapply(test_speed_dating, class)
levels(test_speed_dating$race_o)
levels(test_speed_dating$race)
levels(test_speed_dating$goal)


# Validation on 20% of the data
threshold <- 0.45
train_ind <- sample(1:nrow(test_speed_dating), .8*nrow(test_speed_dating))
train_set <- test_speed_dating[train_ind, ]
test_set <- test_speed_dating[-train_ind, ]
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
avg_accuracies <- rep(0,length(seq(0, 1, 0.05)))
thresholds <- seq(0, 1, 0.05)
for (threshold_index in (1:length(seq(0, 1, 0.05)))) {
  test_speed_dating <- test_speed_dating[sample(nrow(test_speed_dating)),]
  folds <- cut(seq(1, nrow(test_speed_dating)), breaks=10, labels=FALSE)
  threshold <- thresholds[threshold_index]
  print(threshold)
  avg_acc <- 0
  for (i in (1:10)) {
    testIndices <- which(folds==i, arr.ind=TRUE)
    test_set <- test_speed_dating[testIndices,]
    train_set <- test_speed_dating[-testIndices,]
    
    log_reg_model <- glm(match~., data = train_set, family = binomial)
    preds <- predict(log_reg_model, test_set, type = "response")
    # summary(log_reg_model)
    
    
    bin_preds <- as.numeric(preds >= threshold)
    avg_acc = avg_acc + mean(bin_preds==test_set$match)
    # table(bin_preds, test_set$match)
  }
  avg_acc <- avg_acc / 10
  avg_accuracies[threshold_index] = avg_acc
}
best_threshold <- thresholds[which.max(avg_accuracies)]
avg_accuracies
best_threshold




