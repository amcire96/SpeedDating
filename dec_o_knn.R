library(readr)
library(class)

# set.seed(1)

Speed_Dating_Data <- read_csv("~/Cornell Junior/Spring 2017/ORIE 4740/Final Project/Speed Dating Data.csv")
# View(Speed_Dating_Data)

columns_kept <- c("gender", "round", "order", "int_corr", "samerace", "age_o", 
                  "dec_o", "attr_o", "sinc_o", "intel_o", "fun_o", "amb_o", 
                  "shar_o", "like_o", 'age', "prob")

filtered_speed_dating <- Speed_Dating_Data[, columns_kept]


sum(is.na(filtered_speed_dating))
# filtered_speed_dating[is.na(filtered_speed_dating),]
filtered_speed_dating <- filtered_speed_dating[complete.cases(filtered_speed_dating), ]
sum(is.na(filtered_speed_dating))

# Check what fraction of dec_o = 1
sum(filtered_speed_dating$dec_o == 1) / nrow(filtered_speed_dating)

# sapply(filtered_speed_dating, class)
# filtered_speed_dating$race_o <- as.factor(filtered_speed_dating$race_o)
# filtered_speed_dating$race <- as.factor(filtered_speed_dating$race)
# sapply(filtered_speed_dating, class)
# levels(filtered_speed_dating$race_o)
# levels(filtered_speed_dating$race)


# KNN requires scaled/normalized columns and features to be matrices
# filtered_speed_dating$race_o <- model.matrix( ~ race_o - 1, data=filtered_speed_dating)
# filtered_speed_dating$race <- model.matrix( ~ race - 1, data=filtered_speed_dating)
filtered_speed_dating_normal <- scale(filtered_speed_dating[,!names(filtered_speed_dating) %in% 'dec_o'])
filtered_speed_dating_dec_o = filtered_speed_dating[,"dec_o"]
Ks <- c(10:25)
avg_accuracies <- rep(0, length(Ks))
for (k_index in (1:length(Ks))) {
  set.seed(1)
  randomizer = sample(nrow(filtered_speed_dating_normal))
  filtered_speed_dating_normal_random <- filtered_speed_dating_normal[randomizer,]
  filtered_speed_dating_dec_o_random <- filtered_speed_dating_dec_o[randomizer,]
  folds <- cut(seq(1, nrow(filtered_speed_dating_normal)), breaks=10, labels=FALSE)
  k <- Ks[k_index]
  print(k)
  avg_acc <- 0
  for (i in (1:10)) {
    testIndices <- which(folds==i, arr.ind=TRUE)
    test_set <- filtered_speed_dating_normal_random[testIndices,]
    train_set <- filtered_speed_dating_normal_random[-testIndices,]
    train.dec_o <- filtered_speed_dating_dec_o_random[-testIndices,]$dec_o
    test.dec_o <- filtered_speed_dating_dec_o_random[testIndices,]$dec_o
    
    knn.pred=knn(train_set,test_set,train.dec_o,k=k)
    print(table(knn.pred,test.dec_o))
    avg_acc <- avg_acc + mean(knn.pred==test.dec_o)
  }
  avg_acc <- avg_acc / 10
  avg_accuracies[k_index] = avg_acc
}
best_k <- Ks[which.max(avg_accuracies)]
avg_accuracies
best_k
plot(Ks, avg_accuracies, main="Cross Validation Accuracies for Different K values", xlab="k", ylab="Acc", type="b")


# ROC Curve
train_ind <- sample(1:nrow(filtered_speed_dating_normal), .8*nrow(filtered_speed_dating_normal))
train_set <- filtered_speed_dating_normal[train_ind, ]
test_set <- filtered_speed_dating_normal[-train_ind, ]
train_dec_o <- filtered_speed_dating_dec_o[train_ind, ]
test_dec_o <- filtered_speed_dating_dec_o[-train_ind, ]
knn.pred_prob <- attr(knn(train_set, test_set, train_dec_o$dec_o, k=18, prob=TRUE), "prob")
library(pROC)
g <- roc(test_dec_o$dec_o, knn.pred_prob)
plot(g)
