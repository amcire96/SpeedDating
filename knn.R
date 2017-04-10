library(readr)
library(class)

# set.seed(1)

Speed_Dating_Data <- read_csv("~/Cornell Junior/Spring 2017/ORIE 4740/Final Project/Speed Dating Data.csv")
# View(Speed_Dating_Data)

columns_kept <- c("gender", "match", "int_corr", "samerace", "age_o", "race_o", "race", "imprace")
filtered_speed_dating <- Speed_Dating_Data[, columns_kept]


sum(is.na(filtered_speed_dating))
# filtered_speed_dating[is.na(filtered_speed_dating),]
filtered_speed_dating <- filtered_speed_dating[complete.cases(filtered_speed_dating), ]
sum(is.na(filtered_speed_dating))

# Check what fraction of couples actually matched
sum(filtered_speed_dating$match == 1) / nrow(filtered_speed_dating)

sapply(filtered_speed_dating, class)
filtered_speed_dating$race_o <- as.factor(filtered_speed_dating$race_o)
filtered_speed_dating$race <- as.factor(filtered_speed_dating$race)
sapply(filtered_speed_dating, class)
levels(filtered_speed_dating$race_o)
levels(filtered_speed_dating$race)





# KNN requires scaled/normalized columns and features to be matrices
filtered_speed_dating$race_o <- model.matrix( ~ race_o - 1, data=filtered_speed_dating)
filtered_speed_dating$race <- model.matrix( ~ race - 1, data=filtered_speed_dating)
filtered_speed_dating_normal <- scale(filtered_speed_dating[,!names(filtered_speed_dating) %in% 'match'])
filtered_speed_dating_match = filtered_speed_dating[,"match"]
avg_accuracies <- rep(0, 20)
Ks <- c(1:20)
for (k_index in (1:length(Ks))) {
  randomizer = sample(nrow(filtered_speed_dating_normal))
  filtered_speed_dating_normal <- filtered_speed_dating_normal[randomizer,]
  filtered_speed_dating_match <- filtered_speed_dating_match[randomizer,]
  folds <- cut(seq(1, nrow(filtered_speed_dating_normal)), breaks=10, labels=FALSE)
  k <- Ks[k_index]
  print(k)
  avg_acc <- 0
  for (i in (1:10)) {
    testIndices <- which(folds==i, arr.ind=TRUE)
    test_set <- filtered_speed_dating_normal[testIndices,]
    train_set <- filtered_speed_dating_normal[-testIndices,]
    train.match <- filtered_speed_dating_match[-testIndices,]$match
    test.match <- filtered_speed_dating_match[testIndices,]$match
    
    knn.pred=knn(train_set,test_set,train.match,k=k)
    print(table(knn.pred,test.match))
    avg_acc <- avg_acc + mean(knn.pred==test.match)
  }
  avg_acc <- avg_acc / 10
  avg_accuracies[k_index] = avg_acc
}
best_k <- Ks[which.max(avg_accuracies)]
avg_accuracies
best_k
