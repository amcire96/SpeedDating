library(readr)

set.seed(1)

Speed_Dating_Data <- read_csv("~/Cornell Junior/Spring 2017/ORIE 4740/Final Project/Speed Dating Data.csv")
# View(Speed_Dating_Data)

columns_kept <- c("gender", "int_corr", "samerace", "race_o", "race", 
                  "dec_o", "age", "field_cd", "income", "attr_o",	"sinc_o",
                  "intel_o"	, "fun_o", "amb_o",	"shar_o",	"like_o",	"prob_o",	"met_o")

speed_dating_o <- Speed_Dating_Data[, columns_kept]


sum(is.na(speed_dating_o))
speed_dating_o <- speed_dating_o[complete.cases(speed_dating_o), ]
sum(is.na(speed_dating_o))

# Check what fraction of couples actually matched
sum(speed_dating_o$dec_o == 1) / nrow(speed_dating_o)

sapply(speed_dating_o, class)
speed_dating_o$race_o <- as.factor(speed_dating_o$race_o)
speed_dating_o$race <- as.factor(speed_dating_o$race)
speed_dating_o$field_cd <- as.factor(speed_dating_o$field_cd)
sapply(speed_dating_o, class)
levels(speed_dating_o$race_o)
levels(speed_dating_o$race)
levels(speed_dating_o$field_cd)


# Validation on 20% of the data
threshold <- 0.5
train_ind <- sample(1:nrow(speed_dating_o), .8*nrow(speed_dating_o))
train_set <- speed_dating_o[train_ind, ]
test_set <- speed_dating_o[-train_ind, ]
log_reg_model <- glm(dec_o~., data = train_set, family = binomial)
summary(log_reg_model)

train_preds <- predict(log_reg_model, train_set, type="response")
train_bin_preds <- as.numeric(train_preds >= threshold)
train_accuracy <- mean(train_bin_preds==train_set["dec_o"])
train_accuracy
table(train_bin_preds, train_set$dec_o)


test_preds <- predict(log_reg_model, test_set, type="response")
test_bin_preds <- as.numeric(test_preds >= threshold)
test_accuracy <- mean(test_bin_preds==test_set["dec_o"])
test_accuracy
table(test_bin_preds, test_set$dec_o)

library(pROC)
g <- roc(dec_o ~ test_preds, data=test_set)
plot(g)


