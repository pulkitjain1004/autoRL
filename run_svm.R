# Runing SVM and cross validation on feature table.
# Tuning via, cross validation takes some serious time!!!

library(e1071)
library(tidyverse)


# input feature table in data
svm_data <- read.csv("./round2/feature_table.csv",
                     stringsAsFactors=FALSE)
# svm_data <- svm_data[,2:22]


svm_data$response <- as.factor(svm_data$response)

set.seed(1004)
train <- sort(sample(nrow(svm_data), 0.8*nrow(svm_data)))
nc_run.test <- svm_data[-train,]
nc_run.train <- svm_data[train,]




svmfit <- svm(response~.-Indice, data=nc_run.train, kernal = "radial"
              ,cost = 10 , gamma=1)



summary(svmfit)

yhat_test <- predict(svmfit, nc_run.test)
table(test = yhat_test, true = nc_run.test$response)

miss_indice <- nc_run.test %>% filter(yhat_test != response) %>% select(Indice)
miss_indice <- unlist(miss_indice)

miss_pos <- pos_pairs2 %>% filter(Indice %in% miss_indice)
miss_pos <- cbind( true_resp = 1, miss_pos)
miss_neg <- neg_pairs %>% filter(Indice %in% miss_indice)
miss_neg <- cbind( true_resp = 0, miss_neg)

write.csv(miss_pos, file = "./round2/Missclassified_pairs.csv")

tc <- tune.control(cross = 5)

tune_radial_svm <- tune.svm(response~., data= svm_data, kernel="radial",
                            # ranges = list(cost = 10^(seq(-1,3)), 
                            gamma = 0.5*(seq(1,5)) )
# )
# , tunecontrol = tc)

bind_rows(pos_pairs,neg_pairs)[-train,] %>% filter(yhat_test != nc_run.test$response)


tune_radial_svm