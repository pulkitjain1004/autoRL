# Run random forest


library(tree)
library(randomForest)
library(tidyverse)

# insert pos_pairs and neg pairs
# pos_pairs2
# neg_pairs
# fix column names for both pos pairs and neg pairs

# input feature table in data

rf_data <- read.csv("./round2/feature_table.csv",
                     stringsAsFactors=FALSE)
rf_data <- rf_data[,2:23]
rf_data$response <- as.factor(rf_data$response)

set.seed(1004)
train <- sort(sample(nrow(rf_data), 0.8*nrow(rf_data)))
nc_run.test <- rf_data[-train,]
nc_run.train <- rf_data[train,]

rf_voter <- randomForest(response~.-Indice, data=nc_run.train, 
                            mtry= 4, ntree=200, importance=T)
rf_voter
plot(rf_voter)

yhat_rf <- predict(rf_voter, nc_run.test)

table(test = yhat_rf, true = nc_run.test$response)


# Decide number of trees and number of features
# mtry = sqrt(20), 4
# ntree = atleast 200

rf_voter$importance
varImpPlot(rf_voter)
importance(rf_voter)


# the mean decrease of accuracy in predictions on the out of 
# bag samples when a given variable is excluded from the model.

# total decrease in node impurity that results from splits over that
# variable, averaged over all trees.
# In the case of regression trees, the node impurity is measured 
# by the training RSS, and for classification trees by the deviance.

# Nut shell: Higher values on mean decrease accuracy 
# implies more importance
# Higher value on mean decrease gini implies mixed results 
# on split at that node
# A guess can I remove feature having low value in accuracy as well as
# high value in gini?

# With correlated features, strong features can end up with
# low scores and the method can be biased towards variables 
# with many categories

miss_indice_rf <- nc_run.test %>% filter(yhat_rf != response) %>% 
                      select(Indice)
miss_indice_rf <- unlist(miss_indice_rf)
miss_indice_rf


miss_pos_rf <- pos_pairs2 %>% filter(Indice %in% miss_indice_rf)
miss_pos_rf <- cbind( true_resp = 1, miss_pos_rf)
miss_neg_rf <- neg_pairs %>% filter(Indice %in% miss_indice_rf)
miss_neg_rf <- cbind( true_resp = 0, miss_neg_rf)

miss_pairs_rf <- rbind(miss_pos_rf, miss_neg_rf)

write.csv(miss_pairs_rf, file="./round2/rf_miss_pairs.csv")
