# Run random forest


library(tree)
library(randomForest)
library(tidyverse)

# insert pos_pairs and neg pairs
pos_pairs4 <- read.csv("./round4/pos_pairs4.csv")
neg_pairs4 <- read.csv("./round4/neg_pairs4.csv")
# fix column names for both pos pairs and neg pairs

# input feature table in data

rf_data <- read.csv("./round4/feature_table4.csv",
                     stringsAsFactors=FALSE)
rf_data <- rf_data[,2:ncol(rf_data)]
rf_data$response <- as.factor(rf_data$response)
rf_data$age_f <- as.factor(rf_data$age_f)
rf_data$res_city_f <- as.factor(rf_data$res_city_f)
rf_data$race_f <- as.factor(rf_data$race_f)
rf_data$ethnic_f <- as.factor(rf_data$ethnic_f)
rf_data$party_f <- as.factor(rf_data$party_f)
rf_data$gender_f <- as.factor(rf_data$gender_f)
rf_data$birth_st_f <- as.factor(rf_data$birth_st_f)
rf_data$l_new_f <- as.factor(rf_data$l_new_f)
rf_data$rule_f <- as.factor(rf_data$rule_f)
rf_data$f_rule_f <- as.factor(rf_data$f_rule_f)


set.seed(1004)
train <- sort(sample(nrow(rf_data), 0.8*nrow(rf_data)))
nc_run.test <- rf_data[-train,]
nc_run.train <- rf_data[train,]

rf_voter <- randomForest(response~.-indice
                         -l_sp1_ed_i -l_sp1_jw_r -l_sp1_dm1_i -l_sp1_dm2_i 
                         -l_sp2_ed_i -l_sp2_jw_r -l_sp2_dm1_i -l_sp2_dm2_i
                         -birth_st_sp_r -l_new_f - f_rule_f - rule_f
                         , data=nc_run.train, 
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


# Get pairs which are missclasified.
miss_indice_rf <- nc_run.test %>% filter(yhat_rf != response) %>% 
                      select(indice)
miss_indice_rf <- unlist(miss_indice_rf)
miss_indice_rf


miss_pos_rf <- pos_pairs4 %>% filter(indice %in% miss_indice_rf)
miss_pos_rf <- cbind( true_resp = 1, miss_pos_rf)
miss_neg_rf <- neg_pairs4 %>% filter(indice %in% miss_indice_rf)
miss_neg_rf <- cbind( true_resp = 0, miss_neg_rf)

miss_pairs_rf <- rbind(miss_pos_rf, miss_neg_rf)

write.csv(miss_pairs_rf, file="./round4/rf_miss_pairs.csv")

# Get positive predictions for calculatinf many-to-one predictions
pos_pred_indice_rf <- nc_run.test %>% filter(yhat_rf == 1) %>% 
  select(indice)
pos_pred_indice_rf <- unlist(pos_pred_indice_rf)


pos_pred_rf1 <- pos_pairs4 %>% filter(indice %in% pos_pred_indice_rf)
pos_pred_rf1 <- cbind( true_resp = 1, pos_pred_rf1)
pos_pred_rf2 <- neg_pairs4 %>% filter(indice %in% pos_pred_indice_rf)
pos_pred_rf2 <- cbind( true_resp = 0, pos_pred_rf2)

pos_pred_rf <- rbind(pos_pred_rf1, pos_pred_rf2)




# count repeating a_code and b_code
# get their indices
# change yhat of those indices to 0
# make new confusion matrix
# save all repeted values

rep <- summary(pos_pred_rf$a_code)[summary(pos_pred_rf$a_code) >1]
rep <- data.frame(rep)
rep <- rownames(rep[])
a_code_num <- rep[1:length(rep)-1]

rep <- summary(pos_pred_rf$b_code)[summary(pos_pred_rf$b_code) >1]
rep <- data.frame(rep)
rep <- rownames(rep[])
b_code_num <- rep[1:length(rep)-1]

rep_indice <- c(pos_pairs4 %>% filter(a_code %in% a_code_num) %>%
                  select(indice),
                pos_pairs4 %>% filter(b_code %in% b_code_num) %>%
                  select(indice))

rep_indice <- unlist(rep_indice)


for (i in 1:length(rep_indice)){
  
  a <- as.character(rep_indice[i])
  yhat_rf[a] <- 0
}

table(test = yhat_rf, true = nc_run.test$response)


# print miss_rf_here

rep_pred_rf <- rbind(pos_pred_rf %>% filter (a_code %in% a_code_num),
                     pos_pred_rf %>% filter (b_code %in% b_code_num))

write.csv(rep_pred_rf, file="./round4/rep_pred_rf.csv")
