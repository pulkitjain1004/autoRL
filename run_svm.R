# Runing SVM and cross validation on feature table.
# Tuning via, cross validation takes some serious time!!!

library(e1071)
library(tidyverse)

# insert pos_pairs and neg pairs
# fix column names for both pos pairs and neg pairs

colnames(neg_pairs) = c("Indice"
                        , "last_name.x"
                        , "first_name.x"
                        , "middle_name.x"
                        , "res_street_address.x"
                        , "res_city_desc.x"
                        , "race_code.x"
                        , "ethnic_code.x"
                        , "party_cd.x"
                        , "gender_code.x"
                        , "birth_age.x"
                        , "birth_state.x"
                        , "last_name.y"
                        , "first_name.y"
                        , "middle_name.y"
                        , "res_street_address.y"
                        , "res_city_desc.y"
                        , "race_code.y"
                        , "ethnic_code.y"
                        , "party_cd.y"
                        , "gender_code.y"
                        , "birth_age.y"
                        , "birth_state.y"
)


# input feature table in data
svm_data <- read.csv("./round2/feature_table.csv",
                     stringsAsFactors=FALSE)
svm_data <- svm_data[,2:23]


svm_data$response <- as.factor(svm_data$response)

set.seed(1004)
train <- sort(sample(nrow(svm_data), 0.8*nrow(svm_data)))
nc_run.test <- svm_data[-train,]
nc_run.train <- svm_data[train,]




svmfit <- svm(response~.-Indice, data=nc_run.train, probability =T,
              kernal = "radial", cost = 10, gamma=1)

summary(svmfit)

yhat_test <- predict(svmfit, nc_run.test, probability= T)
table(test = yhat_test, true = nc_run.test$response)

miss_indice <- nc_run.test %>% filter(yhat_test != response) %>% select(Indice)
miss_indice <- unlist(miss_indice)
miss_indice

miss_pos <- pos_pairs2 %>% filter(Indice %in% miss_indice)
miss_pos <- cbind( true_resp = 1, miss_pos)
miss_neg <- neg_pairs %>% filter(Indice %in% miss_indice)
miss_neg <- cbind( true_resp = 0, miss_neg)

prob <- data.frame(attr(yhat_test, "probabilities"))

miss_pairs <- rbind(miss_pos, miss_neg)
miss_pairs <- cbind(prob %>% filter(row.names(prob) %in% miss_indice),
                    miss_pairs)
colnames(miss_pairs)[1] <- "prob_1"
colnames(miss_pairs)[2] <- "prob_0"

write.csv(miss_pairs, file = "./round2/Missclassified_pairs.csv")

# sort according to probability
sort_prob <- sort(apply(prob,1,max))
quantile(sort_prob[1:10])

low_prob_indice <- row.names(data.frame(sort_prob))[1:20]

low_prob_p <- pos_pairs2 %>% filter ( Indice %in% low_prob_indice)
low_prob_p <- cbind( true_resp = 1, low_prob_p)                                   
low_prob_n <- neg_pairs %>% filter ( Indice %in% low_prob_indice)
low_prob_n <- cbind( true_resp = 0, low_prob_n)              
low_prob_pair <- rbind(low_prob_p, low_prob_n)
low_prob_pair <- cbind(prob %>% filter(row.names(prob) %in% low_prob_indice)
                       , low_prob_pair)

write.csv(low_prob_pair, file="./round2/low_prob_pair.csv")

tc <- tune.control(cross = 5)

tune_radial_svm <- tune.svm(response~., data= svm_data, kernel="radial",
                            # ranges = list(cost = 10^(seq(-1,3)), 
                            gamma = 0.5*(seq(1,5)) )
# )
# , tunecontrol = tc)

bind_rows(pos_pairs,neg_pairs)[-train,] %>% filter(yhat_test != nc_run.test$response)


tune_radial_svm