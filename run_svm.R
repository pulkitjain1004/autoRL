# Runing SVM and cross validation on feature table.
# Tuning via, cross validation takes some serious time!!!

library(e1071)



# input feature table in data
svm_data <- read.csv("/general/group/pinfo/pulkit/rec_link/round2/feature_table.csv",
                     stringsAsFactors=FALSE)
svm_data <- svm_data[,2:22]


svm_data$response <- as.factor(svm_data$response)


train <- sort(sample(nrow(svm_data), 0.8*nrow(svm_data)))
nc_run.test <- svm_data[-train,]
nc_run.train <- svm_data[train,]


svmfit <- svm(response~., data=nc_run.train, kernal = "radial", scale = F
              ,cost = 10 , gamma=1)



summary(svmfit)

yhat_test <- predict(svmfit, nc_run.test)
table(test = yhat_test, true = nc_run.test$response)


tc <- tune.control(cross = 5)

tune_radial_svm <- tune.svm(response~., data= svm_data, kernel="radial",
                            # ranges = list(cost = 10^(seq(-1,3)), 
                            gamma = 0.5*(seq(1,5)) )
# )
# , tunecontrol = tc)


tune_radial_svm