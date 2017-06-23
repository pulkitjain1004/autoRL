library(e1071)
library(approxmapR)

# understand data and first trial round of SVM

s.lev.dist = function(s1,s2){
  #s1="complicated"; s2="random"
  
  m = nchar(s1)
  n = nchar(s2)
  
  t1 = strsplit(s1, "")[[1]]
  t2 = strsplit(s2, "")[[1]]
  
  D = matrix(0,m+1,n+1)
  D[,1] = 0:m
  D[1,] = 0:n
  
  rownames(D) = c("",t1)
  colnames(D) = c("",t2)
  
  for (i in 1:m){	
    for (j in 1:n){
      a = D[i,j+1]+1
      b = D[i+1,j]+1
      c= D[i,j] + ifelse(t1[i]==t2[j], 0, 1)
      
      D[i+1,j+1] = min(a,b,c)
    }
  }
  
  return(D[m+1, n+1])
  
}



propmiss <- function(dataframe) lapply(dataframe, 
                                        function(x) data.frame( #n=length(x),
                                                               # n_miss=sum(is.na(x)), 
                                                               prop_miss=sum(is.na(x))/length(x)))



nc_voter_apr13 <- read.csv("C:/Users/Pulkit Jain/Desktop/Record Linkage/Data/HPRC/ncvoter15_apr13/ncvoter15_apr13.csv")
# nc_voter_apr13 <- nc_voter_apr13[,-c(seq(46:59), 64,65) ]

# propmiss(nc_voter_apr13)
# write.csv(propmiss(nc_voter_apr13), file="miss_13.csv")

nc_voter_mar17 <- read.csv("C:/Users/Pulkit Jain/Desktop/Record Linkage/Data/HPRC/ncvoter15_mar17/ncvoter15_mar17.csv")
# nc_voter_mar17 <- nc_voter_mar17[,-c(32, seq(47:60), 65,66) ]

propmiss(nc_voter_mar17)
write.csv(propmiss(nc_voter_mar17), file="miss_17.csv")

# nc_voter <- left_join(nc_voter_apr13, nc_voter_mar17, by="voter_reg_num")
# summary(table(nc_voter$voter_reg_num))
# 
# 
# nc_voter2 <- left_join(nc_voter_mar17, nc_voter_apr13, by="voter_reg_num")
# summary(table(nc_voter2$voter_reg_num))


nc_voter1 <- nc_voter_apr13 %>% select(voter_reg_num, last_name, first_name)

nc_voter2 <- nc_voter_mar17 %>% select(voter_reg_num, last_name, first_name)





nc_voter_pos <- left_join(nc_voter1, nc_voter2, by= "voter_reg_num")
nc_voter_pos <- nc_voter_pos[complete.cases(nc_voter_pos),] 

for (i in 2:5){
nc_voter_pos[,i] <- as.character(nc_voter_pos[,i])
nc_voter_pos[,i] <- gsub(" ", "", nc_voter_pos[,i], fixed = T)

}



nc_voter_pos <- nc_voter_pos %>% rowwise %>% mutate(Res=1, phi_L=0, phi_F=0)


# use this later also
for(i in 1:nrow(nc_voter_pos)){

  nc_voter_pos$phi_F[i] = s.lev.dist(nc_voter_pos$first_name.x[i],nc_voter_pos$first_name.y[i])
  nc_voter_pos$phi_L[i] = s.lev.dist(nc_voter_pos$last_name.x[i],nc_voter_pos$last_name.y[i])
}


# sort

nc_voter_pos <- nc_voter_pos %>% arrange(desc(phi_L), desc(phi_F))
# 118 cases where there is a change in name
# taking 1:50, 151:200
nc_voter_pos <- nc_voter_pos[c(1:50, 151:200),]

nc_voter_neg <- matrix(0,2500,7)
colnames(nc_voter_neg) <- c("last_name.x",
                            "first_name.x",
                            "last_name.y",
                            "first_name.y",
                            "Res",
                            "phi_L",
                            "phi_F"
                            )



k=1
for(i in 1:50){
  for (j in 1:50){
    if(i!=j)
    nc_voter_neg[k,1]=nc_voter_pos$last_name.x[i]
    nc_voter_neg[k,2]=nc_voter_pos$first_name.x[i]
    nc_voter_neg[k,3]=nc_voter_pos$last_name.y[j]
    nc_voter_neg[k,4]=nc_voter_pos$first_name.y[j]
    k= k+1
  }
    
}

# fix this
nc_voter_neg[2500,1]=nc_voter_pos$last_name.x[50]

nc_voter_neg <- data.frame(nc_voter_neg)
for (i in 1:4){
  nc_voter_neg[,i] <- as.character(nc_voter_neg[,i])
  
}

for (i in 5:7){
  nc_voter_neg[,i] <- as.integer(nc_voter_neg[,i])
  
}

nc_voter_neg$Res=0


for(i in 1:nrow(nc_voter_neg)){
  
  nc_voter_neg$phi_F[i] = s.lev.dist(nc_voter_neg$first_name.x[i],nc_voter_neg$first_name.y[i])
  nc_voter_neg$phi_L[i] = s.lev.dist(nc_voter_neg$last_name.x[i],nc_voter_neg$last_name.y[i])
}

nc_run <- rbind(nc_voter_pos[,2:8], nc_voter_neg[,1:7])
nc_run <- data.frame(nc_run)
nc_run$Res <- as.factor(nc_run$Res) 

# Run svm

set.seed(1004)
train <- sort(sample(nrow(nc_run), 2000))
nc_run.test <- nc_run[-train,]
nc_run.train <- nc_run[train,]

svmfit <- svm(Res~phi_L+phi_F, data=nc_run.train, kernal = "radial", scale = F
              ,cost = 10 , gamma=1)
plot(svmfit, nc_run.train[,5:7])
# X: svSymbol	 Symbol used for support vectors.
# O: dataSymbol Symbol used for data points (other than support vectors).

summary(svmfit)

yhat_test <- predict(svmfit, nc_run.test)
table(test = yhat_test, true = nc_run.test$Res)

