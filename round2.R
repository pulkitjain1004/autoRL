# Round 2
# Generating Positive and negative pairs
# Blocking

library(e1071)
library(approxmapR)
library(caret)
library(stringr)
library(stringdist)
library(PGRdup) # for Double Metaphone, may have some errors
# not available for 3.3.3

library(phonics)


# use tolower() with stringdist 
# "" vs "" dist = 0
# NA vs any dist = NA

stringdist("ca","abc")

stringdist('MARTHA','MARTHA',method='jw')

# Jaro
stringdist('MARTHA','MATHRA',method='jw')

# Jaro Winkler. Preference to starting characters
stringdist('MARTHA','MATHRA',method='jw', p=0.1)

words <- c('Catherine', 'Katherine', 'Katarina', 'Johnathan', 
           'Jonathan', 'John', 'Teresa', 'Theresa', 'Smith', 
           'Smyth', 'Jessica', 'Joshua')

metaphone(words, maxCodeLen = 10L)


feature_score <- function(entity1, entity2){
  
  score <- 0
  for(i in 2:5){
    
    norm <- max(nchar(entity1[i]),nchar(entity2[i]))
    score <- score + (stringdist( tolower(entity1[i]), tolower(entity2[i]) )) / norm
  }
  
  
  
  for(i in 6:12){
    
    if(i==11){
      if( abs(as.integer(entity1[i]) - as.integer(entity2[i])) <= 3 && 
          abs(as.integer(entity1[i]) - as.integer(entity2[i])) >= 5 ) {
        score = score + 1
      }
    }
    
    if( stringdist( tolower(entity1[i]), tolower(entity2[i])) != 0){
      score = score + 1
    }
  }
  
  return(score)
}



propmiss <- function(dataframe) lapply(dataframe, 
                                       function(x) data.frame( #n=length(x),
                                         # n_miss=sum(is.na(x)), 
                                         prop_miss=sum(is.na(x))/length(x)))



nc_voter_apr13 <- read.csv("C:/Users/Pulkit Jain/Desktop/Record Linkage/Data/HPRC/ncvoter15_apr13/ncvoter15_apr13.csv")
voter2_apr13 <- nc_voter_apr13[,c(3,10,11,12,14,15, seq(26,31) ) ]

# propmiss(nc_voter_apr13)
# write.csv(propmiss(nc_voter_apr13), file="miss_13.csv")

nc_voter_mar17 <- read.csv("C:/Users/Pulkit Jain/Desktop/Record Linkage/Data/HPRC/ncvoter15_mar17/ncvoter15_mar17.csv")
voter2_mar17 <- nc_voter_mar17[,c(3,10,11,12,14,15, seq(26,31) ) ]

# convert 2 to 5 to characters

for (i in 2:5){
  voter2_mar17[,i] <- as.character(voter2_mar17[,i])

  voter2_apr13[,i] <- as.character(voter2_apr13[,i])

}


# remove white spaces from columns 7 to 12
for (i in 7:12){
  voter2_mar17[,i] <- as.character(voter2_mar17[,i])
  voter2_mar17[,i] <- gsub(" ", "", voter2_mar17[,i], fixed = T)
  
  voter2_apr13[,i] <- as.character(voter2_apr13[,i])
  voter2_apr13[,i] <- gsub(" ", "", voter2_apr13[,i], fixed = T)
  
}


voter2_apr13$birth_place[3]
voter2_apr13[1,]
typeof(voter2_apr13$voter_reg_num)







# Convert blanks in col 7 to 12 to NA
# blanks will imply a match. False Positive hurts more.

for(i in 1:nrow(voter2_apr13)){
 
  for(j in 7:12){
    if(voter2_apr13[i,j] == ""){
        voter2_apr13[i,j] <- NA
      }
    }
}

for(i in 1:nrow(voter2_mar17)){
  
  for(j in 7:12){
    if(voter2_mar17[i,j] == ""){
      voter2_mar17[i,j] <- NA
    }
  }
}


# Reduce blank middle names to a single blank
# Remove trail and leading white space

voter2_apr13$midl_name[25]

for(i in 1:nrow(voter2_apr13)){
  voter2_apr13$midl_name[i] = trimws(voter2_apr13$midl_name[i])
  if(voter2_apr13$midl_name[i] == ""){
    voter2_apr13$midl_name[i] <- " "
  } 
}

for(i in 1:nrow(voter2_mar17)){
  voter2_mar17$middle_name[i] = trimws(voter2_mar17$middle_name[i])
  if(voter2_mar17$middle_name[i] == ""){
    voter2_mar17$middle_name[i] <- " "
  } 
}

# Remove lead & trail white spaces in first and last name

for(i in 1:nrow(voter2_apr13)){
  
  voter2_apr13$last_name[i] <- trimws(voter2_apr13$last_name[i])
  voter2_apr13$first_name[i] <- trimws(voter2_apr13$first_name[i])
  
  }

for(i in 1:nrow(voter2_mar17)){
  
  voter2_mar17$last_name[i] <- trimws(voter2_mar17$last_name[i])
  voter2_mar17$first_name[i] <- trimws(voter2_mar17$first_name[i])
  
}



# Remove trailing and leading space for City and Add
# If blank convert to NA

for(i in 1:nrow(voter2_apr13)){
  
  voter2_apr13$res_street_address[i] <- trimws(voter2_apr13$res_street_address[i])  
  voter2_apr13$res_city_desc[i] <- trimws(voter2_apr13$res_city_desc[i])
  
  if(voter2_apr13$res_street_address[i] == ""){
    voter2_apr13$res_street_address[i] <- NA
  }
  
  if(voter2_apr13$res_city_desc[i] == ""){
    voter2_apr13$res_city_desc[i] <- NA
  }
  
}


for(i in 1:nrow(voter2_mar17)){
  
  voter2_mar17$res_street_address[i] <- trimws(voter2_mar17$res_street_address[i])  
  voter2_mar17$res_city_desc[i] <- trimws(voter2_mar17$res_city_desc[i])
  
  if(voter2_mar17$res_street_address[i] == ""){
    voter2_mar17$res_street_address[i] <- NA
  }
  
  if(voter2_mar17$res_city_desc[i] == ""){
    voter2_mar17$res_city_desc[i] <- NA
  }
  
}



colnames(voter2_apr13)[4] <- "middle_name"

colnames(voter2_apr13)[12] <- "birth_state"

pos_pairs <- inner_join(voter2_apr13, voter2_mar17, by="voter_reg_num")
# 6548 pairs

pos_pairs2 <- pos_pairs[complete.cases(pos_pairs),]

feature_score(voter2_apr13[1,], voter2_mar17[1,])


# Remove rows with NA
com_voter2_apr13 <- voter2_apr13[complete.cases(voter2_apr13),]
com_voter2_mar17 <- voter2_mar17[complete.cases(voter2_mar17),]


# Build negative pairs

k=1
neg_pairs <- data.frame(matrix(0, 4*nrow(com_voter2_mar17), 23))
a <- nrow(com_voter2_apr13)
b <- nrow(com_voter2_mar17)

for(i in 1:a){
    n=0
    n1=0
    n2=0
    temp <- data.frame(matrix(0, 1000, 23))
    temp[,1] <- 1000 
  for(j in 1:b){
      if(com_voter2_apr13[i,1] != com_voter2_mar17[j,1]){
      if(stringdist( tolower(com_voter2_apr13[i,3]), 
                    tolower(com_voter2_mar17[j,3]) ) == 0 ){
        
       n1=n1+1
       score <- feature_score(com_voter2_apr13[i,], 
                             com_voter2_mar17[j,])  
       temp[j,] <- cbind( score, 
                     com_voter2_apr13[i,2:12], 
                     com_voter2_mar17[j,2:12])
      }
    }
  }
      
      colnames(temp)[1] <- "score"
      temp <- temp %>% arrange(score)
      if(n1 >= 1){
        neg_pairs[k,] <- temp[1,]
        k=k+1
      }
      if(n1 >= 2){
        neg_pairs[k,] <- temp[2,]
        k=k+1
      }
      
      if(n1>2){
        n1=2
      }
      
      temp <- data.frame(matrix(0, 1000, 23))
      temp[,1] <- 1000 
      for(j in 1:b){
        if(com_voter2_apr13[i,1] != com_voter2_mar17[j,1]){
          if(stringdist( tolower(com_voter2_apr13[i,5]), 
                         tolower(com_voter2_mar17[j,5]) ) == 0 ){
            
            n2=n2+1
            score <- feature_score(com_voter2_apr13[i,], 
                                   com_voter2_mar17[j,])  
            temp[j,] <- cbind( score, 
                               com_voter2_apr13[i,2:12], 
                               com_voter2_mar17[j,2:12])
          }
        }
      }
      
      colnames(temp)[1] <- "score"
      temp <- temp %>% arrange(score)
      if(n2 >= 1){
        neg_pairs[k,] <- temp[1,]
        k=k+1
      }
      if(n2 >= 2){
        neg_pairs[k,] <- temp[2,]
        k=k+1
      }
      
      if(n2>2){ n2=2 }
      # if n<4
      
      n = n1+n2
      
      if(n < 4){
      
        temp <- data.frame(matrix(0, 1000, 23))
        temp[,1] <- 1000 
        for(j in 1:b){
          if(com_voter2_apr13[i,1] != com_voter2_mar17[j,1]){
            if(as.integer(com_voter2_mar17[j,11]) - 
                      as.integer(com_voter2_apr13[i,11]) >= 3 && 
               as.integer(com_voter2_mar17[j,11]) - 
                      as.integer(com_voter2_apr13[i,11]) <= 5 ){
              
                score <- feature_score(com_voter2_apr13[i,], 
                                     com_voter2_mar17[j,])  
                temp[j,] <- cbind( score, 
                                 com_voter2_apr13[i,2:12], 
                                 com_voter2_mar17[j,2:12])
              }
            }
          }
        
        colnames(temp)[1] <- "score"
        temp <- temp %>% arrange(score)
        
        neg_pairs[k,] <- temp[1,]
        n=n+1
        k=k+1
      }
      
      
      if(n < 4){
        neg_pairs[k,] <- temp[2,]
        n=n+1
        k=k+1
      }
      
      if(n < 4){
        neg_pairs[k,] <- temp[3,]
        n <- n+1
        k=k+1
      }
      
      if(n < 4){
        neg_pairs[k,] <- temp[4,]
        n <- n+1
        k=k+1
      }
    
     
      
}



write.csv(neg_pairs, file="neg_pairs_better.csv")
 