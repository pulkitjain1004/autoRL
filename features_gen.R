# Generating Features and Feature Table for pairs of entities

library(e1071)
library(approxmapR)
# library(caret)
library(stringr)
library(stringdist)
library(stringi)
library(igraph)
library(data.table)

# devtools::install_github("cran/PGRdup")
library(PGRdup) # for Double Metaphone, may have some errors
# not available for 3.3.3

# library(phonics) # for metaphone


# use tolower() with stringdist 
# "" vs "" dist = 0
# NA vs any dist = NA
# 
# # Edit dist
# stringdist("ca","abc")
# 
# # Jaro
# stringdist('MARTHA','MARTHA',method='jw')
# stringdist('MARTHA','MATHRA',method='jw')
# 
# # Jaro Winkler. Preference to starting characters
# stringdist('MARTHA','MATHRA',method='jw', p=0.1)
# 
# words <- c('Catherine', 'Katherine', 'Katarina', 'Johnathan', 
#            'Jonathan', 'John', 'Teresa', 'Theresa', 'Smith', 
#            'Smyth', 'Jessica', 'Joshua')
# 
# # Metaphone, library(phonics)
# metaphone(words, maxCodeLen = 10L)
# 
# 
# # Double Metaphone, library(PGRdup)
# 
# DoubleMetaphone(words)
# 
# 
# dm1 <- stringdist (DoubleMetaphone('MARTHA')$primary, 
#             DoubleMetaphone('MATRHA')$primary) 
# 
# dm2 <- stringdist (DoubleMetaphone('MARTHA')$alternate, 
#             DoubleMetaphone('MATRHA')$alternate) 


# 
# 
# START
# FEATURES
# HERE
# 
# take note of col names
# 
# last_name
# first_name
# middle_name
# res_street_address
# res_city_desc
# race_code
# ethnic_code
# party_cd
# gender_code
# birth_age
# birth_state
# 

dist_func <- function(entity1, entity2){
  
  phi_ed <- stringdist(tolower(entity1), tolower(entity2))
  
  phi_jw <- stringdist(tolower(entity1), tolower(entity2),
                       method='jw', p=0.1)
  
  phi_dm1 <- stringdist (DoubleMetaphone(entity1)$primary, 
                         DoubleMetaphone(entity2)$primary) 
  
  phi_dm2 <- stringdist (DoubleMetaphone(entity1)$alternate, 
                         DoubleMetaphone(entity2)$alternate)   
  
  phi_result <- c(phi_ed, phi_jw, phi_dm1, phi_dm2)
  return(phi_result)
  
}




# enter positive paris here
pos_pairs2 <- pos_pairs[complete.cases(pos_pairs),]


# convert these

entity1 <- pos_pairs2[, c(2:12)]
entity2 <- pos_pairs2[, c(13:23)]

colnames(entity1) = c("last_name"
                      , "first_name"
                      , "middle_name"
                      , "res_street_address"
                      , "res_city_desc"
                      , "race_code"
                      , "ethnic_code"
                      , "party_cd"
                      , "gender_code"
                      , "birth_age"
                      , "birth_state"
                       )


colnames(entity2) = c("last_name"
                      , "first_name"
                      , "middle_name"
                      , "res_street_address"
                      , "res_city_desc"
                      , "race_code"
                      , "ethnic_code"
                      , "party_cd"
                      , "gender_code"
                      , "birth_age"
                      , "birth_state"
)

# entity1 <- rbind(entity1, neg_pairs1)

data_round2 = matrix(1, nrow(entity1), 21)
colnames(data_round2) <- c("response",
                           "phi_l_ed", "phi_l_jw", "phi_l_dm1", "phi_l_dm2",
                           "phi_f_ed", "phi_f_jw", "phi_f_dm1", "phi_f_dm2",
                           "phi_m_ed", "phi_m_jw", "phi_m_dm1", "phi_m_dm2",
                           "phi_addr", "phi_age",
                           "phi_res_city", "phi_race", "phi_ethnic",
                           "phi_party", "phi_gender", "phi_birth_st"
                           )



for(i in 1:nrow(entity1)){
  j=i
phi_f <- dist_func(entity1[i,"first_name"], entity2[j,"first_name"])

phi_l <- dist_func(entity1[i,"last_name"], entity2[j,"last_name"])

phi_m <- dist_func(entity1[i,"middle_name"], entity2[j,"middle_name"])

phi_addr <- stringdist(tolower(entity1[i,"res_street_address"]), 
                     tolower(entity2[j,"res_street_address"]))

phi_age <- if( abs(as.integer(entity1[i,"birth_age"]) - as.integer(entity2[i, "birth_age"])) <= 3 && 
              abs(as.integer(entity1[i, "birth_age"]) - as.integer(entity2[i, "birth_age"])) >= 5 ) 0 else 1

phi_res_city <- if( stringdist( tolower(entity1[i, "res_city_desc"]),
                          tolower(entity2[i, "res_city_desc"])) == 0) 1 else 0

phi_race <- if( stringdist( tolower(entity1[i, "race_code"]),
                          tolower(entity2[i, "race_code"])) == 0) 1 else 0

phi_ethnic <- if( stringdist( tolower(entity1[i, "ethnic_code"]),
                          tolower(entity2[i, "ethnic_code"])) == 0) 1 else 0

phi_party <- if( stringdist( tolower(entity1[i, "party_cd"]),
                          tolower(entity2[i, "party_cd"])) == 0) 1 else 0

phi_gender <- if( stringdist( tolower(entity1[i, "gender_code"]),
                          tolower(entity2[i, "gender_code"])) == 0) 1 else 0

phi_birth_st <- if( stringdist( tolower(entity1[i, "birth_state"]),
                          tolower(entity2[i, "birth_state"])) == 0) 1 else 0


data_round2[i, 2:21] <- c(phi_l, phi_f, phi_m, phi_addr, phi_age,
                          phi_res_city, phi_race, phi_ethnic,
                          phi_party, phi_gender, phi_birth_st)

}





# negpairs matrix



# enter negative pairs here
neg_pairs <- read.csv("C:/Users/Pulkit Jain/Desktop/Record Linkage/round2/neg_pairs_better.csv",
                      stringsAsFactors=FALSE)
neg_pairs <- neg_pairs[,3:24]

neg_pairs <- data.frame(neg_pairs, stringsAsFactors=FALSE)

colnames(neg_pairs) = c("last_name"
                        , "first_name"
                        , "middle_name"
                        , "res_street_address"
                        , "res_city_desc"
                        , "race_code"
                        , "ethnic_code"
                        , "party_cd"
                        , "gender_code"
                        , "birth_age"
                        , "birth_state"
                        , "last_name"
                        , "first_name"
                        , "middle_name"
                        , "res_street_address"
                        , "res_city_desc"
                        , "race_code"
                        , "ethnic_code"
                        , "party_cd"
                        , "gender_code"
                        , "birth_age"
                        , "birth_state"
)


neg_pairs1 <- neg_pairs[, c(1:11)]
neg_pairs2 <- neg_pairs[, c(12:22)]



data_round2_2 = matrix(0, nrow(neg_pairs1), 21)
colnames(data_round2_2) <- c("response",
                           "phi_l_ed", "phi_l_jw", "phi_l_dm1", "phi_l_dm2",
                           "phi_f_ed", "phi_f_jw", "phi_f_dm1", "phi_f_dm2",
                           "phi_m_ed", "phi_m_jw", "phi_m_dm1", "phi_m_dm2",
                           "phi_addr", "phi_age",
                           "phi_res_city", "phi_race", "phi_ethnic",
                           "phi_party", "phi_gender", "phi_birth_st"
)



for(i in 1:nrow(neg_pairs1)){
  j=i
  phi_f <- dist_func(neg_pairs1[i,"first_name"], neg_pairs2[j,"first_name"])
  
  
  phi_l <- dist_func(neg_pairs1[i,"last_name"], neg_pairs2[j,"last_name"])
  
  phi_m <- dist_func(neg_pairs1[i,"middle_name"], neg_pairs2[j,"middle_name"])
  
  phi_addr <- stringdist(tolower(neg_pairs1[i,"res_street_address"]), 
                         tolower(neg_pairs2[j,"res_street_address"]))
  
  phi_age <- if( abs(as.integer(neg_pairs1[i,"birth_age"]) - as.integer(neg_pairs2[i, "birth_age"])) <= 3 && 
                 abs(as.integer(neg_pairs1[i, "birth_age"]) - as.integer(neg_pairs2[i, "birth_age"])) >= 5 ) 0 else 1
  
  phi_res_city <- if( stringdist( tolower(neg_pairs1[i, "res_city_desc"]),
                                  tolower(neg_pairs2[i, "res_city_desc"])) == 0) 1 else 0
  
  phi_race <- if( stringdist( tolower(neg_pairs1[i, "race_code"]),
                              tolower(neg_pairs2[i, "race_code"])) == 0) 1 else 0
  
  phi_ethnic <- if( stringdist( tolower(neg_pairs1[i, "ethnic_code"]),
                                tolower(neg_pairs2[i, "ethnic_code"])) == 0) 1 else 0
  
  phi_party <- if( stringdist( tolower(neg_pairs1[i, "party_cd"]),
                               tolower(neg_pairs2[i, "party_cd"])) == 0) 1 else 0
  
  phi_gender <- if( stringdist( tolower(neg_pairs1[i, "gender_code"]),
                                tolower(neg_pairs2[i, "gender_code"])) == 0) 1 else 0
  
  phi_birth_st <- if( stringdist( tolower(neg_pairs1[i, "birth_state"]),
                                  tolower(neg_pairs2[i, "birth_state"])) == 0) 1 else 0
  
  
  data_round2_2[i, 2:21] <- c(phi_l, phi_f, phi_m, phi_addr, phi_age,
                            phi_res_city, phi_race, phi_ethnic,
                            phi_party, phi_gender, phi_birth_st)
  
}

dim(data_round2)
dim(data_round2_2)

data_round2_final <- rbind(data_round2, data_round2_2)
write.csv(data_round2_final, "feature_table.csv")
