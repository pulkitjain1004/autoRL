library(stringr)
library(stringdist)
library(stringi)
library(igraph)
library(data.table)
# devtools::install_github("cran/PGRdup")
library(PGRdup) # for Double Metaphone, may have some errors
# not available for 3.3.3


# life feature: return 1 if most obvious indicator are same

life_feature <- function(first_name.x, gender_code.x, birth_age.x, 
                         birth_state.x,
                         first_name.y, gender_code.y, birth_age.y, 
                         birth_state.y){
  
  first_name.x <- tolower(first_name.x)
  gender_code.x <- tolower(gender_code.x)
  birth_age.x <- as.integer(birth_age.x)
  birth_state.x <- tolower(birth_state.x)
  first_name.y <- tolower(first_name.y)
  gender_code.y <- tolower(gender_code.y)
  birth_age.y <- as.integer(birth_age.y)
  birth_state.y <- tolower(birth_state.y)
  
  
  
  if( stringdist(first_name.x, first_name.y)==0 &&
      stringdist(gender_code.x, gender_code.y)==0 &&
      stringdist(birth_state.x, birth_state.y)==0 &&
      (birth_age.y - birth_age.x) <=5 &&
      (birth_age.y - birth_age.x) >=3
  ){
    
    return(1) 
  }
  
  return(0)
  
}



# dist_func: gives four string distances
dist_func <- function(entity1, entity2){
  
  entity1 <- tolower(entity1)
  entity2 <- tolower(entity2)
  
  
  phi_ed <- stringdist(entity1, entity2)
  
  phi_jw <- stringdist(entity1, entity2, method='jw', p=0.1)
  
  phi_dm1 <- stringdist (DoubleMetaphone(entity1)$primary, 
                         DoubleMetaphone(entity2)$primary) 
  
  phi_dm2 <- stringdist (DoubleMetaphone(entity1)$alternate, 
                         DoubleMetaphone(entity2)$alternate)   
  
  phi_result <- c(phi_ed, phi_jw, phi_dm1, phi_dm2)
  return(phi_result)
  
}


# enter positive paris here
pos_pairs3 <- read.csv("./round3/pos_pairs3.csv",
                       stringsAsFactors = F)
pos_pairs3 <- pos_pairs3[, 2:24]
pos_pairs3 <- cbind( matrix(1,nrow(pos_pairs3),1), pos_pairs3)

neg_pairs3 <- read.csv("./round3/neg_pairs3.csv", 
                      stringsAsFactors=FALSE)
neg_pairs3 <- neg_pairs3[,2:24]
neg_pairs3 <- cbind(matrix(0, nrow(neg_pairs3),1), neg_pairs3)


# change column names and merge

colnames(neg_pairs3) = c("response"
                        , "indice"
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

colnames(pos_pairs3) = c("response"
                         , "indice"
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

pairs3 <- rbind(pos_pairs3, neg_pairs3)

# create features

feature_table3 <- matrix(0, nrow(pairs3), 34)
colnames(feature_table3) <- c("response", "indice",
                              "l_ed_i", "l_jw_r", "l_dm1_i", "l_dm2_i", 
                              "f_ed_i", "f_jw_r", "f_dm1_i", "f_dm2_i", 
                              "m_ed_i", "m_jw_r", "m_dm1_i", "m_dm2_i", 
                              "addr_i",
                              "age_f", "res_city_f", "race_f", 
                              "ethnic_f", "party_f", "gender_f", 
                              "birth_st_f",
                              "l_sp1_ed","l_sp1_jw", 
                              "l_sp1_dm1", "l_sp1_dm2", 
                              "l_sp2_ed","l_sp2_jw", 
                              "l_sp2_dm1", "l_sp2_dm2", 
                              "birth_st_sp", "l_new_f", 
                              "rule_f", "f_rule_f")

for(i in 1:nrow(pairs3_entity1)){
  
  phi_f <- dist_func(pairs3$first_name.x[i], pairs3[i,"first_name.y"])
  
  phi_l <- dist_func(pairs3[i,"last_name.x"], pairs3[i,"last_name.y"])
  
  phi_m <- dist_func(pairs3[i,"middle_name.x"], pairs3[i,"middle_name.y"])
  
  phi_addr <- stringdist(tolower(pairs3[i,"res_street_address.x"]), 
                         tolower(pairs3[i,"res_street_address.y"]))
  
  phi_age <- if( as.integer(pairs3[i, "birth_age.y"]
                   -as.integer(pairs3[i,"birth_age.x"])
  ) < 3 ||
  as.integer(pairs3[i, "birth_age.y"]
             -as.integer(pairs3[i,"birth_age.x"])
  ) > 5 ) 0 else 1
  
  phi_res_city <- if( stringdist( tolower(pairs3[i, "res_city_desc.x"]),
                                  tolower(pairs3[i, "res_city_desc.y"])) == 0) 1 else 0
  
  phi_race <- if( stringdist( tolower(pairs3[i, "race_code.x"]),
                              tolower(pairs3[i, "race_code.y"])) == 0) 1 else 0
  
  phi_ethnic <- if( stringdist( tolower(pairs3[i, "ethnic_code.x"]),
                                tolower(pairs3[i, "ethnic_code.y"])) == 0) 1 else 0
  
  phi_party <- if( stringdist( tolower(pairs3[i, "party_cd.x"]),
                               tolower(pairs3[i, "party_cd.y"])) == 0) 1 else 0
  
  phi_gender <- if( stringdist( tolower(pairs3[i, "gender_code.x"]),
                                tolower(pairs3[i, "gender_code.y"])) == 0) 1 else 0
  
  phi_birth_st <- if( stringdist( tolower(pairs3[i, "birth_state.x"]),
                                  tolower(pairs3[i, "birth_state.y"])) == 0) 1 else 0
  
  
  a = life_feature(pairs3[i, "first_name.x"], 
                   pairs3[i, "gender_code.x"], 
                   pairs3[i, "birth_age.x"], 
                   pairs3[i, "birth_state.x"],
                   pairs3[i, "first_name.y"], 
                   pairs3[i, "gender_code.y"], 
                   pairs3[i, "birth_age.y"], 
                   pairs3[i, "birth_state.y"]
                   )
  phi_new <- 0
  if (a == 1 && 
      stringdist("f", tolower(pairs3[i, "gender_code.x"])) ==0) {
  
    split <- stringdist( tolower(pairs3[i, "last_name.x"]), 
                         tolower(pairs3[i, "last_name.y"]) )
    split <- split/max(nchar(pairs3[i, "last_name.x"]), 
                       nchar(pairs3[i, "last_name.y"]))
    if(split>0.5){
      phi_new <- 1
    }
  }
  phi_l_sp1 <- c(0, 0, 0, 0)
  
  if (a == 1 && 
      stringdist("f", tolower(pairs3[i, "gender_code.x"])) ==0) {
    phi_l_sp1 <- dist_func(pairs3[i,"last_name.x"], 
                           pairs3[i,"last_name.y"]) 
  }    
    
  phi_l_sp2 <- c(0, 0, 0, 0)
  
  if (a == 1 && 
      stringdist("f", tolower(pairs3[i, "gender_code.x"])) ==0){
    phi_l_sp2 <- dist_func(pairs3[i,"last_name.x"], 
                           pairs3[i,"middle_name.y"])
  }   
  
  phi_birth_st_sp <- 0
  
  if (a == 1) {
  phi_birth_st_sp <-  stringdist(tolower(pairs3[i, "birth_state.x"]),
                                 tolower(pairs3[i, "birth_state.y"]),
                                 method = "jw")
  }
  
  phi_rule = if(a==1 ) {1} else 0
  
  f_rule_f <- if(a==1 && stringdist("f", tolower(pairs3[i, "gender_code.x"])) ==0) {1} else 0
  
  feature_table3[i, 1:34] <- c(pairs3$response[i], pairs3$indice[i],
                               phi_l, phi_f, phi_m, phi_addr, phi_age,
                               phi_res_city, phi_race, phi_ethnic,
                               phi_party, phi_gender, phi_birth_st,
                               phi_l_sp1, phi_l_sp2, phi_birth_st_sp,
                               phi_new, phi_rule, f_rule_f)
  
}

colnames(feature_table3) <- c("response", "indice",
                              "l_ed_i", "l_jw_r", "l_dm1_i", "l_dm2_i", 
                              "f_ed_i", "f_jw_r", "f_dm1_i", "f_dm2_i", 
                              "m_ed_i", "m_jw_r", "m_dm1_i", "m_dm2_i", 
                              "addr_i",
                              "age_f", "res_city_f", "race_f", 
                              "ethnic_f", "party_f", "gender_f", 
                              "birth_st_f",
                              "l_sp1_ed_i","l_sp1_jw_r", 
                              "l_sp1_dm1_i", "l_sp1_dm2_i", 
                              "l_sp2_ed_i","l_sp2_jw_r", 
                              "l_sp2_dm1_i", "l_sp2_dm2_i", 
                              "birth_st_sp_r", "l_new_f", 
                              "rule_f", "f_rule_f")
write.csv(feature_table3, file="./round3/feature_table3.csv")

