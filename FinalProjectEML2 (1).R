#Team 3 Data Analysis - Troy et al. 2017 Replication & Extension Study
library(readr)
library(dplyr)
install.packages("sjPlot")
library(sjPlot)
install.packages("tidyverse")
library(tidyverse)
install.packages("pwr")
library(pwr)

#new data frame
new_df <- data_frame(Replication_of_Troy_et_al_2017_Study_1_at_Pomona_College_April_26_2024_13_21)

#cleaning data, checking for exclusion criteria (survey duration & survey completion) - deleting those who did not finish and/or completed in less than 120 seconds,
#as well as first 2 rows from unnecessary Qualtrics data

rows_to_delete <- c(1, 2, 111, 133, 134, 135, 136, 137)
new_df <- new_df[-rows_to_delete, ]

#running descriptives statistics
mean_duration <- mean(as.numeric(new_df$`Duration (in seconds)`), na.rm = TRUE)
sd_duration <- sd(as.numeric(new_df$`Duration (in seconds)`), na.rm = TRUE)

#demographics coding, new data frame for text

new_df2 <- data_frame(Replication_of_Troy_et_al_2017_Study_1_at_Pomona_College_April_26_2024_13_23)
rows_to_delete2 <- c(1, 2, 111, 133, 134, 135, 136, 137)
new_df2 <- new_df2[-rows_to_delete2, ]

#age stats
mean_age <- mean(as.numeric(new_df2$Q2))
sd_age <- sd(as.numeric(new_df2$Q2)) 
range_age <- range(as.numeric(new_df2$Q2))

#demographics table
table_gender <- table(new_df2$Q3)
table_race_ethnicity <- table(new_df2$Q5)
table_SES <- table(new_df2$Q6)

#SES stats
mean_SES <- mean(as.numeric(new_df$Q6))
sd_SES <- sd(as.numeric(new_df$Q6))

#separate SES BINS

new_df$SES2 <- case_match(new_df$SES,
                           1 ~ "Low_SES",
                           2 ~ "Low_SES",
                           3 ~ "Low_SES",
                           4 ~ "Low_SES",
                           5 ~ "Middle_SES",
                           6 ~ "Middle_SES",
                           7 ~ "Middle_SES",
                           8 ~ "Middle_SES",
                           9 ~ "High_SES",
                           10 ~ "High_SES",
                           11 ~ "High_SES",
                           12 ~ "High_SES")


#COMPOSITE MEASURE SCORES

#composite score for CRA

#loop to turn characters into numbers per column 

for (i in 1:8) {
  col_name <-paste0("Q8_", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}

new_df$CRA_score <- rowMeans(new_df[,c("Q8_1", "Q8_2","Q8_3","Q8_4","Q8_5", "Q8_6", "Q8_7", "Q8_8")])


#composite score for CES-D 

#loop to turn characters into numbers per column# 

for (i in 1:5) {
  col_name <-paste0("Q10_", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}

new_df$CESD_score <- rowMeans(new_df[,c("Q10_1", "Q10_2","Q10_3","Q10_4","Q10_5")])


#life stress, change to numeric values

for (i in 1:4) {
  col_name <-paste0("Q12_", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}


# life stress, reverse scoring for items 2 & 3

new_df$Q12_2 <- case_match(new_df$Q12_2,
                           1 ~ 5,
                           2 ~ 4,
                           3 ~ 3,
                           4 ~ 2,
                           5 ~ 1)

new_df$Q12_3 <- case_match(new_df$Q12_3,
                      1 ~ 5,
                      2 ~ 4,
                      3 ~ 3,
                      4 ~ 2,
                      5 ~ 1)


#life stress, composite score

new_df$LSscore <- rowMeans(new_df[,c("Q12_1", "Q12_2","Q12_3","Q12_4")])


#make numeric for BDI-II

for (i in 16:36) {
  col_name <-paste0("Q", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}


#Reversing scoring for 2 items in BDI-II

new_df$Q31 <- case_match(new_df$Q31,
                           1 ~ 0,
                           2 ~ 1,
                           3 ~ 1,
                           4 ~ 2,
                           5 ~ 2,
                           6 ~ 3,
                           7 ~ 3)

new_df$Q33 <- case_match(new_df$Q33,
                         1 ~ 0,
                         2 ~ 1,
                         3 ~ 1,
                         4 ~ 2,
                         5 ~ 2,
                         6 ~ 3,
                         7 ~ 3)  

#reverse scoring
for (i in 16:30) {
  col_name <-paste0("Q", i)
  new_df[[col_name]] <- case_match(new_df[[col_name]],
                                   1 ~ 0,
                                   2 ~ 1,
                                   3 ~ 2,
                                   4 ~ 3)
}

new_df$Q32 <- case_match(new_df$Q32,
                         1 ~ 0,
                         2 ~ 1,
                         3 ~ 2,
                         4 ~ 3)


for (i in 34:36) {
  col_name <-paste0("Q", i)
  new_df[[col_name]] <- case_match(new_df[[col_name]],
                                   1 ~ 0,
                                   2 ~ 1,
                                   3 ~ 2,
                                   4 ~ 3)
}

                        
#composite score for BDI-II

new_df$BDI2_score <- rowSums(new_df[,c("Q16", "Q17","Q18","Q19","Q20", 
                                       "Q21", "Q22","Q23","Q24","Q25",
                                       "Q26", "Q27","Q28","Q29","Q30",
                                       "Q31", "Q32","Q33","Q34","Q35","Q36")])


#reverse scoring for Controllability scale

for (i in 1:6) {
  col_name <-paste0("Q13_", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}

for (i in 1:6) {
  col_name <-paste0("Q13_", i)
  new_df[[col_name]] <- case_match(new_df[[col_name]],
                                   1 ~ 7,
                                   2 ~ 6,
                                   3 ~ 5,
                                   4 ~ 4,
                                   5 ~ 3,
                                   6 ~ 2,
                                   7 ~ 1)
}

for (i in 1:6) {
  col_name <-paste0("Q14_", i)
  new_df[[col_name]] <- as.numeric(new_df[[col_name]])
}

for (i in 1:6) {
  col_name <-paste0("Q14_", i)
  new_df[[col_name]] <- case_match(new_df[[col_name]],
                                   1 ~ 7,
                                   2 ~ 6,
                                   3 ~ 5,
                                   4 ~ 4,
                                   5 ~ 3,
                                   6 ~ 2,
                                   7 ~ 1)
}


# Composite score for controllability

new_df$controllability <- rowMeans(new_df[,c("Q13_1","Q13_2","Q13_3","Q13_4","Q13_5", 
                                       "Q13_6", "Q14_1","Q14_2","Q14_3","Q14_4",
                                       "Q14_5", "Q14_6")])
                             

# Replication analysis
new_df <- new_df %>% 
  rename(SES = Q6)
    
new_df$SES <- as.numeric(new_df$SES)

new_df %>% 
  group_by(SES) %>% 
  summarise( percent = 100 * n() / nrow( new_df) )

model1 <- lm(CESD_score ~ CRA_score * SES + LSscore, data = new_df)
summary(model1)

model1_b <- lm(CESD_score ~ CRA_score * SES, data = low_SES_population)
summary(model1_b)

high_SES_population <- filter(new_df, SES2 == "High_SES")
model1_c <- lm(CESD_score ~ CRA_score * SES, data = high_SES_population)
summary(model1_c)

# Extension analysis

low_SES_population <- filter(new_df, SES2 == "Low_SES")

extension_model1 <- lm(controllability ~ CRA_score, data = low_SES_population)
summary(extension_model1)

extension_model2 <- lm(BDI2_score ~ CRA_score, data = low_SES_population)
summary(extension_model2)

extension_model3 <- lm(BDI2_score ~ CRA_score + controllability, data = low_SES_population)
summary(extension_model3)

                             
                             
                             
                             
                             
                             
                             
                             