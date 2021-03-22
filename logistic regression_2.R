library(dplyr)
library(ggplot2)
raw_data <- read.csv("C:/Users/86923/Desktop/Gradute/lecture/DS552/Final_Project/compas-scores-two-years-violent.csv")

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, v_score_text, sex, priors_count, 
                    days_b_screening_arrest, v_decile_score, is_recid, two_year_recid) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>% 
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(v_score_text != 'N/A')

df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(v_score_text != "Low", labels = c("LowScore","HighScore")))
model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor + two_year_recid, family="binomial", data=df)
summary(model)