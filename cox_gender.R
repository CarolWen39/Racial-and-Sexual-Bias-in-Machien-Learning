library(survival)
library(ggfortify)
library(ggplot2)
library(survminer)

data <- filter(filter(read.csv("C:/Users/86923/Desktop/Gradute/lecture/DS552/Final_Project//cox-parsed.csv"), score_text != "N/A"), end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]



female <- filter(data, sex == "Female")
male   <- filter(data, sex == "Male")
male_fit <- survfit(f, data=male)
female_fit <- survfit(f, data=female)

grid.arrange(plotty(female_fit, "Female"), plotty(male_fit, "Male"),ncol=2)