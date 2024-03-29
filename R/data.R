#### packages ####

library(tidyverse)
library(magrittr)


#### load data #### 

data <- read.csv("data/heart.csv")


#### data processing ####

colnames(data) <- c("age", 
                    "gender", 
                    "chest_pain_type", 
                    "rest_bp", 
                    "chol", 
                    "fbs", 
                    "rest_ecg", 
                    "max_heart_rate", 
                    "exercise_angina", 
                    "oldpeak", 
                    "slope", 
                    "vessels", 
                    "thal", 
                    "dx_heart")

data %<>% 
  mutate(gender = case_when(
    gender == 1 ~ "male", 
    gender == 0 ~ "female"), 
    fbs = case_when(
      fbs == 1 ~ "true", 
      fbs == 0 ~ "false"), 
    exercise_angina = case_when(
      exercise_angina == 1 ~ "yes", 
      exercise_angina == 0 ~ "no"))


#### set reference level #### 

data$gender %<>% as.factor() %>% relevel(ref = "male")

data$fbs %<>% as.factor() %>% relevel(ref = "false")

data$exercise_angina %<>% as.factor() %>% relevel(ref = "no")


#### add bins ####

data %<>%
  mutate(rest_bp_bins = cut(rest_bp, breaks = seq(90, 200, 10)), 
         chol_bins = cut(chol, breaks = seq(100, 700, 45)), 
         max_heart_rate_bins = cut(max_heart_rate, seq(70, 220, 15)), 
         oldpeak_bins = cut(oldpeak, seq(0, 6.4, 0.8), include.lowest = TRUE))


# > 1. age
# > 2. sex
# > 3. chest pain type (4 values)
# > 4. resting blood pressure
# > 5. serum cholestoral in mg/dl
# > 6. fasting blood sugar > 120 mg/dl
# > 7. resting electrocardiographic results (values 0,1,2)
# > 8. maximum heart rate achieved
# > 9. exercise induced angina
# > 10. oldpeak = ST depression induced by exercise relative to rest
# > 11. the slope of the peak exercise ST segment
# > 12. number of major vessels (0-3) colored by flourosopy
# > 13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
