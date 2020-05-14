

source("R/data.R")

fit <- glm(dx_heart ~ gender + max_heart_rate + exercise_angina + vessels, 
           data = data, 
           family = "binomial")

broom::tidy(fit) %>% 
  cbind(confint(fit)) %>% 
  knitr::kable(caption = "Summary Statistics", row.names = FALSE, digits = 3) %>% 
  kableExtra::kable_styling("bordered", full_width = FALSE)
