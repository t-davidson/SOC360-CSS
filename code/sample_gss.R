library(tidyverse)
library(haven)

data <- read_dta("data/GSS2018.dta")

data <- data %>% select(age, sex, race, realrinc, educ, hrs1, marital, bible, childs,  paeduc, maeduc, degree) %>%
    drop_na(degree) %>%
    filter(age <= 89) %>% haven::zap_labels() %>%
    mutate(sex = ifelse(sex == 1, 1, 0),
           race = as.factor(race),
           degree = ifelse(degree >= 3, 1, 0))
write_csv(data, "data/gss_sample.csv")

