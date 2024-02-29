library(readr)
library(tidyverse)
library(haven)
library(knitr)
library(tibble)
library(dplyr)
library(gapminder) 
library(kableExtra)


Arab1 = read_csv('Arab_Barometer_Wave_6_Part_1_ENG_RELEASE.csv')
Arab2 = read_csv('Arab_Barometer_Wave_6_Part_2_ENG_RELEASE.csv')
Arab3 = read_csv('Arab_Barometer_Wave_6_Part_3_ENG_RELEASE.csv')


#DATA FILTERED BY COUNTRY 
filtered_Arab1 = Arab1 |> 
  filter(COUNTRY %in% c(1, 13, 21))
filtered_Arab2 = Arab2 |> 
  filter(COUNTRY %in% c(1, 13, 21))
filtered_Arab3 = Arab3 |> 
  filter(COUNTRY %in% c(1, 13, 21))
combined_dataset = bind_rows(filtered_Arab1, filtered_Arab2, filtered_Arab3) 

combined_dataset <- replace(combined_dataset, combined_dataset == 98 | combined_dataset == 99, NA)

#COUNTRY VARIABLE 
combined_dataset = combined_dataset |> 
  rename(c_name = COUNTRY) |> 
  mutate(c_name = case_when(
    c_name == 1 ~ "Algeria",
    c_name == 13 ~ "Morocco",
    c_name == 21 ~ "Tunisia",
    TRUE ~ as.character(c_name)))

#RESPONDANT ID 
combined_dataset = combined_dataset |> 
  rename(r_id = ID)

#SURVEY WEIGHT 
combined_dataset = combined_dataset |> 
  rename(r_swt = WT)

#ARABBAROMETER
combined_dataset = combined_dataset |> 
  mutate(source = "ArabBarometer")

#FEMALE INDICATOR 
combined_dataset = combined_dataset |> 
  rename(r_female2 = Q1002)

labels = c("Female", "Not Female")
combined_dataset <- combined_dataset |> 
  mutate(r_female = recode(r_female2, `1` = 0, `2` = 1))  |> 
  mutate(r_female_factor = factor(r_female, levels = 0:1, labels = labels))


#DATA FILTERED BY MONTH AND YEAR 
combined_dataset = combined_dataset |> 
  mutate(r_year = substr(`DATE`, 1, 4),   
         r_month = substr(`DATE`, 6, 7))  


#POLITCAL IDEAOLOGY 
labels = c("US", "Russia", "China", "UK")
combined_dataset <- combined_dataset |> 
  rename(p_ideology = Q14COVID19) |> 
  mutate(p_ideology_factor = factor(p_ideology, levels = 1:4, labels = labels))

#POLITICAL INTEREST
labels = c("Throughout the day", 
           "At least once daily", 
           "Several times a week", 
           "Once a week", 
           "Less than once a week", 
           "I do not use the Internet")
combined_dataset = combined_dataset |> 
  mutate(p_interest = coalesce(Q409, Q409_NEW)) |> 
  mutate(p_interest_factor = factor(p_interest, levels = 1:6, labels = labels))


#FACTOR
FctWhen= function(...){
  args = rlang::list2(...)
  rhs = map(args, rlang::f_rhs)
  cases = case_when(!!!args)
  exec(fct_relevel, cases, !!!rhs)}

#EVALUATION OF NATURAL ECONOMEY 
labels = c("Very Good","Good","Bad", "Very Bad")
combined_dataset = combined_dataset |> 
  rename(p_economy = Q101) |> 
  mutate(p_economy_factor = factor(p_economy, levels = 1:4, labels = labels))

#CORE VALUES
labels = c("For people like me, it doesn't matter what kind of government we have",
            "Under some circumstances, a non-democratic government can be preferable",
            "Democracy is always preferable to any other kind of government")
combined_dataset = combined_dataset |> 
  rename(core_values = Q516A)  |> 
  mutate(core_values_factor = factor(core_values, levels = 1:3, labels = labels))

#ESSENTIAL DEMOCRACY
labels = c("Absolutely Essential", "Somewhat Essential", "Not Very Essential", "Not at all Essential")
combined_dataset = combined_dataset |> 
  mutate(d_essential = coalesce(Q512A3_1, Q512A3_2, Q512A3_5)) |> 
  mutate(d_essential_factor = factor(d_essential, levels = 1:4, labels = labels))

#COUNTRY GOVERNENCE
labels = c("Strongly Agree", "Somewhat Agree", "Somewhat Disagree", "Strongly Disagree")
combined_dataset = combined_dataset |> 
  mutate(c_govern = coalesce(Q533_4, Q533_5)) |> 
  mutate(c_govern_factor = factor(c_govern, levels = 1:4, labels = labels))


#INSTITUTIONAL TRUST
labels = c("A great deal of trust", "Quite a lot of trust", "Not a lot of trust", "No trust at all")
combined_dataset = combined_dataset |> 
  mutate(i_trust = coalesce(Q201A_1, Q201A_2, Q201A_6A_LIB,Q201A_6B_LIB)) |> 
  mutate(i_trust_factor = factor(i_trust, levels = 1:4, labels = labels))

#FINAL DATA
ARAB123 = combined_dataset |> 
  select(c_name, source, r_id, r_year, r_month, r_swt, r_female, p_ideology, p_interest, p_economy, core_values, d_essential, c_govern, i_trust)
view(ARAB123) 


write.csv(combined_dataset, "dataset.csv", row.names = FALSE)
write.csv(ARAB123, "ARAB123.csv", row.names = FALSE)
