# The packages
library(tidyr)
library(dplyr)
library(readr)
library(gt)

# The data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Cleaning up the data
passwords_category <- passwords %>%
  group_by(category) %>%
  summarize(avg_strength = mean(strength), 
            avg_offline = mean(offline_crack_sec,
            avg_guess_min = mean(value)))%>%
  ungroup(time_unit) %>%
  drop_na()

password_examples <- passwords %>%
  select(rank, password, category) %>%
  group_by(category) %>%
  top_n(3, desc(rank)) %>%
  arrange(category) %>%
  mutate(rank = rep(c(1,2,3))) %>%
  pivot_wider(names_from = rank, values_from = password)

passwords_full <- passwords_category %>%
  left_join(password_examples, by = "category")

  
  
