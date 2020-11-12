# The packages
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

# The data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Cleaning up the data

fix_value(passwords)

passwords_category <- passwords %>%
  group_by(category) %>%
  summarize(avg_strength = mean(strength), 
            avg_offline = mean(offline_crack_sec))%>%
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

fix_value <- function(value, time_unit) {
  if (time_unit == "hours") {
    return(value/60)
  } else if (time_unit == "days") {
    return(value/1440)
  } else if (time_unit == "months") {
    return(value/43200)
  } else if (time_unit == "years") {
    return(value/518400)
  } else {
    return(value)
  }
}
  
  