# The packages
library(tidyr)
library(dplyr)
library(readr)
library(gt)

# The data
# This data table has passwords, strength, and a whole bunch of other data
# It also includes 2 columns which would tell you how long it would take to guess a password by just testing random passwords
# One of the columns has the "value" and one of the columns has the "time_unit", like days, hours, etc.
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Cleaning up the data

# This creates a table that summarizes some data based on different categories of passwords
passwords_category <- passwords %>%
  group_by(category) %>%
  summarize(avg_strength = mean(strength), 
            avg_offline = mean(offline_crack_sec))%>%
  ungroup(time_unit) %>%
  drop_na()

# This table provides the top 3 most common passwords for each category from the data source
password_examples <- passwords %>%
  select(rank, password, category) %>%
  group_by(category) %>%
  top_n(3, desc(rank)) %>%
  arrange(category) %>%
  mutate(rank = rep(c(1,2,3))) %>%
  pivot_wider(names_from = rank, values_from = password)

# This table joins the passwords_category and password_examples tables so you can see everything at once
passwords_full <- passwords_category %>%
  left_join(password_examples, by = "category")

# I want to figure out how to find the average time to crack the password by randomly guessing
# So I created this "fix_value" function, which would theoretically make all values and time_units the same (minutes)
# And then I would need to summarize this data for each category
# I do not know how to apply this to the passwords_category table above so that I have all of the summarized data there

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
  
  
