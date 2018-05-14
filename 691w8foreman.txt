# 691 Data Analysis for Data Science
# Foreman ch.3 walk through
# Eddie Chapman
# 3/16/18

# Load packages
library(dplyr)
library(tidytext)

# Load data
app <- read.csv("aboutMandrillApp.csv", as.is = 1, col.names = c("tweet"))
other <- read.csv("aboutOther.csv", as.is = 1, col.names = c("tweet"))
test <- read.csv("testTweets.csv", as.is = 1, col.names = c("tweet"))

# Combine all tweets. "group" preserves origin, "id" preserves tweet number.
tweets <- bind_rows("app" = app, "other" = other, "test" = test, .id = "group") %>%
  mutate(group = as.factor(group)) %>%      
  mutate(id = row_number())

# Strip tweets into single word tokens. Remove stop words. 
tokens <- tweets %>%       
  unnest_tokens(word, tweet)       

# Find token frequencies, probabilities.
model <- tokens %>%
  count(group, word, sort = TRUE) %>%
  group_by(group) %>%
  mutate(n = n + 1) %>%
  mutate(p = n / sum(n)) %>%
  mutate(ln = log(p)) %>%
  ungroup()    

# Seperate "app" token probabilities for easy joining. Rename "ln" as "p_app". 
app_prob <- model %>%
  filter(group == "app") %>%
  select(word, p_app = ln) 

# Seperate "other" tokens probabilities for easy joining. Rename "ln" as "p_other".
other_prob <- model %>%
  filter(group == "other") %>%
  select(word, p_other = ln)

# Select "test" tokens, match them with "app" and "other" probabilities. 
# For tokens that do not match either set, replace NA with 1/Total tokens,
# removing the tokens associated with the "test" set. 
test <- tokens %>%
  filter(group == "test") %>% 
  select(id, word) %>%
  left_join(app_prob) %>%
  left_join(other_prob) %>%
  replace(., is.na(.), (1/(nrow(tokens)-nrow(test))))

# Total the probabilties per tweet using the tweet ID. Add prediction column. 
# Connect results with original tweets for user inspection. 
results <- test %>%
  group_by(id) %>%
  summarise(p_app_total = sum(p_app),
            p_other_total = sum(p_other)) %>%
  mutate(prediction = if_else(p_app_total >= p_other_total, "other", "app")) %>%
  left_join(tweets) %>%
  select(-group)

results
