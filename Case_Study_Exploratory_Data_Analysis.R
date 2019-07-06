install.packages("openintro")
library(ggplot2)
library(openintro)
library(dplyr)
data("email")

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char), IQR(num_char)) %>% print()

# Create plot
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(mapping = aes(x = spam, y = log_num_char, group=spam)) +
  geom_boxplot()

email %>% group_by(spam) %>% summarize(IQR(exclaim_mess), sd(exclaim_mess), 
                                       median(exclaim_mess), mean(exclaim_mess)) %>% print()

# Compute center and spread for exclaim_mess by spam
email %>% group_by(spam) %>% summarize(IQR(exclaim_mess), median(exclaim_mess)) 



# Create plot for spam and exclaim_mess
email %>% mutate(log_exclaim_mess = log10(exclaim_mess)) %>% ggplot(mapping=aes(x=log_exclaim_mess)) + geom_histogram() + facet_wrap(~spam)
# Create plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")

# Test if images count as attachments
sum(email$image > email$attach)

# Question 1
email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarize(median(dollar))

# Question 2
email %>%
  filter(dollar > 10) %>%
  ggplot(aes(x = spam)) +
  geom_bar()
# Reorder levels
email$number <- factor(email$number, levels=c("none", "small", "big"))

# Construct plot of number
ggplot(email, mapping=aes(x=number)) +
  geom_bar() +
  facet_wrap(~spam)
