# Load required packages
library(dplyr)
require(ggplot2)
require(tidyverse)

set.seed(331)
n = 100

# define the race categories and percentages
political_groups <-  c("str democrat", "democract", "str republican", "republican", "moderate", "independent")
political_percentages <- c(0.10, 0.20, 0.20, 0.25, 0.10, 0.15)
racial_groups <- c("black", "white", "latinx", "asian", "other")
race_percentages <- c(0.12, 0.57, 0.11, 0.10, 0.10)

# Create a sample dataset
df <- data.frame(
  race = sample(racial_groups, size = n, replace = TRUE, prob = race_percentages),
  region = sample(c("southeast", "west", "northeast", "midwest", "south", "central"), n, replace = TRUE),
  party = sample(political_groups, size = n, replace = TRUE, prob = political_percentages),
  education = sample(0:20, n, replace = TRUE),
  income = rnbinom(n, 10, .5) + (40000)*df$education,
  random_health = sample(-35:35, n, replace = TRUE) + (1.4)*df$education + (1.75)*df$income,
  actual_health = (-35)*df$education * -0.1*df$income
)
df

# Calculate capabilities
df <- df %>%
  mutate(capability = (3/5)*education * (1/5)*actual_health * income)
df

cor(df$education, df$capability)
plot(df$education, df$capability)

cor(df$education, df$actual_health)
plot(df$education, df$actual_health)


# Summarize capabilities
summary(df$capability)

# Compare capabilities by education level
mean_cap_by_education <- df %>%
  group_by(education) %>%
  summarize(mean_capability = mean(capability))

# Plot the comparison
plot(mean_cap_by_education$education, mean_cap_by_education$mean_capability,
     xlab = "Education", ylab = "Mean Capability",
     type = "b", pch = 16, col = "blue", ylim = c(0, max(mean_cap_by_education$mean_capability) + 1))

# Compare capabilities by health
mean_cap_by_health <- df %>%
  group_by(actual_health) %>%
  summarize(mean_capability = mean(capability))

# Plot the comparison
plot(mean_cap_by_health$actual_health, mean_cap_by_health$mean_capability,
     xlab = "Health", ylab = "Mean Capability",
     type = "b", pch = 16, col = "red", ylim = c(0, max(mean_cap_by_health$mean_capability) + 1))


ggplot(df, aes(x = region)) +
  geom_bar()

ggplot(df, aes(x = education)) +
  geom_histogram(binwidth = 1)

ggplot(df, aes(x = education, y = capability)) +
  geom_point()

ggplot(df, aes(x = capability)) +
  geom_density()

cor(df$capability, df$education)
plot(df$capability, df$education)
cor(df$education, df$capability)
plot(df$education, df$capability)

ggplot(df, aes(x = race, y = capability)) +
  geom_boxplot()


ggplot(df, aes(x = capability, color = region)) +
  geom_density(linewidth = 0.75)

ggplot(df, aes(x = capability, color = race)) +
  geom_density(linewidth = 0.75)

ggplot(df, aes(x = capability, color = region, fill = region)) +
  geom_density(alpha = 0.5)


ggplot(df, aes(x = region, fill = region)) +
  geom_bar()

ggplot(df, aes(x = region, fill = race)) +
  geom_bar()

ggplot(df, aes(x = education, y = capability)) +
  geom_point()

ggplot(df, aes(x = education, y = capability)) +
  geom_point(aes(color = race))


ggplot(df, aes(x = education, y = capability)) +
  geom_point(aes(color = region, shape = race)) +
  facet_wrap(~race)

ggplot(df, aes(x = education, y = capability)) +
  geom_point(aes(color = region, shape = race))
