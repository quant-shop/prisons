library(readr)
library(dplyr)
library(tidyverse)

# Create the year vector from 1926 to 2022
years <- 1925:2022

# Generate random pseudo data for the percent_federal column
test_federal <- runif(length(years), min = 0, max = 100)

# Generate random pseudo data for the percent_state column, ensuring it is generally 12 to 15 times higher than percent_federal
test_state <- test_federal * runif(length(years), min = 5, max = 15)

# Scale the percent_state and percent_federal values to ensure they sum up to 100
test_total <- test_state + test_federal
test_state <- test_state / test_total * 100
test_federal <- test_federal / test_total * 100

# Create the data frame
test_data <- data.frame(year = years,
                          test_total = test_state + test_federal,
                          test_state = test_state,
                          test_federal = test_federal)

# Print the data frame
print(test_data)

#### load real data

url <- "https://raw.githubusercontent.com/quant-shop/prisons/main/data/incarceration_counts_and_rates_by_type_over_time_raw.csv"
data <- read_csv(url)
str(data)

# Transpose the real data
transposed_data <- t(data)
df <- as.data.frame(transposed_data)
df

df <-  df %>% 
  rownames_to_column('Year') %>% 
  select(-(V7:V28))
str(df)
df <- as_tibble(df)
df <- df %>% 
  rename(year = Year) %>% 
  rename(state_prisons_count = V1) %>% 
  rename(federal_prisons_count = V2) %>% 
  select(-V3) %>% 
  rename(state_prisons_rates = V4) %>% 
  rename(federal_prisons_rates = V5) %>% 
  select(-V6) %>% 
  slice(-1)
df
tail(df)



# Merge the data frames by the variable 'year'
merged_df <- merge(test_data, df, by = "year")

# Print the merged data frame
print(merged_df)
df2 <- as_tibble(merged_df)
df2
tail(df2)
str(df2)
df2
