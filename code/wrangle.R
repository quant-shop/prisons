library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(here)


here::i_am("wrangle.R")
sentences <- read.csv("../data/sentences.csv")
str(sentences)

CENSUS_API_KEY='3cc2bbb655ec6af727c987df3a4559e41effe377'
library(tidycensus)
library(tidyverse)


sentences2 <- sentences %>%
  mutate(Outcome_Category = case_when(
    Outcome.of.Sentence %in% c("Executed",
                               "Executed for a Different Crime", 
                               "Executed by a Different State", 
                               "Executed for a Different Crime Pending Retrial or Resentencing"
    ) ~ "Executed",
    
    Outcome.of.Sentence %in% c("Resentenced to Death", 
                               "Resentenced to Life or Less", 
                               "Resentenced to Time Served"
    ) ~ "Resentenced to Life or Less",
    
    Outcome.of.Sentence %in% c("Exonerated",
                               "Grant of Relief (Retrial/Resentencing Pending)", 
                               "Grant of Relief (Never Retried)", 
                               "Grant of Relief (Subject to Appeal)",
                               "Conviction Voided", 
                               "Conviction Voided, Convicted and Sentenced to Life or Less in Another Jurisdiction",
                               "Not Guilty by Reason of Insanity", 
                               "Acquitted, Convicted and Sentenced to Death in Another Jurisdiction"
    ) ~ "Exonerated",
    
    Outcome.of.Sentence %in% c("Died on Death Row",
                               "Died Pending Retrial or Resentencing") ~ "Died (Other)", 
    
    Outcome.of.Sentence %in% c("Active Death Sentence")  ~ "Active Death Sentence",
    
    Outcome.of.Sentence %in% c("Sentence Commuted (Administrative)", 
                               "Sentence Commuted", 
                               "Sentence was Commuted to life without parole", 
                               "Sentence was Commuted to time served"
    ) ~ "Sentence Commuted",
    
    TRUE ~ NA_character_  # For outcomes that don't fit or need review
  )
  ) %>% 
  relocate(defendant, 
           sentence, 
           Year, 
           Gender, 
           Races, 
           Outcome.of.Sentence, 
           Outcome_Category, 
           Current.Case.Status, 
           Region,
           State.Abbreviation) %>% 
  mutate(Multi.sentence.identifier = as.numeric(Multi.sentence.identifier))

# drop all races except for white and black
sentences3 <- sentences2 %>%
  as.tibble() %>% 
  filter(Races %in% c("White", "Black")) %>% 
  rename(outcome_original = Outcome.of.Sentence) %>% 
  rename(outcome_category = Outcome_Category) %>% 
  rename(race = Races) %>% 
  rename(year = Year) %>% 
  rename(gender = Gender) %>% 
  rename(state = State.Abbreviation) %>% 
  rename(region = Region) %>% 
  rename(name = Name) %>% 
  mutate(multiple_sentences = !is.na(Multi.sentence.identifier))

# count rows per defendant
sentences3 %>%
  count(defendant) %>%
  filter(n > 1) %>%
  arrange(desc(n))

sentences3_clean <- sentences3 %>% 
  # Group by defendant
  group_by(defendant) %>%
  # Count original rows and keep most recent record
  mutate(count_of_records = n()) %>%
  arrange(desc(year)) %>%
  slice(1) %>%
  # Ungroup
  ungroup()

# count rows per defendant
sentences3_clean %>%
  count(defendant) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# create two data sets by cases
df <- sentences3_clean %>% 
  relocate(defendant, year, gender, race, outcome_category, outcome_original, region, state, multiple_sentences, count_of_records, name)
single <- df %>% 
  filter(count_of_records == 1)
multiple <- df %>% 
  filter(count_of_records > 1)
df
single
multiple


glimpse(df) %>% 
  select(Name = "name",
         Year = "year",
         multiple_sentences,
         count_of_records,
         Category = "outcome_category") %>% 
  as.tibble()


# gather population estimates from Census data
# Filter for Black/White defendants and calculate counts
sentencing_race <- df %>%
  filter(race %in% c("Black", "White")) %>%  # Keep only Black/White
  count(race) %>%
  mutate(race = tolower(race))  # Match ACS race labels ("black", "white")

# Get race_pop including ALL racial groups (for accurate denominator)
race_pop_all <- get_acs(
  geography = "us",
  variables = c(
    white = "B02001_002",
    black = "B02001_003",
    asian = "B02001_005",
    hispanic = "B03003_003"  # Add other groups as needed
  ),
  year = 2023,
  survey = "acs5"
) %>%
  select(race = variable, us_pop = estimate)

# Calculate disparity using total US population denominator
disparity_df <- sentencing_race %>%
  left_join(
    race_pop_all %>% filter(race %in% c("black", "white")),
    by = "race"
  ) %>%
  mutate(
    us_pct = us_pop / sum(race_pop_all$us_pop),  # Key change: total pop denominator
    sentenced_pct = n / sum(n),
    disparity_ratio = sentenced_pct / us_pct
  )

# Result includes only Black/White but uses full population denominator

# view the df
disparity_df



# Exonerated Graph         
ggplot(subset(sentences2, Outcome_Category == "Exonerated"), aes(x = Outcome_Category, fill = Races)) +
    geom_bar(position = "dodge") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Executed Graph
ggplot(subset(sentences2, Outcome_Category == "Executed"), aes(x = Outcome_Category, fill = Races)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Died Other Graph
ggplot(subset(sentences2, Outcome_Category == "Died (Other)"), aes(x = Outcome_Category, fill = Races)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sentence Commuted
ggplot(subset(sentences2, Outcome_Category == "Sentence Commuted"), aes(x = Outcome_Category, fill = Races)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Active Death Sentence
ggplot(subset(sentences2, Outcome_Category == "Active Death Sentence"), aes(x = Outcome_Category, fill = Races)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



exonerated_props <- sentences2 %>%
  filter(Outcome_Category == "Exonerated") %>%
  group_by(Races) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

exonerated_props

commuted_props <- sentences2 %>%
  filter(Outcome_Category == "Sentence Commuted") %>%
  group_by(Races) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

commuted_props