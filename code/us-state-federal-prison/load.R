# prison population 1926 : 1986
# load packages and libraries
library(readr)
library(ggplot2)
library(dplyr)
install.packages("patchwork") # To display 2 charts together
library(patchwork)
install.packages("plotly")
library(plotly)

# read in data and update
read_csv("data/fed_state_prison_pop_1926_1986.csv",
         col_types = cols(Year = col_date(format = "%Y"),
                          Total = col_number(),
                          `Total Percentage` = col_number(),
                          `Prison Type` = col_factor(levels = c("State + Federal", "State", "Federal")), 
                          `White Percentage` = col_number(),
                          `Black Percentage` = col_number())) -> fed_state_prison_pop_1926_1986
problems(fed_state_prison_pop_1926_1986) # identify issues with df
View(fed_state_prison_pop_1926_1986)
df <- fed_state_prison_pop_1926_1986
df %>% 
  rename(Count = `Total`,
         TotalPct = `Total Percentage`,
         Type = `Prison Type`,
         WhitePct = `White Percentage`,
         BlackPct = `Black Percentage`) %>% 
  relocate(Year, Type) -> df

# subset data
df_state = df %>% filter(Type == "State")
df_state

df_federal = df %>% filter(Type == "Federal")
df_federal

df_both = df %>% filter(Type == "State + Federal")
df_both

## basic plot of state population counts
ggplot(df_state, aes(x=Year, y=Count)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="#69b3a2") +
  ggtitle("US State Prison Counts (1926-1986)")

# basic plot of white vs. black population counts (state + federal)
p1 <- ggplot(df_both, aes(x=Year, y=WhitePct)) +
  geom_line(color="lightblue", size=2) +
  ggtitle("White Percentage")
p1
p2 <- ggplot(df_both, aes(x=Year, y=BlackPct)) +
  geom_line(color="darkblue",size=2) +
  ggtitle("Black Percentage")
p2
# Display both charts side by side thanks to the patchwork package
p1 + p2

# create an interactive plot using plotly
ggplotly(p1)
ggplotly(p2)

