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
                          `Black Percentage` = col_number())) -> fed_state_prison_pop_1926_1986_clean
write_csv(fed_state_prison_pop_1926_1986_clean, "data/fed_state_prison_pop_1926_1986_clean.csv")

problems(fed_state_prison_pop_1926_1986_clean) # identify issues with df
View(fed_state_prison_pop_1926_1986_clean)
df <- fed_state_prison_pop_1926_1986_clean
df %>% 
  rename(Count = `Total`,
         TotalPct = `Total Percentage`,
         Type = `Prison Type`,
         WhitePct = `White Percentage`,
         BlackPct = `Black Percentage`) %>% 
  relocate(Year, Type) -> df

# subset data
df_state = df %>% filter(Type == "State")
df_state <- df_state %>% 
  mutate(date = as.Date(Year, format = "%Y-%m-%d"))
df_state
write_csv(df_state, "/Users/nathanalexander/Dropbox/Projects/prisons/data/df_state.csv")

df_federal = df %>% filter(Type == "Federal")
df_federal <- df_federal %>% 
  mutate(date = as.Date(Year, format = "%Y-%m-%d"))
df_federal
write_csv(df_federal, "/Users/nathanalexander/Dropbox/Projects/prisons/data/df_federal.csv")

df_both = df %>% filter(Type == "State + Federal")
df_both <- df_both %>% 
  mutate(date = as.Date(Year, format = "%Y-%m-%d"))
df_both
write_csv(df_both, "/Users/nathanalexander/Dropbox/Projects/prisons/data/df_both.csv")


## basic plot of state population counts
ggplot(df_state, aes(x=date, y=Count)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=1.5) +
  geom_point(size=3, color="#69b3a2") +
  ggtitle("US State Prison Counts (1926-1986)")

# basic plot of white vs. black population counts (state + federal)
p1 <- ggplot(df_both, aes(x=date, y=WhitePct)) +
  geom_line(color="lightblue", size=2) +
  ggtitle("US State and Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as White") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()
p1
p2 <- ggplot(df_both, aes(x=date, y=BlackPct)) +
  geom_line(color="red",size=2) +
  ggtitle("US State and Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as Black") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()
p2
# Display both charts side by side thanks to the patchwork package
p1 + p2



# create an interactive plot using plotly
ggplotly(p1)
ggplotly(p2)

