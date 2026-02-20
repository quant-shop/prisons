## Prisons {background-image="https://www.urban.org/sites/default/files/shutterstock_365427041_crop.jpg" background-opacity=0.5}

::: {.incremental}

- "In 2021, Black Americans were imprisoned at 5.0 times the rate of whites, while American Indians and Latinx people were imprisoned at 4.2 times and 2.4 times the white rate, respectively." ([The Sentencing Project](https://www.sentencingproject.org/reports/one-in-five-ending-racial-inequity-in-incarceration/), 2023)

- "One in five Black men born in 2001 is likely to experience imprisonment within their lifetime, a decline from one in three for those born in 1981. Pushback from policymakers threatens further progress in reducing racial inequity in incarceration." ([The Sentencing Project](https://www.sentencingproject.org/reports/one-in-five-ending-racial-inequity-in-incarceration/), 2023)

:::
  
## Prisons
  
The U.S. Bureau of Justice Statistics maintains records of federal and state prison populations[^carson].

```{r}
#| echo: false
#| output: false
#| warning: false
library(ggplot2)
library(readr)
library(dplyr)
race <- c("Black", "American Indian", "Latinx", "White", "Asian")
rate <- c(901, 763, 434, 181, 72)
prison_rates_ethnicity <- data.frame(race, rate)
```

```{r}
#| echo: false
#| output: true
#| warning: false

ggplot(prison_rates_ethnicity, aes(race, rate)) +
  geom_col(fill="darkred") +
  geom_text(aes(label=rate), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  labs(title= "Imprisonment Rates by Race, 2021",
       subtitle = "",
       x ="Race",
       y = "Per 100, 000 of US residents of each demographic group",
       caption = "Carson (2022). Prisoners in 2021.") + 
  theme(panel.background = element_blank())

```
[^carson]: Source: [Carson (2022). Prisoners in 2021 – Statistical tables. Bureau of Justice Statistics.](https://bjs.ojp.gov/library/publications/prisoners-2021-statistical-tables)

## Prisons

To support our framing of contemporary data, we take on an historical view[^prisons].

```{r}
#| echo: false
#| output: false
#| warning: false
# read in data and update
df <- read_csv("https://raw.githubusercontent.com/professornaite/prisons/main/data/fed_state_prison_pop_1926_1986_clean.csv")


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

df_federal = df %>% filter(Type == "Federal")
df_federal <- df_federal %>% 
  mutate(date = as.Date(Year, format = "%Y-%m-%d"))

df_both = df %>% filter(Type == "State + Federal")
df_both <- df_both %>% 
  mutate(date = as.Date(Year, format = "%Y-%m-%d"))

```

::: {.panel-tabset}

### Federal

Federal racialization data

```{r}
#| echo: false
#| output: false
#| warning: false
f1 <- ggplot(df_state, aes(x=date, y=WhitePct)) +
  geom_line(color="lightblue", size=2) +
  ggtitle("US Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as White") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()

f2 <- ggplot(df_state, aes(x=date, y=BlackPct)) +
  geom_line(color="red",size=2) +
  ggtitle("US Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as Black") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()
```


```{r}
#| echo: false
#| output: true
#| warning: false
f1 + f2
```


### State

State racialization data

```{r}
#| echo: false
#| output: false
#| warning: false

s1 <- ggplot(df_federal, aes(x=date, y=WhitePct)) +
  geom_line(color="lightblue", size=2) +
  ggtitle("US State Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as White") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()

s2 <- ggplot(df_federal, aes(x=date, y=BlackPct)) +
  geom_line(color="red",size=2) +
  ggtitle("US State Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as Black") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()

```

```{r}
#| echo: false
#| output: true
#| warning: false
s1 + s2
```

### Federal and State

Federal and State racialization data

```{r}
#| echo: false
#| output: false
#| warning: false

b1 <- ggplot(df_both, aes(x=date, y=WhitePct)) +
  geom_line(color="lightblue", size=2) +
  ggtitle("State and Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as White") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()

b2 <- ggplot(df_both, aes(x=date, y=BlackPct)) +
  geom_line(color="red",size=2) +
  ggtitle("State and Federal Prison Racialization (1926 - 1986)") +
  ylab("% Racialized as Black") +
  xlab("Year") + 
  ylim(1,100) +
  geom_line()
```

```{r}
#| echo: false
#| output: true
#| warning: false
b1 + b2
```

:::
  
  [^prisons]: For the year 1928, there was not prison data available.

## Racism vs. Anti-Blackness {background-image="https://upload.wikimedia.org/wikipedia/commons/c/c6/Blackboard.png?20200731124623" background-opacity=0.5}

In a 2020 NY Times article, kihana miraya ross chronicles the realities of anti-Blackness. [ross (2020)](https://www.nytimes.com/2020/06/04/opinion/george-floyd-anti-blackness.html) deals with the related but differing functions of racism and anti-Blackness.

::: {.incremental}

- ross notes that “'racism' fails to fully capture what black people in this country are facing."
  
  - ross continues by noting that "Anti-blackness is one way some black scholars have articulated what it means to be marked as black in an anti-black world."
  
  - Broadly, ross defines anti-Blackness as society's inability to recognize Black people's humanity.

:::


# Broad Implications

  - K-12 teaching and learning
  
    - This also exposes students to a diversity of problems and methods to solve those problems.

  - Computationally-focused research and training in higher education
  
    - Provides an equitable pathway and entry into "high information, high density" conversations.
    
  - Industry and professional organizations

# Gratitude for your time.
. . .

Thank you for joining us and citing today's presentation.

Alexander, N., Davis, K., Ghali, B., Stewart, Q., & La Cour, G. (2024, April 26). The Principles of Reconstruction: Still a Viable Route to Full Citizenship. *The 2024 Bob Moses Conference*. [Online](https://www.bobmosesconference.com/){target="blank"}.
