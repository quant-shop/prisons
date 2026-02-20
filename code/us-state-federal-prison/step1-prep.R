# analysis

df_state = read.csv("/Users/nathanalexander/Dropbox/Projects/prisons/data/df_state.csv")
df_federal = read.csv("/Users/nathanalexander/Dropbox/Projects/prisons/data/df_federal.csv")
df_both = read.csv("/Users/nathanalexander/Dropbox/Projects/prisons/data/df_both.csv")

str(df_state)
str(df_federal)
str(df_both)

df <- rbind(df_state, df_federal, df_both)
df

# add indicators to df_both
df$greatd = ifelse(df$Year >= 1930, 1, 0) # create great depression flag
df$ww2 = ifelse(df$Year >= 1939, 1, 0) # create ww2 flag
df$kwar = ifelse(df$Year >= 1950, 1, 0) # create korean war flag
df$eisenhower = ifelse(df$Year >= 1953, 1, 0) # create dwight eisenhowar flag
df$civilrights = ifelse(df$Year >= 1964, 1, 0) # create civil rights act flag
df$mlk = ifelse(df$Year >= 1969, 1, 0) # create mlk indicator
df$regan = ifelse(df$Year >= 1981, 1, 0) # create ronald regan flag
df$date = as.Date(df$date)
df
str(df)

lm(Count ~ BlackPct, data = df_state)
