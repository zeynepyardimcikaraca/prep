#download packages

install.packages('tidyverse')
library('tidyverse')
library('dplyr')

#load the data

songs <-read_csv('songs.csv')
streams <- read_csv('streams.csv')
country_codes <- read_delim('country_codes.csv', ';')

#What is the target unit of analysis?
  #sond_id
#Do columns seem complete?
  #no,coma

#Are there any values that do not make sense?
  #different character like chinese 
#Are “strings” properly encoded?
  #No chinese chracters not latin alphabet characters
#What are common columns across the three files?
  #user id and country codes

summary(songs)
summary(streams)
length(streams)
nrow(streams)


distinct(streams$song_id)

uniq_values <- streams %>% distinct(song_id)

group_id <- streams %>% group_by((song_id)) %>%
  summarize(u_id = n())

#The streams data
#set contains the top 200 songs on given date in a country.

cont <- streams %>% group_by(country, date) %>% summarize(how_many= n())

summary(streams)

summary(uniq_values)


summary(songs)


df1 <- songs %>% left_join(streams, by = 'song_id')
df2 <- df1 %>%  left_join(country_codes, by = c('country'= 'country_code_2_letter'))




df <- streams %>% 
  inner_join(songs, by = "song_id") %>%
  left_join(country_codes, by = c("country" = "country_code_2_letter"), suffix = c("_streams", "_cc"))

uniq <- df %>% 
  distinct(song_id)
nrow(uniq)

write_csv(df, "merged_df.csv")
df <- read_csv("merged_df.csv")



df_unique <- df %>% distinct(song_id, .keep_all = TRUE)

df_merged <- read_csv("merged_df.csv")

df_no_duplicates.csv <- df_merged %>% filter(!duplicated(df_merged))

write.csv(df,'df_no_duplicates.csv')

#Implement this data imputation strategy using the filter, groupby, 
#and summarise functions from the dplyr package and the result as 
#df_imputed.csv. 
#We recommend first computing the imputation values
#(for each country across the days), and 
#then replacing missing values
#in the popularity column with their matching counterparts. 

#Evaluate your results in terms of the pros and cons of this 
#approach and try to come up with an alternative imputation 
#strategy (incl. arguments why you prefer that approach).


df_avg <- df %>% group_by(country,date) %>% 
  summarize(pop_avg= median(popularity))



# input
df <- read_csv("df_no_duplicates.csv")

# transformation
imputation_values <- df %>% filter(!is.na(popularity)) %>% 
  group_by(country, date) %>% 
  summarise(popularity_filled = median(popularity))

df_joined <- df %>% left_join(imputation_values, by = c("country" = "country", "date" = "date"))

df <- df_joined %>% mutate(popularity_filled = ifelse(is.na(popularity), popularity_filled, popularity))


#Create a data frame dates with two columns: 
#date (i.e., the sequence of dates) and counter
#(i.e., a numeric value between 1 and 350). 
#Tip: you may first need to convert the dates column into
#character format (as.charcter(date)). 
#3. Merge df and dates and inspect your results
#(which type of join do you use?)

library(lubridate)

dates <- df %>% mutate(date= as.Date(date))


# Step 1
date_han <- seq(min(as.Date(df$date)), max(as.Date(df$date)), by = "day")

# Step 2
counter <- 1:length(date)
date_hannes <- data.frame(counter= counter, date=date_han)

# Step 3
df <- df %>% inner_join(date_hannes, by = "date")


#EX6#   Add a ranking column to df using the dplyr package and export the result as df_ranking.csv. 
#Although it is unlikely that some tracks have exactly the same number of streams, 
#think about how you will deal with these edge cases (tip: run ?rank to look up the documentation). 
#Does your ranking always run from 1 to 200? Why is that? Investigate the root cause and fix it!

###LOOK EXCERCISE 5 AND 6 AGAIN

# input
df <- read_csv("df_imputed.csv")

# transformation
df <- df %>% group_by(country, date) %>%
  mutate(ranking = rank(-streams, ties.method = "random"))

# The maximum ranking is 400 (rather than 200)
# Once we inspect one of the listings of df[df$ranking >= 200, ] we find that these records typically have redundant data somewhere hidden in between. For example, for df[(df$date == "2020-05-17") & (df$country == "HK"),] we discover that ranking 59 appears 17 times (!) which can be attributed to missing values `song_id` 

# remove rows without song_id (if you then recreate the ranking column you'll come to the conclusion the maximum ranking has become 200!)
df <- df %>% filter(!is.na(song_id))

# output
write_csv(df, "df_ranking.csv")


text <- "Hello World this is R"
split_text <- strsplit(text, " ")
print(split_text)

splitted <- strsplit(df1$genres, ",", fixed = TRUE)


genres <- df1 %>% 
genres_example_split <- split(df1$genres, ",")
genres_example_split[[1]]



###excersise 5 ten sonrasina erdemle bak ozellikle genres kismina 





























