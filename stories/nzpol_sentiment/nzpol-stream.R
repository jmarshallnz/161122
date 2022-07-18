# these packages need to be already installed in order to be loaded and used
library(rtweet) # access twitter API - should work without developer credentials
library(dplyr) # general data handling commands
library(lubridate) # specialist date handling
library(ggplot2) #graphmaking
library(httpuv)
library(scales)
library(tidytext) #for splitting character strings and sentiment analysis

# For the RNZ story by Kate Newton:
#
# https://www.rnz.co.nz/news/national/421955/bad-vibes-rating-political-scandals-by-twitter-toxicity

the_now <- gsub("\\.", "_", make.names(Sys.time())) #files datestamped so if script used again the new files get new filenames
local_tz <- "Pacific/Auckland"

# storage folder for account details in working directory
if(!dir.exists("nzpol-stream")){
  dir.create("nzpol-stream")
}


filepath <- paste0("nzpol-stream/", the_now, ".csv") # or substitute file name if previously downloaded


result <- tryCatch(search_tweets("#nzpol", include_rts = F, n = 18000), # Exclude retweets; 18000 is max returned in single call but you can ramp this up - see rtweet documentation 
                   error = function(e) NULL, warning = function(w) NULL)

if(!is.null(result)){
  write_as_csv(result, filepath)}

filepath <- "nzpol-stream/X2020_07_23_16_10_37.csv"
tweets_today <- read_csv(here::here(file.path("stories/nzpol_sentiment", filepath)))

tweets_today <- tweets_today %>% 
  select(user_id, status_id, created_at, 
         screen_name, text, source, is_quote, 
         is_retweet, hashtags, retweet_text, 
         retweet_screen_name, retweet_user_id, 
         retweet_location, retweet_created_at) %>% 
  arrange(created_at)


tidy_today <- tweets_today %>% 
  unnest_tokens(word, text) %>% #use tidytext pkg to split each tweet into one row per word
  anti_join(stop_words) %>% # get rid of stopwords aka common words such as 'the, 'a'
  filter(!str_detect(word, "http|https|t\\.co|amp|[:digit:]+")) %>% # and get rid of common web jargon
  mutate(day_of = floor_date(created_at, "day"), 
         hour_of = floor_date(created_at, "hour")) # create columns to categorise by day and hour

#--------

# This next bit was just for fun

words_today <- tidy_today %>% #find most common words
  count(word, day_of, sort = TRUE)

top_today <- words_today %>% # mutate to find the top three words used each day
  filter(!str_detect(word, "nzpol")) %>% 
  group_by(day_of) %>% 
  mutate(rank=rank(desc(n))) %>% 
  filter(rank<=3)

#---------
sentiments <- tidy_today %>% 
  inner_join(get_sentiments("afinn")) %>% # using AFINN library (gives words a positivity/negativity rating on a scale of +5 to -5); see tidytext documentation for alternatives
  select(word, day_of, hour_of, value)

write_csv(sentiments, "sentiments.csv")

library(tidyverse)
library(scales)
local_tz <- "Pacific/Auckland"

sentiments <- read_csv(here::here("stories/nzpol_sentiment/sentiments.csv"))

sentiments_today <- sentiments %>% # find hourly overall sentiment 
  group_by(index = hour_of) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(color = case_when(sentiment < 0 ~ "negative",
                           sentiment > 0 ~ "positive"))

#graph
ggplot(data=sentiments_today) + 
  geom_col(mapping=aes(index, sentiment, fill = color)) + 
  scale_fill_manual(values = c("red", "blue")) + 
  scale_x_datetime(breaks = breaks_width("1 day", -43200),  # offsetting breaks by minus twelve hours (expressed in seconds)
                   labels = date_format("%b%d %H:%M",tz = local_tz))

ggsave("nzpol-week.png", width = 25, height = 10, units = "cm")

# JM version to better match actual:
sentiments_today %>% filter(!is.na(color)) %>% #graph
  ggplot(aes(index, sentiment, fill = as_factor(color))) + 
  geom_col() + 
  scale_fill_manual(values = c(positive = "#02D45E", negative = "#DE1920")) + 
  scale_x_datetime(breaks = breaks_width("1 day", -43200),  # offsetting breaks by minus twelve hours (expressed in seconds)
                   labels = date_format("%B %d",tz = local_tz)) +
  scale_y_continuous(breaks = 0) +
  labs(fill = "SENTIMENT", title = "BAD VIBES",
       subtitle = "Increasingly negative tweets using #nzpol over the last week",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(text = element_text(colour = "white"),
        plot.background = element_rect(fill='black'),
        panel.grid.major.x = element_line(colour = "grey30"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'grey50'),
        legend.title = element_text(colour = 'white'),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white")
  )

