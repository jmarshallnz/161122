library(tidyverse)

boty <- read_csv("stories/boty2019/BOTY-votes-2019.csv", col_types = cols(country = "c"))

# Votes by date
boty |>
  count(date)

boty |>
  ggplot() +
  geom_bar(aes(x=date))

# Votes by hour

boty |>
  ggplot() +
  geom_bar(aes(x=hour))

# First preferences (i.e. first vote)
boty |> count(vote_1) |>
  slice_max(n, n=10)

boty |> count(vote_1) |>
  slice_max(n, n=10) |>
  ggplot() + 
  geom_col(aes(y=vote_1, x=n))

# Fifth preferences:
boty |> count(vote_5) |>
  slice_max(n, n=10) |>
  ggplot() + 
  geom_col(aes(y=vote_5, x=n))

# Total votes: Make long
long <- boty |>
  tibble::rowid_to_column("Voter") |>
  pivot_longer(vote_1:vote_5, names_to = "Vote", values_to = "Bird",
               names_prefix = "vote_") |>
  filter(!is.na(Bird))

# Hoiho is in front overall, so almost certainly wins, as it has more first preference
# votes and more overall votes.
long |> count(Bird) |>
  slice_max(n, n=10)

library(ggrepel)

long |>
  group_by(date) |>
  count(Bird) |>
  group_by(Bird) |>
  arrange(date) |>
  mutate(Total = cumsum(n)) |>
  group_by(date) |>
  slice_max(Total, n=10) |>
  ungroup() |>
  mutate(label = if_else(date == max(date), Bird, NA_character_)) |>
  ggplot() +
  geom_line(aes(x=date, y=Total, col=Bird)) +
  geom_label_repel(aes(x=date, y=Total, col=Bird, label=label))+
  guides(col='none')






# How did the vote go through time?
long %>%
  group_by(date) %>%
  count(Bird) %>%
  group_by(Bird) %>%
  arrange(date) %>%
  mutate(votes = cumsum(n)) %>%
  group_by(date) %>%
  slice_max(votes, n=10) %>%
  ungroup() %>%
  mutate(label = if_else(date == max(date), Bird, NA_character_)) %>%
  ggplot() +
  geom_line(aes(x=date, y=votes, col=Bird)) +
  geom_label_repel(aes(x=date, y=votes, label=label, col=Bird)) +
  guides(col='none')

# Election tampering?
# https://www.newshub.co.nz/home/new-zealand/2019/11/nz-bird-of-the-year-how-russia-influenced-the-vote.html
long %>% count(country) %>% arrange(desc(n))

long %>%
  filter(country == "Russian Federation") %>%
  count(Bird) %>%
  top_n(10, n)

long %>%
  filter(country != "Russian Federation" | is.na(country)) %>%
  count(Bird) %>%
  top_n(10, n)


long %>%
#  filter(Vote == 1) %>%
  group_by(date) %>%
  count(Bird) %>%

  group_by(Bird) %>%
  arrange(date) %>%
  mutate(cumm = cumsum(n)) %>%
  filter(max(cumm) > 5000) %>%
  mutate(Label = if_else(date == max(date), Bird, NA_character_)) %>%
  ggplot() +
  geom_line(aes(x=date, y=cumm, col=Bird)) +
  geom_label_repel(aes(x=date, y=cumm, label=Label, col=Bird)) +
  guides(col='none')

long %>% filter(country == "Russian Federation") %>%
  count(Bird) %>%
  arrange(desc(n))

testfun <- function(x) {
  out <- x %>%
    pivot_wider(names_from = Bird, values_from = Vote, values_fill = NA) %>%
    select(-Voter) %>%
    as.data.frame() %>%
    stv(seats=10)
  out$elected
}

date_hours <- long %>% select(Date=date, Hour=hour) %>% unique()

grab_votes <- function(df) {
  Date <- df$Date
  Hour <- df$Hour
  cat("Up to ", as.character(Date), " hour ", Hour, "\n")
  dfr <- long %>% filter(date < Date | date == Date | hour <= Hour) %>%
    select(-date, -hour, -country) %>%
    pivot_wider(names_from = Bird, values_from = Vote, values_fill = NA) %>%
    select(-Voter) %>%
    as.data.frame()

  out <- dfr %>% stv(seats=1)
  data.frame(date = Date, hour = Hour, elected = out$elected)
}

date_list <- date_hours %>% split(1:nrow(date_hours))
library(parallel)
foo <- mclapply(date_list, grab_votes, mc.cores=8)

write_csv(bind_rows(foo), "stories/boty2019/stv_winner.csv")

long %>%
  select(-country, -hour) %>%
  group_by(date) %>%
  nest() %>%
  mutate(out = map(data, testfun))


# How would STV run-off work on each day. Basically in STV for 1 position, we
# keep dropping out and distributing preferences right?

  filter(Vote == 1) %>%
  group_by(date, country) %>%
  count(Bird)
  
