library(tidyverse)
library(lubridate)
library(patchwork)

log <- read_csv("stories/stream_activities/logs_161122_2022_S2FS_20220729-1450.csv")

remove_users <- c("-", "Ray Khanthaporn", "Admin User", "Julio Pereira", "Adam Smith", "Jonathan Marshall")

first_access <- log |>
  mutate(Time = dmy_hm(Time),
         Day  = date(Time)) |>
  rename(User = `User full name`,
         Item = `Event context`) |>
  filter(!User %in% remove_users,
         Day >= '2022-07-15') |>
  filter(str_detect(Item, "A0[1-8]"),
         str_detect(Item, "URL")) |>
  extract(Item, into=c("Type", "Item", "Extra"),
          regex="URL: (.*) A(0[1-8])(.*)", convert=TRUE) |>
  filter(Item <= 4) |>
  mutate(Item = paste0("A0", Item),
         Type = if_else(str_detect(Extra, "recording"), "Video", Type)) |>
  group_by(User, Type, Item) |>
  summarise(Day = first(Day))

g1 = first_access |> 
  filter(Type != "Video") |>
  group_by(Day, Type, Item) |>
  summarise(n=n()) |>
  ggplot() +
  geom_line(mapping=aes(x=Day, y=n, col=Item)) +
  facet_wrap(vars(Type), ncol=1) +
  theme_minimal() +
  scale_colour_brewer(palette='Dark2') +
  labs(x=NULL, y="Students") +
  guides(col='none')

g2 = first_access |>
  filter(Type != "Video") |>
  ggplot() +
  geom_bar(aes(x=Type, fill=Item), position='dodge', alpha=0.8) +
  scale_fill_brewer(palette='Dark2') +
  theme_minimal() +
  labs(x=NULL, y="Students")

g2 + g1 + plot_layout(guides='collect') +
  plot_annotation(title="Number of students accessing workshops and lectures", subtitle = "Only first access per student is counted") &
  theme(legend.position='bottom')

ggsave(filename="stories/stream_activities/activity.png", width=8, height=5, dpi=150)
