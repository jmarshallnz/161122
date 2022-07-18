library(tidyverse)
library(readxl)
library(lubridate)

bridge_old <- read_excel(here::here("stories/pncc_cycling/14513_2020-04-30-12-05-52.xlsx"))

bridge <- read_excel(here::here("stories/pncc_cycling/He Ara Kotahi - 1 March to 31 July - 15 minute blocks.xlsx"),
                     skip = 2)

# Join these up - only care about cyclists for now
all <- bind_rows(bridge_old, bridge %>% set_names(nm = names(bridge_old))) %>% unique() %>%
  filter(!is.na(`Channel 1 IN`)) %>%
  arrange(Date) %>% select(-`E29 He Ara Kotahi Bridge`) %>%
  pivot_longer(-Date, names_to = "Channel", values_to = "Count")

clean <- all %>% extract(Channel, into=c("Channel", "Direction"), regex="^Channel ([0-9]) (.*)", convert=TRUE) %>%
  mutate(Type = ifelse(Channel <=2, "Pedestrians", "Cyclists"))

# NA out all the values before the first cyclist
cyclists <- clean %>% filter(Type == "Cyclists")

first_cyclist_count <- cyclists %>% filter(Count > 0) %>% arrange(Date) %>% slice(1) %>%
  pull(Date)

cyclists %>% filter(Date >= first_cyclist_count) %>%
  mutate(Date = floor_date(Date, unit = "hour")) %>%
  group_by(Date, Direction) %>%
  summarise(Count = sum(Count, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(Direction = if_else(Direction == "IN", "From City", "To City")) %>%
  pivot_wider(names_from = Direction, values_from = Count) %>%
  mutate(Hour = hour(Date), 
         Date = as.Date(floor_date(Date, unit = "day"))) %>%
  select(Date, Hour, everything()) %>%
  write_csv(here::here("stories/pncc_cycling/he_ara_kotahi.csv"))

cyclists <- read_csv(here::here("stories/pncc_cycling/he_ara_kotahi.csv"))


min_date <- min(as.Date(cyclists$Date))
max_date <- max(as.Date(cyclists$Date))
lockdown <- tibble::tribble(~Date, ~Level,
                            as.character(min_date), 0,
                            "2020-03-26", 4,
                            "2020-04-28", 3,
                            "2020-05-14", 2,
                            "2020-06-09", 1,
                            as.character(max_date), 1) %>%
  mutate(Date = as.Date(Date))

lockdown %>% mutate(Date = as_datetime(Date)) %>% complete(Date = seq(min_date, max_date, by=1)) %>%
  tidyr::fill(Level) %>% mutate(Timeperiod = case_when(Level < 2 ~ "Before times",
                                                       TRUE ~ "Lockdown")) %>%
  select(-Level) %>%
  write_csv(here::here("stories/pncc_cycling/lockdown_dates.csv"))

lockdown_dates <- read_csv(here::here("stories/pncc_cycling/lockdown_dates.csv"))

cyclists %>%
  pivot_longer(cols = ends_with("City"), names_to = "Direction", values_to = "Count") %>%
  left_join(lockdown_dates) %>%
  mutate(WeekDay = wday(Date, label=TRUE, abbr = FALSE)) %>%
  group_by(Timeperiod, Direction, WeekDay, Hour) %>% summarise(Count = mean(Count, na.rm=TRUE)) %>%
  ungroup() ->
  plot_days

write_csv(plot_days, here::here("stories/pncc_cycling/cycle_counts_plot.csv"))

plot_days <- read_csv(here::here("stories/pncc_cycling/cycle_counts_plot.csv"))

ggplot(plot_days) + aes(x=Hour, y=Count, col=Direction) +
  geom_line(size=1) + 
  facet_grid(rows = vars(Timeperiod), cols = vars(as_factor(WeekDay))) +
  theme_minimal() + labs(x="", y="",
                         title="Every day is a weekend. Cycling across He Ara Kotahi in lockdown",
                         subtitle = "Count data thanks to PNCC") +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position="bottom") +
  scale_colour_manual(values = c("#984ea3", "#1f78b4"))


png("he_ara_kotahi.png", width=1600, height=900)
ggplot(plot_days) + aes(x=Hour, y=Count, col=Direction) +
  geom_line(aes(group=Direction), col='black', size=2.4) +
  geom_line(size=2) + 
  facet_grid(rows = vars(Timeperiod), cols = vars(as_factor(WeekDay))) +
  theme_minimal(base_size=28) + labs(x="", y="",
                                     title="Every day is a weekend. Cycling across He Ara Kotahi in lockdown",
                                     subtitle = "Count data thanks to PNCC") +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) +
  theme(legend.position="bottom") +
  scale_colour_manual(values = c("#984ea3", "#1f78b4"))
dev.off()

g
ggplot(plot_days) + aes(x=Hour, y=Count, col=Direction) +
  geom_point(alpha=0.3, size=0.8) +
#  geom_line(data=average_days, aes(x=Hour, y=Count, col=Direction), size=1) + 
  facet_grid(Lockdown~WeekDay) +
  theme_minimal() + labs(x="", y="",
                         title="Cycling across He Ara Kotahi looks very different in lockdown",
                         subtitle = "Count data thanks to PNCC") +
  theme(axis.text.x = element_blank()) +
  guides(col='none')

g
ggsave("cycle_counts.png", g, width=8, height=4.8, dpi=100)

library(ggridges)

g <- ggplot(plot_days) +
  geom_ridgeline(aes(x=Hour, y=fct_rev(Week), fill=Direction, height=Count), scale=0.04, alpha=0.4) +
  facet_wrap(~ WeekDay) + scale_x_continuous(breaks=seq(0,24,by=4), labels=function(x) { paste0(x, ":00") }, expand=c(0,0)) +
  theme_minimal() + guides(fill='none') +
  labs(x="", y="", title="Cyclists across He Ara Kotahi", subtitle = "Count data thanks to PNCC")

cycle_counts %>% mutate(Week = floor_date(Date, unit="week"), WeekDay = wday(Date, label=TRUE, abbr = FALSE), Hour = hour(Date), Minute = minute(Date),
                        Time = Date - floor_date(Date, unit="day")) %>%
  as_tibble() %>%
  group_by(Direction, Week, WeekDay) %>% summarise(Count = sum(Count, na.rm=TRUE), Repeats=n()) %>%
  ungroup() %>%
  mutate(Week = as.character(Week)) ->
  plot_weeks

ggplot(plot_weeks) + aes(x=as.Date(Week), y=Count, col=Direction) + geom_line() +
  facet_wrap(~WeekDay)

g
ggsave("cycle_counts.png", g)

clean %>% filter(Type == "Pedestrians") %>% 
  filter(Date >= first_cyclist_count) %>% mutate(Week = floor_date(Date, unit="week"), WeekDay = wday(Date, label=TRUE, abbr = FALSE), Hour = hour(Date), Minute = minute(Date),
                        Time = Date - floor_date(Date, unit="day")) %>%
  group_by(Direction, Week, WeekDay, Hour) %>% summarise(Count = sum(Count, na.rm=TRUE), Repeats=n()) ->
  plot_peds

g2 <- ggplot(plot_peds) +
  geom_ridgeline(aes(x=Hour, y=factor(Week), fill=Direction, height=Count), scale=0.04, alpha=0.4) +
  facet_wrap(~ WeekDay) + scale_x_continuous(breaks=seq(0,24,by=4), labels=function(x) { paste0(x, ":00") }, expand=c(0,0)) +
  theme_minimal() + guides(fill='none') +
  labs(x="", y="", title="Pedestrians across He Ara Kotahi", subtitle = "Count data thanks to PNCC")

g2
ggsave("ped_counts.png", g2)


foo <- clean %>% filter(Type == "Pedestrians") %>% 
  filter(Date >= first_cyclist_count) %>% mutate(Week = floor_date(Date, unit="week"), WeekDay = wday(Date, label=TRUE, abbr = FALSE), Hour = hour(Date), Minute = minute(Date),
                                                 Time = Date - floor_date(Date, unit="day"))

ggplot(foo) + aes(x=Date, y=Count, col=Direction) + geom_line(alpha=0.2) + geom_smooth(method='gam') +
  coord_cartesian(ylim = c(0,50))
