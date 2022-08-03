library(tidyverse)
library(lubridate)
library(patchwork)

quiz = read_csv("stories/stream_activities/161122_2022_S2FS-Quiz A1-grades.csv") |>
  select(StudentID = `ID number`, Time = `Time taken`, Grade = `Grade/5.00`, starts_with("Q"))

clean_quiz <- quiz |>
  filter(Time != "30 mins" | Grade != 0) |>
  mutate(DecTime = ms(Time) |> time_length() |> as.integer()) |>
  extract(Time, into="DecTime2", regex="([0-9]+) secs", convert=TRUE) |>
  mutate(DecTime = if_else(is.na(DecTime), DecTime2, DecTime)) |>
  filter(!is.na(DecTime)) |>
  rowid_to_column("Attempt") |>
  pivot_longer(starts_with('Q'), names_to="Question", values_to="Value") |>
  mutate(Value = as.numeric(Value)) |>
  group_by(StudentID, Attempt, Time=DecTime) |>
  summarise(Grade = sum(Value == 1, na.rm=TRUE)) |>
  ungroup()

g1 = clean_quiz |>
  count(StudentID) |>
  ggplot() +
  geom_bar(aes(x=n), width=0.9, fill='steelblue') +
  labs(x="Attempts")

g2 = clean_quiz |>
  ggplot() +
  geom_histogram(aes(x=Time/60), binwidth=1, boundary=0, fill='plum') +
  labs(x="Time (minutes)")

g3 = clean_quiz |>
  ggplot() +
  geom_bar(aes(x=Grade), fill='gray50') +
  scale_x_continuous(limits=c(-0.5,5.5), breaks=0:5) +
  labs(x="Number correct (all attempts)")

g4 = clean_quiz |>
  group_by(StudentID) |>
  summarise(Grade = max(Grade)) |>
  ggplot() +
  geom_bar(aes(x=Grade), fill='orange') +
  scale_x_continuous(limits=c(-0.5,5.5), breaks=0:5) +
  labs(x="Number correct (best attempt)")

g1 + g2 + g3 + g4 + plot_layout(nrow=2, byrow=FALSE) +
  plot_annotation(title="Quiz A1 performance", subtitle="With practice, students take less than 5 minutes to get 5/5") &
  theme_minimal() +
  theme(axis.title.y = element_blank(),
        plot.background = element_rect(fill='white', colour=NA))
ggsave("stories/stream_activities/quizA1.png", dpi=150, width=8, height=5)
