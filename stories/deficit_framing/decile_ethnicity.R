library(tidyverse)
library(ggbeeswarm)

roll <- read_csv("https://www.massey.ac.nz/~jcmarsha/data/schools/roll.csv")
schools <- read_csv("https://www.massey.ac.nz/~jcmarsha/data/schools/schools.csv")

clean <- roll |> mutate(Level = as.numeric(substring(Level, 6)))

all_data <- clean |> left_join(schools)

all_data |>
  filter(Sector == "Secondary") |>
  filter(EthnicGroup != "International fee paying") |>
  filter(Decile %in% c(1, 10)) |>
  group_by(School, EthnicGroup, Decile) |>
  summarise(Students = sum(Students)) |>
  group_by(School) |>
  mutate(Proportion = Students/sum(Students)) |>
  mutate(Decile = as_factor(Decile)) |>
  ggplot() +
  geom_beeswarm(mapping = aes(y=EthnicGroup, x=Proportion, fill=Decile), groupOnX=FALSE,
                shape = 'circle filled', alpha=0.7,
                dodge.width = 0.6) +
  scale_x_continuous(labels=scales::percent_format()) +
  guides(col='none') +
  theme_minimal() +
  theme(legend.position='bottom') +
  labs(y=NULL, x=NULL, title = "Ethnicity of students in Decile 1 and 10 schools")

ggsave("stories/deficit_framing/school_ethnicity.png", width=8, height=5, dpi=150)
