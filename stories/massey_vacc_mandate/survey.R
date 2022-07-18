library(tidyverse)
library(readxl)

vacc <- read_excel(here::here("stories/massey_vacc_mandate/for stats anonymous+vaccination+status+information_entries-6.xlsx"))

vacc %>% select(Person = `I am a:`, Status = `Vaccination status:`) %>%
  count(Person, Status) %>%
  group_by(Person) %>%
  mutate(Proportion = n/sum(n))

  ggplot(mapping = aes(x=Person, y=n, fill=Status)) + 
  geom_col(position='fill')
