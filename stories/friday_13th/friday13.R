library(tidyverse)
library(lubridate)

# A 400 year cycle is what we need
many_years = data.frame(Date = seq(ymd('1600-01-01'), ymd('1999-12-31'), by=1)) %>%
  mutate(Day = day(Date),
         WeekDay = wday(Date, label=TRUE))

# Check weekdays are represented the same amount
many_years %>% count(WeekDay)

# Now see how often the 13th is a weekday
many_years %>%
  filter(Day == 13) %>%
  count(WeekDay)
