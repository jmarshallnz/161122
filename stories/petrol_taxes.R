library(tidyverse)

dat <- read_csv("~/Downloads/weekly-table(2).csv")

petrol <- dat %>% select(Week_ending_Friday, starts_with("Regular_Petrol"))

cleaner <- petrol %>% rename_with( ~ str_remove(., "Regular_Petrol_"), starts_with("Reg")) %>%
  rename_with( ~ str_remove(., "_NZc.p.l"), -Week_ending_Friday)

clean <- cleaner %>% select(week = Week_ending_Friday, importer_cost:taxes, discounted_retail_price, importer_margin) %>%
  mutate(taxes = taxes - GST) %>%
  pivot_longer(-c(week, discounted_retail_price), names_to="category", values_to="value")

# Check: These should be about the same
check <- clean %>% group_by(week, discounted_retail_price) %>% summarise(price=sum(value))

ggplot(check) +
  geom_point(aes(x=discounted_retail_price, price, col=week))
# Seems not too far off for later values at least?

ggplot(clean %>% mutate(category = fct_relevel(category, "importer_cost", "importer_margin", "GST", "taxes", "ETS"))) +
  geom_area(aes(x=week, y=value/100, fill=category)) +
  scale_fill_viridis_d() +
  theme_minimal() +
  labs(x = NULL, y="Discounted price ($/L)", title = "Petrol price fluctuations are mostly due to importer cost",
  subtitle = "Longer term increases are mostly due to taxes and inflation", fill=NULL) + 
  scale_y_continuous(expand = c(0,0), labels = scales::label_dollar()) +
  scale_x_date(expand = c(0,0)) +
  theme(legend.position = "top")
