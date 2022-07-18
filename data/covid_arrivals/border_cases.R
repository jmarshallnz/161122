library(tidyverse)
library(readxl)
library(lubridate)
library(mgcv)

cases <- read_excel("~/Downloads/covid-cases22jun2020.xlsx", skip=3) %>%
  rename(Date = `Date notified of potential case`)

cases %>% count(`Overseas travel`)
overseas <- cases %>% filter(`Overseas travel` == "Yes")

ggplot(cases) +
  aes(x=Date, fill=`Overseas travel`) +
  geom_bar()

case_plot <- overseas %>% count(Date=`Arrival date`) %>%
  filter(Date >= "2020-03-01") %>% arrange(Date) %>% 
  complete(Date = seq(min(Date), max(Date), by="1 day"), fill=list(n=0)) %>%
  mutate(Total = cumsum(n))

ggplot(case_plot) +
  aes(x=Date, y=n) +
  geom_line()

dat <- read_csv("~/Downloads/covid_19_data_portal.csv")
arrivals <- dat %>% filter(class == "Economic", category=="Travel",
               indicator_name == "Daily border crossings - arrivals") %>%
  mutate(Date = dmy(parameter)) %>%
  select(series_name, Date, Count = value) %>%
  filter(Date >= "2020-02-01")

arrival_plot <- arrivals %>% filter(Date >= "2020-03-01") %>%
  group_by(series_name) %>% arrange(Date) %>% mutate(Total = cumsum(Count))

ggplot(arrival_plot) +
  aes(x=Date, y=Count, col=series_name) +
  geom_line()

case_plot %>% left_join(arrival_plot, by="Date") %>%
  group_by(Date) %>% summarise(cases=first(n), arrivals=sum(Count)) -> out

dat_gam <- out %>% mutate(Date = as.Date(Date), DateNum=as.numeric(Date-min(Date))) %>%
  filter(arrivals != 0)

gam(cases ~ s(DateNum) + offset(log(arrivals)), family="quasipoisson", data=dat_gam) -> mod

summary(mod)

foo = broom::augment(mod, newdata=dat_gam %>% mutate(arrivals = 100))
foo

ggplot(foo) +
  aes(x=Date, y=exp(.fitted) ) +
  geom_col(data=dat_gam, aes(x=Date, y=cases/arrivals*100), fill='red') +
  geom_ribbon(aes(ymin=exp(.fitted-2*.se.fit), ymax=exp(.fitted+2*.se.fit)), alpha=0.3) +
  geom_line() +
  ylab("Detected COVID-19 positives (%)") +
  xlab("") +
  theme_minimal(base_size=15) +
  ggtitle("The Mystery of the missing May: COVID-19 cases at the border",
          subtitle = "Dots are cases per arrival, line and band is smoothed trend with uncertainty (GAM)")
