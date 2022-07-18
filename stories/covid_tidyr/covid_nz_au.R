library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)

# read in data to get NZ tests per day etc:

foo <- html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases")
tables <- foo %>% html_nodes("table") %>% html_table()

# find the one we want - we want Tests per day as a column

test_table <- tables[map(tables, ~ "Tests per day" %in% names(.)) %>% unlist()][[1]]

tests_per_day <- test_table %>% mutate(Date = paste0(Date, "-2020"),
                      Date = dmy(Date)) %>%
  select(date=Date, tests=`Tests per day`)

cases_sheet <- read_excel(here::here("stories/covid_tidyr/covid-cases-10august2020.xlsx"), "Confirmed", skip=3)
cases_per_day <- cases_sheet %>% mutate(date = as.Date(`Date notified of potential case`)) %>% 
  count(date, name='cases')

nz_data <- cases_per_day %>% full_join(tests_per_day) %>%
  replace_na(list(cases = 0))

# Now the australian data
au_table <- read_csv("https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_national.csv")
au_data <- au_table %>% select(date, cases=confirmed, tests)

final_data <- nz_data %>% mutate(country = 'New Zealand') %>%
  bind_rows(au_data %>% mutate(country = "Australia")) %>%
  filter(!is.na(tests), tests > 0) %>% mutate(month = month(date),
                      month_label = month(date, label=TRUE, abbr=FALSE),
                      group_month = (month + 1) %/% 2) %>%
  group_by(group_month, country) %>% summarise(cases = sum(cases), tests = sum(tests), month_label = paste(first(month_label), last(month_label), sep="-")) %>%
  ungroup() %>% select(country, month = month_label, cases, tests)

final_data %>% write_csv(here::here("data/covid19/covid_au_nz.csv"))
final_data %>% pivot_longer(cases:tests, names_to='type', values_to='count') %>%
  write_csv(here::here("data/covid19/covid_au_nz2.csv"))

final_data %>% unite(rate, cases:tests, sep="/") %>%
  write_csv(here::here("data/covid19/covid_au_nz3.csv"))

final_data %>% select(-tests) %>% pivot_wider(names_from=country, values_from=cases) %>%
  write_csv(here::here("data/covid19/covid_au_nz4a.csv"))

final_data %>% select(-cases) %>% pivot_wider(names_from=country, values_from=tests) %>%
  write_csv(here::here("data/covid19/covid_au_nz4b.csv"))

final_data %>% separate(month, into=c("start", "end")) %>%
  write_csv(here::here("data/covid19/covid_au_nz5.csv"))

# Generate the images we need (yay ggplot!)
library(ggpubr)
library(cowplot)

# Fucking around with ggpubr carry on. Should do a PR for this
add_caption <- function (tab, text, face = NULL, size = NULL, color = NULL, 
                         family = NULL, padding = unit(1.5, "line"), just = "center", 
                         hjust = NULL, vjust = NULL) 
{
  tabgrob <- ggpubr:::get_tablegrob(tab)
  text <- grid::textGrob(text, x = 0.5, just = just, hjust = hjust, 
                         vjust = vjust, gp = grid::gpar(fontsize = size, fontface = face, 
                                                        fontfamily = family, col = color))
  tabgrob <- gtable::gtable_add_rows(tabgrob, heights = grid::grobHeight(text) + 
                                       padding, pos = -1)
  tabgrob <- gtable::gtable_add_grob(tabgrob, list(text), t = nrow(tabgrob), 
                                     b = nrow(tabgrob), l = 1, r = ncol(tabgrob))
  ggpubr:::tab_return_same_class_as_input(tabgrob, input = tab)
}


# Should do a PR so that color drops down to body style?
png("test.png", width=470, height=350)
foo <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                  tbody.style = tbody_style(color='grey50', size=20))) %>%
  add_caption("variables", size=24)

for (i in 1:6) {
  foo <<- foo + draw_line(
    x = c(0.1, 0.9),
    y = c((i+0.5)/8, (i+0.5)/8),
    color = "black", size = 2,
    arrow = arrow(angle = 30)
  )
}
print(foo)
dev.off()

# Should do a PR so that color drops down to body style?
png("tidy1.png", width=470, height=350)
foo <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                         tbody.style = tbody_style(color='grey50', size=20))) %>%
  add_caption("observations", size=24)

for (i in 1:6) {
  foo <<- foo + draw_line(
    x = c(0.1, 0.9),
    y = c((i+0.5)/8, (i+0.5)/8),
    color = "black", size = 2,
    arrow = arrow(angle = 30)
  )
}
print(foo)
dev.off()

png("tidy2.png", width=470, height=350)
foo <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                         tbody.style = tbody_style(color='grey50', size=20))) %>%
  add_caption("variables", size=24)

vals <- c(0.16, 0.45, 0.68, 0.89)
for (x in vals) {
  foo <<- foo + draw_line(
    x = rep(x,2),
    y = c(6.5/8, 1.5/8),
    color = "black", size = 2,
    arrow = arrow(angle = 30)
  )
}
print(foo)
dev.off()

png("tidy3.png", width=470, height=350)
foo <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                         tbody.style = tbody_style(color='grey50', size=20))) %>%
  add_caption("values", size=24)

x_vals <- c(0.16, 0.45, 0.68, 0.89)
y_vals <- (1:6 + 0.5)/8
for (x in x_vals) {
  g <- grid::circleGrob(x=0,y=0,r=0.4,gp = gpar(col='black', fill='transparent', lwd=5))
  for (y in y_vals) {
  foo <<- foo + draw_grob(g,
    x = x,
    y = y,
    width=0.1
  )
}
}
print(foo)
dev.off()


covid2 <- final_data %>% pivot_longer(cases:tests, names_to='type', values_to='count')
png("covid2_tidy1.png", width=470, height=570)
foo1 <- ggtexttable(covid2, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                         tbody.style = tbody_style(color='grey50', size=20))) %>%
  table_cell_bg(row=seq(2,12,by=2),column=4,fill='yellow') %>%
  table_cell_bg(row=seq(3,13,by=2),column=4,fill='orange') %>%
  table_cell_font(row=2:13,column=4,color='black', size=20)
foo1
dev.off()

png("covid2_tidy2.png", width=470, height=310)
foo2 <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                      tbody.style = tbody_style(color='grey50', size=20))) %>%
  table_cell_bg(row=2:7,column=3,fill='yellow') %>%
  table_cell_bg(row=2:7,column=4,fill='orange') %>%
  table_cell_font(row=2:7,column=3:4,color='black', size=20)
foo2
dev.off()

# Should do a PR so that color drops down to body style?
covid4a <- final_data %>% select(-tests) %>%
  pivot_wider(names_from=country, values_from=cases)
png("covid4_tidy1.png", width=404, height=180)
foo1 <- ggtexttable(covid4a, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                      tbody.style = tbody_style(color='grey50', size=20)))

foo1 %>%
  table_cell_bg(row=2:4,column=2,fill='yellow') %>%
  table_cell_bg(row=2:4,column=3,fill='orange') %>%
  table_cell_font(row=2:4,column=2:3,color='black', size=20)
dev.off()

png("covid4_tidy2.png", width=370, height=310)
foo1 <- covid4a %>%
  pivot_longer(c(Australia, `New Zealand`), names_to="country", values_to = "cases") %>%
  ggtexttable(rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                       tbody.style = tbody_style(color='grey50', size=20)))

foo1 %>%
  table_cell_bg(row=seq(2,6,by=2),column=3,fill='yellow') %>%
  table_cell_bg(row=seq(3,7,by=2),column=3,fill='orange') %>%
  table_cell_font(row=2:7,column=3,color='black', size=20)
dev.off()

png("covid2_tidy2.png", width=470, height=310)
foo2 <- ggtexttable(final_data, rows=NULL, theme = ttheme(base_size = 20, padding = unit(c(10,10), "mm"),
                                                          tbody.style = tbody_style(color='grey50', size=20))) %>%
  table_cell_bg(row=2:7,column=3,fill='yellow') %>%
  table_cell_bg(row=2:7,column=4,fill='orange') %>%
  table_cell_font(row=2:7,column=3:4,color='black', size=20)
foo2
dev.off()



for (i in 1:6) {
  foo <<- foo + draw_line(
    x = c(0.1, 0.9),
    y = c((i+0.5)/8, (i+0.5)/8),
    color = "black", size = 2,
    arrow = arrow(angle = 30)
  )
}
print(foo)
dev.off()





# Fucking around with the tables
tab <- ggpubr:::get_tablegrob(foo)


  tab_add_footnote(text="variables", just="center", size=20) +

+
  geom_segment(x=0.5, y=0.5, xend=0.7, yend=0.7, col='black') +
  scale_x_continuous(limits = c(0,5))
g
