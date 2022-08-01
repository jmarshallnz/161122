library(tidyverse)

manawatu <- read_csv('https://www.massey.ac.nz/~jcmarsha/data/island/manawatu.csv', guess_max = 10000)
mb2013   <- read_csv('https://www.massey.ac.nz/~jcmarsha/data/island/mb2013.csv')

# Our region is that covered by Mid Central Public Health Unit, which is the intersection
# of the Mid Central District Health Board and the Manawatu-Wanganui region:
pop <- mb2013 %>% filter(DHB_name == "Mid Central", RC2013_name == "Manawatu-Wanganui Region") %>%
  select(MB2013, MB2006, Pop2001, Pop2006, Pop2013, UR2006_num, UR2013_num) %>%
  gather(Year, Population, Pop2001:Pop2013) %>%
  extract(Year, into="Year", regex="([0-9]+)", convert=TRUE)

# OK, now fit a linear model per meshblock to the population
pops <- split(pop, pop$MB2013)

predict_popn <- function(dat) {
  m <- lm(Population ~ Year, data=dat)
  data.frame(Year=2005:2014, Population=predict(m, data.frame(Year=2005:2014)))
}

pop_interp <- dplyr::bind_rows(lapply(pops, predict_popn), .id='MB2013') %>%
  mutate(MB2013 = as.numeric(MB2013)) %>%
  left_join(mb2013 %>% select(MB2006, MB2013, UR2006_num) %>% unique)

# now rurality by year popn...

# 2. Once done, compute number of cases by rurality by year
manawatu

case_rates <- pop_interp %>% group_by(UR2006_num, Year) %>% summarize(Population=sum(Population)) %>%
  left_join(manawatu %>% group_by(UR2006_num, Year) %>% summarize(Cases = n()))

rates <- case_rates %>% ungroup %>% mutate(Group = ifelse(UR2006_num > 0, "Urban", "Rural")) %>%
  group_by(Group, Year) %>% summarize(Population=sum(Population, na.rm=TRUE), Cases=sum(Cases, na.rm=TRUE)) %>%
  mutate(CaseRate = Cases/Population*100000) %>%
  mutate(CaseRate = ifelse(Year == 2005, CaseRate/0.75, CaseRate)) %>% # In 2005 we have cases from 75% of the year
  ungroup() %>%
  mutate(Year = factor(Year))


#pdf('fig_case_rates.pdf', width=6, height=3.5)
ggplot(rates, aes(x=Year, y=CaseRate, fill=Group)) +
  geom_col(position='dodge') +
  theme_bw() +
  theme(text = element_text(family="Times"),
        legend.position = c(0.07, 0.91),
        legend.margin=margin(0,0,0,0),
        legend.background = element_rect(fill = 'transparent')) +
  scale_fill_manual(NULL, values=c("grey30", "grey70")) +
  ylab("Cases per 100,000 population")
#dev.off()
