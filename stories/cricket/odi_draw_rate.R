library(cricketr)
library(tidyverse)
library(lubridate)
library(mgcv)

# TODO: Make this into a data story.

# Motivation: World cup cricket 'loss'

# What was asked - what is the chances of a draw?

# In this case, chances are quite low as need both a draw after 50 AND after 51 overs.

# RPO for super over likely to be higher - maybe double?

# What about just draws?

# Then David tracked whether the 1.8% we find for draws given runrate of 4.8 is what we've seen in history

# He found it seems to change through time. Also some thoughts on whether teams are balanced. If not, rate will be lower.

# JM realised runrate assumption isn't constant through time - steadily increasing, which means draw rate decreases (variance of larger numbers is larger)

# how do we speed up the computation/simulation?
# can use sum of poisson is a poisson. Then use diff of poisson to get to bessel function.
# alternatively, could use normal approximation to poisson
# incorporate this to show teams seem to be getting more similar, but lots of uncertainty.

# how can we improve the model further? Match location? Likely runrate, so chance of draw depends on that as well as time, also relative strength
# of teams.

# overall conclusions: even with imperfect data we can often find something of interest. e.g. sometimes we can use proxies to answer questions,
# just answer won't be anywhere near precise. Lots of statistical distributions/models are related. Simplifications don't tend to matter too much
# compared to noise in data, so don't be too worried about getting model wrong, if model wrongness isn't big compared to data uncertainty?

# Lots of jumping off points: Maths for distributions, cricket information, runrate model etc.

odis <- getTeamData(matchType = "ODI")
odis[names(odis) == ""] <- NULL

#write_csv(odis, "ODIs.csv")
#odis <- read_csv("ODIs.csv")

# Note: Averaging RPO for both teams isn't correct, but probably close enough for what we want.
#       should really weight by innings length
matches <- odis %>% group_by(Ground, Start.Date) %>% summarise(RPO = mean(RPO), Draw = any(str_detect(Result, "tied"))) %>%
  mutate(Start.Date = dmy(Start.Date)) %>%
  mutate(Draw = as.numeric(Draw)) %>%
  arrange(Start.Date) %>%
  ungroup() %>%
  mutate(Day = as.numeric(Start.Date - min(Start.Date))) %>%
  mutate(ExpectedRate = besselI(2*50*RPO, 0, expon.scaled = TRUE))

# model the draw rate through time
mod <- gam(Draw ~ s(Day), data=matches, family=binomial)

mat <- broom::augment(mod, matches) %>%
  mutate(y = plogis(.fitted),
         ymin = plogis(.fitted - 2*.se.fit),
         ymax = plogis(.fitted + 2*.se.fit)) %>%
  select(-starts_with('.')) %>%
  mutate(Model = "Observed")

# and the expected draw rate (given run rate of match)
mod2 <- gam(ExpectedRate ~ s(Day), data=matches)

mat2 <- broom::augment(mod2, matches) %>%
  mutate(y = .fitted,
         ymin = .fitted - 2*.se.fit,
         ymax = .fitted + 2*.se.fit) %>%
  select(-starts_with('.')) %>%
  mutate(Model = "Expected")

# combine together and plot
final <- bind_rows(mat, mat2)

ggplot(final, aes(x=Start.Date, y=y, ymin = ymin, ymax=ymax, fill=Model, col=Model)) + 
  geom_ribbon(col=NA, alpha=0.3) + geom_line(lwd=1) +
  coord_cartesian(ylim=c(0,0.03)) + scale_x_date("Match date", expand=c(0,0)) +
  scale_y_continuous("Draw rate", expand=c(0,0))

