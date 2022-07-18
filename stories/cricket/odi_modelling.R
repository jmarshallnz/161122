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

if (0) {
  odis <- getTeamData(matchType = "ODI")
  odis[names(odis) == ""] <- NULL

  write_csv(odis, here::here("stories/cricket/ODIs.csv"))
}
odis <- read_csv("ODIs.csv") %>% extract(Score, into=c("Runs", "Wickets"), regex="([0-9]+)([/0-9])*", convert=TRUE, remove=FALSE) %>%
  mutate(Opposition = str_sub(Opposition, 3)) %>% replace_na(list(Wickets = 10)) %>%
  mutate(`Start Date` = dmy(`Start Date`))

# OK, now find all matches where the first team won...

# Note: Averaging RPO for both teams isn't correct, but probably close enough for what we want.
#       should really weight by innings length
won_first_innings <- odis %>% filter(Inns == 1, Result != 'lost')

matches <- won_first_innings %>% left_join(odis %>% select(Opposition = Team, LostRuns = Runs, LostWickets = Wickets, Ground, `Start Date`)) %>%
  mutate(Margin = Runs - LostRuns, Total = Runs + LostRuns)

# plot them!
ggplot(matches, aes(x=Margin)) + geom_histogram()

# interesting - note the margin is negative for a few
matches %>% filter(Margin < 0)

# this is duckworth-lewis-stern in action. Let's truncate at 0 - as presumably the teams are pretty evenly balanced?

# right, we want to model the difference in runs, accounting for team etc. How many teams do we have to deal with?
wins <- matches %>% group_by(Team) %>% summarise(Wins = n())
losses <- matches %>% group_by(Team = Opposition) %>% summarise(Losses = n())
all_teams <- wins %>% left_join(losses) %>% mutate(Total = Wins + Losses) %>% arrange(Total) %>% as.data.frame()
# seems we have 25 teams, but only 10 of them have more than 100 matches. Let's just look at them
teams_to_include <- all_teams %>% filter(Total > 100) %>% pull(Team)

final_matches <- matches %>% filter(Team %in% teams_to_include, Opposition %in% teams_to_include) %>%
  mutate(Margin = pmax(Margin, 0))

ggplot(final_matches, aes(x=Margin)) + geom_histogram()

# are we going to be able to account for ground? Probably not in many cases. Possibly we could do something like home
# vs away if we can match ground to country?
table(final_matches$Ground)

# ok, let's assume we're just going to model the margin. Hmm, doesn't seem much evidence of it changing much
# over time... Wonder if it's influenced by the very large margins?
ggplot(final_matches, aes(x=`Start Date`, y=Margin/Total)) + geom_point() +
  geom_smooth()

# doesn't seem to be!
ggplot(final_matches, aes(x=`Start Date`, y=log10(Margin+1))) + geom_point() +
  geom_smooth()

modelling_matches <- final_matches %>% mutate(Year = year(`Start Date`)) %>% select(Margin, Total, Team, Opposition, Year) %>%
  filter(Year > 1979)
m1 <- model.matrix(Margin ~ -1 + Team*Year, data=modelling_matches)
m2 <- model.matrix(Margin ~ -1 + Team*Year, data=modelling_matches %>% select(Team=Opposition, Year, Margin))
all.equal(colnames(m1), colnames(m2))
final_mat <- m1 - m2
# work out the final means?
final_mat %*% mod$coefficients
# hmm, this isn't still what we want. We could just do it manually I guess?
coefs <- modelling_matches %>% filter(Team != "Australia") %>% expand(Team, Year) %>% tibble::rowid_to_column()

# find which coefficients go where
matching <- modelling_matches %>% left_join(coefs) %>%
  left_join(coefs %>% select(Opposition = Team, Year, negid = rowid))

# construct a model matrix for this
model_mat <- matrix(0, nrow(modelling_matches), nrow(coefs))
model_mat[cbind(1:nrow(model_mat), matching$rowid)] <- 1
model_mat[cbind(1:nrow(model_mat), matching$negid)] <- -1

# try fitting the linear model
mod <- lm.fit(model_mat, modelling_matches$Margin)
fit <- coefs %>% bind_cols(Fit = mod$coefficients)

ggplot(fit, aes(x=Year, y=Fit, col=Team)) + geom_line() + 
  facet_wrap(~Team) + ggtitle("Performance relative to Australia")

fit_by_year <- fit %>% group_by(Year) %>% summarise(varFit = var(c(Fit, 0), na.rm=TRUE), meanFit = mean(c(Fit,0), na.rm=TRUE))
ggplot(fit_by_year, aes(x=Year, y=varFit)) + geom_line()
ggplot(fit_by_year, aes(x=Year, y=meanFit)) + geom_line()

fit <- data.frame(mod$coefficients) %>% tibble::rownames_to_column("Variable") %>%
  extract(Variable, into=c("Team"), regex="Team([^:]+)", remove=FALSE) %>%
  extract(Variable, into=c("Year"), regex="Year([0-9]+)", convert=TRUE) %>%
  replace_na()

# Try some sort of weird gam
gam_dat <- modelling_matches %>% mutate(Team = factor(Team), Opposition = factor(Opposition))
mod <- gam(Margin ~ s(Year, Team, k=5, bs="fs", m=2) +
             s(Year, Opposition, k=5, bs="fs", m=2), data = gam_dat, method="REML", family=nb)
all_possible <- modelling_matches %>% expand(Team, Opposition, Year) %>%
  left_join(modelling_matches)
aug <- broom::augment(mod, newdata=all_possible)
ggplot(aug) + geom_point(data=modelling_matches, aes(x=Year, y=Margin)) +
  geom_ribbon(aes(x=Year, ymin = exp(.fitted-2*.se.fit), ymax = exp(.fitted + 2*.se.fit)), alpha=0.3) +
  geom_line(aes(x=Year, y = exp(.fitted))) +
  facet_grid(Team ~ Opposition) + ylim(0,300)

# could also do an interaction just for fun?
gam_dat_pairs <- modelling_matches %>% mutate(First = pmin(Team, Opposition),
                                              Second = pmax(Team, Opposition),
                                              Margin = if_else(Team == First, Margin, -Margin)) %>%
  unite(Pairing, First, Second, sep=":") %>%
  mutate(Pairing = factor(Pairing)) %>%
  select(Margin, Total, Year, Pairing)

# we can then simplify this so that the ordering is incorporated into the margin...

#mod <- gam(Margin ~ s(Year, k=5, m=2) + s(Year, Pairing, k=5, bs="fs", m=2), data = gam_dat_pairs, method="REML")
mod <- gam(Margin/Total ~ s(Year, Pairing, k=5, bs="fs", m=2), data = gam_dat_pairs, method="REML")
#mod <- gam(Margin ~ s(Year, Pairing, bs="fs", m=2), data = gam_dat_pairs, method="REML")

all_possible <- gam_dat_pairs %>% expand(Pairing, Year) %>%
  left_join(gam_dat_pairs)
aug <- broom::augment(mod, newdata=all_possible)
#ranges <- aug %>% filter(!is.na(Margin)) %>% group_by(Pairing) %>% summarise(MinYear = min(Year), MaxYear = max(Year))
one_way <- aug %>% #left_join(ranges) %>% filter(Year >= MinYear, Year <= MaxYear) %>%
  separate(Pairing, into=c("First", "Second"), sep=":", remove=FALSE)

# now extend out so it's pretty to plot
other_way <- one_way %>% mutate(First1 = First, First = Second, Second = First1) %>%
  select(-First1) %>%
  mutate_at(vars(Margin, .fitted, .se.fit), ~ -.)
aug <- bind_rows(one_way, other_way)
ggplot(aug) + geom_point(data=aug, aes(x=Year, y=Margin/Total), alpha=0.3) +
  geom_ribbon(aes(x=Year, ymin = .fitted-2*.se.fit, ymax = .fitted + 2*.se.fit), alpha=0.3, fill='steelblue') +
  geom_line(aes(x=Year, y = .fitted), col='steelblue', size=1) +
  facet_grid(First ~ Second) + theme_bw()

ggplot(aug) + geom_point(data=aug, aes(x=Year, y=Margin/Total), alpha=0.3) +
#  geom_ribbon(aes(x=Year, ymin = .fitted-2*.se.fit, ymax = .fitted + 2*.se.fit, fill=Pairing), alpha=0.3) +
  geom_line(aes(x=Year, y = .fitted, group=Second), col='black', size=1.5) +
  geom_line(aes(x=Year, y = .fitted, col=Second), size=1) +
  facet_wrap(~First, nrow=2) + theme_bw() + scale_color_brewer(name = "Opposition", type="qual", palette = "Paired") +
  ggtitle("Margin for a win by the team that bats first") + scale_y_continuous(name="Winning/losing margin", labels=scales::percent)
#  facet_grid(First ~ Second) + theme_bw()

# Now what about those that win batting second?

ggplot(gam_dat_pairs, aes(x=Year, y=Margin)) + geom_point() + geom_smooth(method='gam') + facet_wrap(~Pairing)

# OK, if we want to estimate the team effects as smooth effects per time, then the Margin could
# be modelled using the difference between them (winner effect - loser effect)
# the design matrix would be a little tricky maybe? Hmm...
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

