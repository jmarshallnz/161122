library(tidyverse)
library(lubridate)

he_ara_kotahi <- read_csv("https://www.massey.ac.nz/~jcmarsha/161122/data/he_ara_kotahi_one_year.csv") %>%
  mutate(Day = wday(Date, label=TRUE),
         Month = month(Date, label=TRUE))

peds <- he_ara_kotahi %>% filter(Mode == "Pedestrian")

g1 <- ggplot(peds) +
  geom_boxplot(aes(x=Day, y=Count, col=Direction))

g2 <- ggplot(peds) +
  geom_boxplot(aes(x=Month, y=Count, col=Direction))

g3 <- ggplot(peds) +
  geom_line(aes(x=Date, y=Count, col=Direction))

library(patchwork)

g3 / (g1 + g2) + plot_layout(guides = 'collect')

mod_peds <- peds %>%
  mutate(Day = factor(Day, ordered=FALSE),
         Month = factor(Month, ordered=FALSE))

full = lm(Count ~ Direction * Day + Month, data=mod_peds)
summary(full)
anova(full)
par(mfrow=c(2,2))
plot(full)

log_full = lm(sqrt(Count) ~ Direction * Day + Month, data=mod_peds)
summary(log_full)
anova(log_full)
par(mfrow=c(2,2))
plot(log_full)

library(visreg)

visreg(full, "Day", by="Direction", gg=TRUE, overlay=TRUE)
visreg(log_full, "Day", by="Direction", gg=TRUE, overlay=TRUE,
       trans = function(x) { x^2 }, partial=TRUE)

m1 =  lm(Count ~ Direction + Day + Month, data=mod_peds)
anova(m1)
summary(m1)
1. Produce plots of pedestrian counts by direction and day, and by direction and month.

When considering modelling the pedestrian counts with a linear model.
 - Which terms would you include?
 - Would any interactions be considered? Which ones, and why?

2. Fit a linear model for pedestrian count with terms for Direction, Day and Month, and produce an ANOVA table.
 - Which terms are important for pedestrian count?
 - How much variation in Counts does this model explain?
 - Which day (after adjusting for Direction and Month) has highest counts on average?

3. The council is interested whether the Direction effect (that more people go To Massey than To City) differs
by day. Add a term to your model to assess this. What is your conclusion?

4. Produce a plot of model diagnostics. What are your conclusions? Frame your answer
in terms of the linear model assumptions.

5. An alternate model is considered, where instead of modelling the raw counts, we model
the square root of the count. This model is implemented below.

Implement the code using anova, summary and plot to allow you to compare the model
to the one above from parts 3 and 4.

What are your conclusions from this model? Have any of your views above changed?

6. Using the square root model, estimate the mean counts of pedestrians going
To Massey on a Friday in October, alongw ith a 95% confidence interval.

Hint: You'll need to undo the square root transformation by squaring the result.


(This can be motivated from the fact that the Poisson distribution's variance is the same as the mean, so increasing mean will result in increasing variance. By square rooting, we can remove this trend removing this trend.)