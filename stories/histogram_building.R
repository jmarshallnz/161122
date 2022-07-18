library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)

# histogram building from data (i.e. to show how histograms work)

# generate some data, where each point is separated a bit so the rug is legible
set.seed(3)
dat <- data.frame(value = rnorm(50)) %>%
  arrange(value) %>%
  mutate(diff = value - lag(value)) %>%
  filter(diff > 0.01)

# bin the data
bins <- seq(-2,2,by=0.4)
bin_data <- data.frame(left = bins, right = lead(bins)) %>%
  filter(!is.na(right)) %>%
  mutate(odd = row_number() %% 2) %>%
  mutate(center = (left + right)/2) %>%
  tibble::rowid_to_column("bin")

dat_binned <- expand_grid(dat, bin_data) %>%
  filter(value > left, value < right)

final_dat <- dat_binned %>% mutate(time = sample(n())) %>%
  arrange(time) %>%
  group_by(bin) %>% mutate(ymax = seq_len(n()), ymin = ymax-1) %>%
  ungroup()

# do the animated plot
ggplot(final_dat) +
  geom_rect(mapping=aes(group=time, xmin=left, xmax=right, ymin=ymin, ymax=ymax,), fill='grey50') +
  geom_rug(mapping=aes(x=value), data=dat_binned, size=0.5) +
  geom_rug(mapping=aes(group=time, x=value), size=2) +
  labs(x=NULL) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_continuous(breaks = bins, minor_breaks = NULL) +
  scale_y_continuous(breaks = NULL, minor_breaks = NULL) +
  transition_states(states = time, transition_length = 90, state_length = 10) +
  shadow_mark() +
  enter_drift(y_mod = 12) ->
  anim

animate(anim, renderer = gifski_renderer(file='lectures/graphics/histogram_building.gif', loop=FALSE),
        width = 960, height = 540, units = "px", duration=20, fps=30)

# Now do the same for densities
gen_density <- function(x, vals = seq(-2, 2, by=0.04), bw=0.2) {
  # generate our density centered at x
  d <- 1/(2*pi*bw)*exp(-0.5*((x - vals)/bw)^2)
  # set a bunch to zero where the density contribution is low
  data.frame(x = vals, d = d) %>%
    filter(d > 0.001)
}

dens <- final_dat %>% group_by(time, value) %>%
  summarise(gen_density(value)) %>%
  group_by(x) %>%
  mutate(y_max = cumsum(d), y_min = y_max - d)

ggplot(dens) +
  geom_ribbon(mapping = aes(group=time, x=x, ymin=y_min, ymax=y_max, fill=time)) +
  geom_rug(mapping=aes(x=value), data=dat_binned, size=0.5) +
  geom_rug(mapping=aes(group=time, x=value), data=final_dat, size=2) +
  labs(x=NULL) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_continuous(breaks = NULL, minor_breaks = NULL) +
  scale_y_continuous(breaks = NULL, minor_breaks = NULL, expand=c(0,0)) +
    transition_states(states = time, transition_length = 80, state_length = 20) +
    shadow_mark() +
    enter_drift(y_mod = -0.5) +
  enter_fade() +
  scale_fill_gradient(low = 'grey50', high = 'grey90') +
  guides(fill='none') ->
  anim

animate(anim, renderer = gifski_renderer(file='lectures/graphics/density_building.gif', loop=FALSE),
        width = 960, height = 540, units = "px", duration=20, fps=30)

