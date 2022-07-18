# Our second R script

# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# Let's plot where the earthquakes are again
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# Let's try some other geometries. First: geom_bin2d
ggplot(data=quakes) +
  geom_bin2d(mapping=aes(x=long, y=lat))

# A 2d density
ggplot(data=quakes) +
  geom_density2d(mapping=aes(x=long, y=lat))

# Showing two geometries
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.5) +
  geom_density2d(mapping=aes(x=long, y=lat),
                 linetype = 'longdash',
                 col='red')

# Finding help on the aesthetics (e.g. linetypes)
vignette("ggplot2-specs")

# Altering the amount of smoothing that the density does via
# the adjust parameter. Smaller than one smoothes less:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.5) +
  geom_density2d(mapping=aes(x=long, y=lat),
                 linetype = 'solid',
                 col='red',
                 adjust = 0.5)

# Larger than one smooths more:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.5) +
  geom_density2d(mapping=aes(x=long, y=lat),
                 linetype = 'solid',
                 col='red',
                 adjust = 2)

# We can tune the amount of smoothing that we want.
# Choosing a small value of adjust will mean we have less
# smoothing, so the smoothed density will be closer to the
# data that we have built it from. But, when a new dataset
# comes along, the density that we build from that data might
# look quite a bit different to this one.

# Choosing a large value of adjust will smooth more. The density
# will not reflect our data quite as well (as we smooth it too
# much), but when we do the same thing on a new dataset, we're
# likely to get a very similar smoothed density. This is the
# bias-variance tradeoff: By reducing bias (smoothing less) we
# increase variance (between data sets). Alternatively we can
# reduce variance (so different datasets produce similar results)
# but as a result each one will be more biased (doesn't fit a
# particularly dataset very well).

ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.5) +
  geom_density2d(mapping=aes(x=long, y=lat),
                 linetype = 'solid',
                 col='red',
                 adjust = 1) +
  labs(x = "Longitude", y = "Latitude",
       title = "There are lots of earthquakes along a ridge around 180 degrees Longitude")

# Having a look at a different plot:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=mag, y=stations))

# These data are rounded, so there may be overplotting where
# more than one datapoint are identical in both x and y so
# show up in exactly the same spot. We could look for this
# by using some transparency:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=mag, y=stations),
             alpha = 0.2)

# We can also do this by jittering the data points: Adding
# a small amount of random noise to the data to get rid of
# the rounding (note that this is just made up though!)
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              alpha=0.3,
              width = 0.06, height = 0)

# We could try adding a density2d like above:
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              alpha=0.3,
              width = 0.04, height = 0) +
  geom_density2d(mapping = aes(x=mag, y=stations))

# Or maybe geom_smooth() - this looks better as it
# helps us tell the story: Increasing magnitude leads
# to more stations detecting the earthquake.
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              alpha=0.3,
              width = 0.04, height = 0) +
  geom_smooth(mapping = aes(x=mag, y=stations))

# Playing around with the smooth (how smooth?)
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              alpha=0.3,
              width = 0.04, height = 0) +
  geom_smooth(mapping = aes(x=mag, y=stations),
              method = 'loess', span = 0.35)

# Another example of the bias-variance tradeoff: reducing
# span means you smooth less which means the curve is closer
# to the data trend. But, a new dataset will have a different
# smooth curve so the variation between datasets increases.

# Increasing span means you smooth more which means the curve
# doesn't fit the data as well. But, a new dataset smoothed
# the same way will result in a very similar smooth trend.
# i.e. variation between datasets is reduced, but bias for
# a particular dataset increases.

ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              alpha=0.3,
              width = 0.04, height = 0) +
  geom_smooth(mapping = aes(x=mag, y=stations),
              method = 'loess', span = 0.6) +
  labs(x = "Magnitude of earthquake",
       y = "Number of stations that detect the earthquake",
       title = "Bigger earthquakes are felt more widely")

