# Our second R script

# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# Let's plot where the earthquakes are again
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# We can try geom_bin2d instead to see what that does:
ggplot(data=quakes) +
  geom_bin2d(mapping=aes(x=long, y=lat))

# this is showing us that a bunch of earthquakes occur along
# a line around 180 degrees longitude.
ggplot(data=quakes) +
  geom_density2d(mapping=aes(x=long, y=lat))

# This is done by popping a little blob of 'jelly' on each of
# the 1000 points. Where points are close to each other the jelly
# accumulates into mountains.
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.1)

# We can change the colour with `col` or `color` or `colour`. The width
# of the lines are specified with `size`.

# The vignette can tell us more:
vignette("ggplot2-specs")

# So to use dashed lines:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.5,
                 linetype='dashed')


# We can also adjust the amount of 'smoothing' that is being
# applied when using the density geometry:
# The default smooths based on some criteria that tends to work
# well but is nonetheless somewhat arbitrary.
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.5,
                 linetype='solid')

# We can adjust the amount of smoothing via the `adjust` parameter.
# Default is 1, values less than one will smooth less, and values
# greater than one will smooth more.

# Less smoothing:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.5,
                 linetype='solid', adjust=0.5)

# More smoothing:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.5,
                 linetype='solid', adjust=2)

# When we smooth only a little (i.e. adjust < 1) then our density estimate
# follows our data better, capturing detail.

# When we smooth a lot (adjust > 1) then our density estimate starts 'smooshing'
# the data so we're not really capturing the data intricacies or detail.

# When considering sample to sample variation, if we smooth only a little,
# then the density estimate from the two samples may differ (potentially quite
# a bit) as the two samples will differ a little bit.

# If we smooth a lot, then our sample to sample variation of the density
# will be expected to be small, because we're smoothing a lot, so any differences
# will be smoothed over.

# So there's a trade-off: Smoothing a little bit gets us closer to the
# data we have in front of us. But, it might mean when we get a new sample
# our 'trend' from the density will differ a bunch.

# Smoothing a lot will leave us further away from the data in front of us,
# (we're not getting the detail) but the sample to sample variation will
# be low.

# This is known as the Bias-Variance trade-off.

# Tidy up the axis labels and go back to the default level of smoothing:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), alpha=0.3) +
  geom_density2d(mapping=aes(x=long, y=lat), col='dark red', size=0.5,
                 linetype='solid', adjust=1) +
  labs(x = "Longitude", y="Latitude",
       title = "Earthquakes around Fiji",
       subtitle = "Many earthquakes occur along a ridge around Longitude 180")

# Improving a plot:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=mag, y=stations))

# We notice that the magnitude variable has been rounded. This has led
# to overplotting.
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.06, height = 0)

# Jitter adds some random noise to the data. useful if the data has
# been rounded (artificially). Nonetheless, this is altering the
# data, so should be used with caution.

# The option to deal with overplotting is to use transparency:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=mag, y=stations),
             alpha = 0.2)

# Or you could combine them:
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.04, height = 0, alpha=0.3)

# What about adding some smoothing to get the underlying trend?

# We could try density2d again: This shows the areas of the x/y
# grid that have the most points in them.
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.04, height = 0, alpha=0.3) +
  geom_density2d(mapping=aes(x=mag, y=stations))

# In this case we probably want the trend in the data instead,
# which geom_smooth() gives us:
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.04, height = 0, alpha=0.3) +
  geom_smooth(mapping=aes(x=mag, y=stations))

# Just like with the density2d smoother, we can mess about with
# how smooth the geom_smooth is.

# To do this we switch the type of smoother to a `loess` and
# adjust the `span`:
ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.04, height = 0, alpha=0.3) +
  geom_smooth(mapping=aes(x=mag, y=stations),
              method='loess', span=0.4)

ggplot(data=quakes) +
  geom_jitter(mapping=aes(x=mag, y=stations),
              width = 0.04, height = 0, alpha=0.3) +
  geom_smooth(mapping=aes(x=mag, y=stations),
              method='loess', span=0.4) +
  labs(x="Magnitude", y="Number of stations",
       title="Larger earthquakes are detected at more stations")
