# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# A histogram of magnitudes
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag))

# This distribution shows that most quakes have magnitude 4-5, so the measure of center
# would be around 4.5 magnitude (the mode). The spread is from 4 up to 6.5 (the range).
# And the shape is skew to the right, in that most data is bunched up on the left, but
# the 'tail' of large magnitude quakes goes out to the right.

# 30 bins is the default. We can change that with bins:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=10)
# Same conclusion from this one, but the display looks courser. The 'chunkiness' of
# the plot is very apparent.
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=17)
# With these data, the number of bins makes the plot look quite different!

# The reason is that the data here are rounded.
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=30, alpha=0.2, col='black') +
  geom_rug(mapping=aes(x=mag), col='red')
# The rug shows us where the data are, so we can get a feel for why the
# histogram is responding the way it is: As the data are rounded, some of the bins
# consist of the quakes from two different magnitudes:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=20, alpha=0.2, col='black') +
  geom_rug(mapping=aes(x=mag), col='red')
# e.g the tall spike here has magnitude 4.5 and 4.6 quakes contributing.

# and some consist of bins with no data:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=30, alpha=0.2, col='black') +
  geom_rug(mapping=aes(x=mag), col='red')
# such as the bin between 4.0 and 4.1

# An alternative is to specify the bin width and then the start of the bin.
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), binwidth = 0.2, boundary = 0.05, alpha=0.2, col='black') +
  geom_rug(mapping=aes(x=mag), col='red')

ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), binwidth = 0.1, boundary = 0.05, alpha=0.2, col='black') +
  geom_rug(mapping=aes(x=mag), col='red')

# The density plot gets rid of some of the problems with the histogram. It can be
# thought of as a smooth histogram:
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag)) +
  geom_rug(mapping=aes(x=mag), col='red')

# Can change the amount of smoothness: adjust > 1 smooths more:
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=4) +
  geom_rug(mapping=aes(x=mag), col='red')
# This is probably oversmoothed.
#
# adjust < 1 smooths less:
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=0.3) +
  geom_rug(mapping=aes(x=mag), col='red')
# This is undersmoothed way too much - but, it helps us see how the density
# plot is built up.
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=0.6) +
  geom_rug(mapping=aes(x=mag), col='red')
# Even with adjust=0.6 we're probably still undersmoothing.
# Typically the default (adjust=1) works well for most datasets.

ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=0.6, col='darkgreen', fill='pink', alpha=0.5) +
  geom_rug(mapping=aes(x=mag), col='red')

# The boxplot plots the minimum, lower quartile, median, upperquartile and maximum.
# The five number summary.
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=mag))
# From here, we'd conclude center (median)  is at 4.6, and half the data are within the box
# between 4.3 and 4.9. We get shape information from where the box is (and maybe where
# the median is within the box).
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=mag), col='blue', fill='orange')

# Let's have a look at depth:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=depth))
ggplot(data=quakes) +
  geom_density(mapping=aes(x=depth))
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=depth))
# Boxplot completely inappropriate here, as we have two peaks. Generally
# a boxplot should only be used if you have a single peak or hump (unimodal).

# Otherwise, stick to a density or histogram.
# This distribution has two humps - most earthquakes are either shallow (small depth)
# or deep (large depth). Few are in between. So the measure of center here isn't
# all that useful.

# Let's have a look at stations:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=stations))
ggplot(data=quakes) +
  geom_density(mapping=aes(x=stations))
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=stations))
# This distribution is more skew than the others. Skew to the right (i.e. the tail
# of extreme values is out to the right, and most data is to the small values).
# Most eqarthquakes are felt at are around 25 stations.

# The outliers shown as individual points on the boxplot are determined by an
# algorithm. Anything further than 1.5 interquartile ranges (length of box) away from
# the ends of the box is considered extreme.

# If you have just one or two with a big gap back to the next ones, it's worth checking
# the data for coding errors. Otherwise, don't worry too much!
