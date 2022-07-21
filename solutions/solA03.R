# Load up the libraries we need and data
library(tidyverse)
data(quakes)

# A histogram of magnitudes
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag))

# Let's try varying the number of bins:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=20)

# This is a very different shape than with 30 bins!
# 25 bins gives another again:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=25)

# Let's see why this is happening by adding a 'data rug' to the plot.
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag)) +
  geom_rug(mapping=aes(x=mag))

# We can clearly see here from the rug, that our observations are rounded,
# appearing only at every 0.1. So the bar that starts a little higher than 4.0
# and stops a little bit before 4.1 contains no observations!

# What about 20 bins?
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag), bins=20, fill='orange', col='black') +
  geom_rug(mapping=aes(x=mag))

# We can see what is happening here - the very tall bar is covering observations
# of magnitude 4.5 and 4.6 so is counting them both. Whereas most other bars are
# only counting observations ta a single value.

# This is a consequence of the rounding. When data are rounded, changing the number
# of bins in a histogram can dramatically alter the shape. So generally, a histogram
# isn't ideal for these data.

# However, we can work around this by specifying the bins so that they align
# with the rounding in the data. We can do this by specifying the boundary between
# bins to be in-between our observations (e.g. we want them to be half way between
# 4.0 and 4.1, so we want them at 4.05 for example).
# We then specify the binwidth (rather than number of bins) so that our bins all|
# start in-between observations:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=mag),
                 fill='orange',
                 col='black',
                 binwidth=0.2,
                 boundary=0.05,
                 ) +
  geom_rug(mapping=aes(x=mag))

# Another option (often better!) is a kernel density estimate plot, or density plot.
# This can be thought of as a smooth version of a histogram:
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag))

# This looks like it might be slightly oversmoothed? We can alter the amount of
# smoothing by using the `adjust` parameter:

# A value lower than 1 will smooth less. 0.3 is certainly not enough smoothing
# it is massively undersmoothed, but it does show how the kernel density is
# constructed (by placing little 'blobs' centered at each `mag` data point, then
# accummulating these 'blobs' up into hills).
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=0.3) +
  geom_rug(mapping=aes(x=mag))

# We might want a bit more smoothing than that: Let's try something larger than 1:
# This is probably oversmoothed! We're losing detail now - there's just one hill?
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=2) +
  geom_rug(mapping=aes(x=mag))

# Let's try 0.8 - this is still capturing the key trend (most common for earthquakes
# to be around 4.5 magnitude, most occur between 4 and 5.5, a few higher around 6-6.5)
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), adjust=0.8) +
  geom_rug(mapping=aes(x=mag))

# This distribution looks a bit skew to the right (i.e. longer tail on the right hand
# side of the main data center), but this could just be due to there being no earthquakes
# of magnitude less than 4 in the data. Presumably these exist, so the data have been
# truncated.

# We could also try a boxplot: A boxplot shows the 5 number summary (min, lower quartile,
# median, upper quartile, max)
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=mag))

# Conclusion here is roughly the same! Most earthquakes are around 4.6 or so. Around
# half are between the quartiles (inside the box) between 4.3 and 4.9. A quarter
# to the left of the box (below 4.3) and a quarter to the right of the box (above 4.9)

# Again this looks to be a bit skew to the right, however we notice that the median
# is actually bang in the middle of the box. Again, remember that these data are likely
# truncated at 4, so we're not getting a true representation here as we don't have
# all possible magnitudes.

# The observations being singled out on the right are potential outliers or extreme values.
# It's somewhat arbitrary. What happens is the software takes the length of the box
# (inter-quartile range) stretches it by 50% (i.e. 1.5*iqr) and takes that distance above
# and below the box. Anything past those bounds are potential outliers or extreme values.

# Boxplot and density colouring:
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=mag), col='orange', fill='green3')

ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), col='orange', fill='green3', alpha=0.4)

# We can turn off the line for the density by specifying col=NA:
ggplot(data=quakes) +
  geom_density(mapping=aes(x=mag), col=NA, fill='green3', alpha=0.4)

# Let's have a look at depth:
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=depth), bins=20)

ggplot(data=quakes) +
  geom_density(mapping=aes(x=depth), adjust=0.8) +
  geom_rug(mapping=aes(x=depth))

# The histogram and density clearly show two humps. So earthquakes tend to either be
# shallow or deep, and not much in-between.

# A boxplot will not be appropriate for these data as we have two bumps!

# Reason is that when we look at a boxplot, we 'see' the box. Which suggests to us
# a single 'mode' or hump. But these data have two humps, so where the box is is actually
# where the data aren't!
ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=depth))

# This is a bimodal distribution, so a boxplot will not be appropriate. Use a density
# or histogram here.

# What about stations?
ggplot(data=quakes) +
  geom_histogram(mapping=aes(x=stations), bins=20)

ggplot(data=quakes) +
  geom_density(mapping=aes(x=stations), adjust=1) +
  geom_rug(mapping=aes(x=stations))

# Stations is clearly skew to the right which makes sense as it's a count that plausibly
# could be low (in this case I think it's at least 10 - that's probably data truncation
# though!) so the counts can't be less than 10, so the data will tend to bunch up there.

ggplot(data=quakes) +
  geom_boxplot(mapping=aes(x=stations), fill='purple')

# This is classic 'skew to the right' where extreme values tend to be only on the
# right tail - most of the data are bunched on the left. Right hand tail longer than
# the left hand tail, and the median is to the left of the box.


