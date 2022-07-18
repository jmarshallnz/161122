# Our first R script

# Lines starting with a # are comments. You can write anything you like there and
# it will be ignored when 'run' in R/RStudio. These are useful for adding notes
# to yourself for later.

# This first command loads up the `tidyverse` library/package which includes
# `ggplot2` for creating pretty charts.
library(tidyverse)

# Now we'll load some data on earthquakes from around Fiji
data(quakes)

# Let's plot where the earthquakes are
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat))

# Let's break down this code piece by piece:
# The ggplot() bit produces a fresh plot and associates our data
# 'quakes' with it.
ggplot(data=quakes)

# what about the aes() bit?
# Running the aes() bit by itself is an example of an expression:
aes(x=long, y=lat)

# This is an example of an assignment, where we take the output
# from an expression (the aes bit) and assign it (via =) to the
# object called 'mapping'
mapping=aes(x=long, y=lat)
mapping

# the geom_point bit create a 'layer' of geometry in the form
# of points that could be added to a plot.
geom_point(mapping = aes(x=long, y=lat))

# bringing it all together:
ggplot(data=quakes) +
  geom_point(mapping = aes(x=long, y=lat))

# Let's try altering the plot by changing colour etc.
# When specifying colour names (or names in general for things on the plot),
# e.g. for plot labels (title, subtitle, x axis label etc) we
# use quotes around the character string/word:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour='maroon', size=2)

ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour="maroon", size=2)

# To find all the colours we can use colors()
colors()
colours()

# Let's try some other stuff.
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour="maroon", size=3)

# Finding help on geom_point: Place cursor on function name
# and press F1, or type ? in front of function names:
?geom_point
# we find the aesthetics, and a note to read more using:
vignette("ggplot2-specs")
# Names of shapes:
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)
shape_names

# Adding some transparency to reduce overplotting:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour="maroon", size=3,
             alpha = 0.5)

ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour="maroon", size=3,
             alpha = 0.1)

ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat), colour="maroon", size=3,
             alpha = 0.8)

# Changing these aesthetics based on the data: Put it inside the aes()
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat, colour=mag), size=3,
             alpha = 0.8)

# Let's try stations instead of magnitude:
ggplot(data=quakes) +
  geom_point(mapping=aes(x=long, y=lat, colour=stations), size=3,
             alpha = 0.8)

# These trends are pretty much the same! This makes sense as
# larger earthquakes (high magnitude) will be felt at more
# stations.

