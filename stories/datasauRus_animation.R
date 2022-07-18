library(datasauRus)
library(ggplot2)
library(gganimate)

g <- ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point(size=3, alpha=0.4)+
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  transition_states(dataset, 3, 3) + 
  ease_aes('cubic-in-out')

a <- animate(g, nframes = 12*18, fps = 12, height = 640, width = 640)

anim_save("lectures/graphics/datasaurus.gif", a)
