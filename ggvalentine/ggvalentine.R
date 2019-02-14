library(tidyverse)
library(gganimate)

t<-rnorm(10000,0,1.8)

x<-16*(sin(t)^3)
y<-13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
z <- 1:length(x)

rainbow <- t(hcl(seq(0, 360, length = 300 * 50), 90, 70))

dat <- data.frame(x = x, y = y, z = as.numeric(z))

p <- ggplot(dat,aes(x,y)) +
  annotation_raster(rainbow, -Inf, Inf, -Inf, Inf) +
  geom_point(colour="white",alpha=0.1) +
  theme_void() 

p2 <- p + geom_text(aes(x=0, y=3, label="Roses are red"),
              size=9, color="white") + 
  geom_text(aes(x=0, y=0, label="Violets are  blue"),
            size=9, color="white") + 
  geom_text(aes(x=0, y=-3, label="Unexpected ) on line 42"),
            size=9, color="white") + 
  geom_text(aes(x=0, y=-5, label=""), size=1, color="white") +
  transition_layers(layer_length = 0.3, transition_length = 0.5,
                    from_blank = FALSE) +
  enter_grow()

anim_save(filename = "ggvalentine.gif",p2, path="C:/Users/Jordan/Documents/")
