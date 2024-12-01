#### Metapopulations
library(tidyverse)

library(gganimate) #Add frames to ggplot
library(gifski) #(Gif output)

##Abbreviations
#x: uninhabited fraction
#y: inhabited fraction
#h: total habitable fraction

#ex: extinction parameter
#col: colonization parameter

#chanX: patch desertion
#chanY: patch colonization

#### Levins metapopulation ####

#Total habitable fraction
hab <- 0.5
#Possible Inhabited
y <- seq(from=0,to=0.5,length.out=10)

##Ex log sequence
col <- 100
ex <- c(c(1,5) %o% 10^(0:5))

chanY_ex_vary <- data.frame( chanX = c(),
                             wy = c(),
                             ex = c())

#chanY for each change in ex
for (i in 1:length(ex)) {
  
  chanY <- -ex[i]*y + col*(hab-y)*y
  
  chanY_temp <- data.frame( chanY, wy = y,
                      ex = rep(ex[i], times=length(chanY)) )
  
  chanY_ex_vary <- rbind(chanY_ex_vary, chanY_temp)
  
}

to_plot <- chanY_ex_vary

#Plot
p <- ggplot(to_plot, aes(wy, chanY)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  labs(title = "ex: {frame_time}") +
  transition_time(ex) +
  ease_aes("linear") #Rate of frames

animate(p, renderer = gifski_renderer())

##As above but col varies
ex <- 100
col <- c(c(1,5) %o% 10^(0:5))
chanY_col_vary <- data.frame( chanX = c(),
                             wy = c(),
                             col = c())
for (i in 1:length(col)) {
  chanY <- -ex*y + col[i]*(hab-y)*y
  chanY_temp <- data.frame( chanY, wy = y,
                            col = rep(col[i], times=length(chanY)) )
  chanY_col_vary <- rbind(chanY_col_vary, chanY_temp)
}
to_plot <- chanY_col_vary
p <- ggplot(to_plot, aes(wy, chanY)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  labs(title = "col: {frame_time}") +
  transition_time(col) +
  ease_aes("linear") #Rate of frames
animate(p, renderer = gifski_renderer())

