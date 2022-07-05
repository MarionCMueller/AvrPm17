###### Plot for Figure 5C ###

library(ggplot2)

Phenotypes<-read.csv("Phenotypes_amigo.csv", h=T)

## Make historgram

ggplot(Phenotypes) +geom_histogram(aes(x=Amigo)) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    axis.text = element_text(colour= "black"),
    axis.text.x=element_text(size = 12),
    axis.text.y=element_text(size = 12),
    axis.title=element_text(size=14)) 
