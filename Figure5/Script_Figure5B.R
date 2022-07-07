##### Figure 5B #######
library("ggplot2")


parphe<-read.table("Phenotypes_amigo_96224_and_THUN12.txt", h=T)
parphe

ggplot(parphe,aes(x=Isolate, y=Amigo)) + geom_boxplot() + geom_dotplot(binaxis='y',dotsize=0.5,stackdir='center') +
 stat_summary(fun="mean", color="red", shape=16) +
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
    axis.title=element_text(size=14)) +
  scale_fill_manual(values=c("lightgrey", "lightgrey"))  +
  ylab("Leaf coverage")
