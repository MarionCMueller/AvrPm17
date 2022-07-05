########## Protoplast data Figure ###########

library(dplyr)
library(ggplot2)
library(reshape2)


protoplast.data<-read.table("Protoplast_data_all.txt",h=T)
protoplast.data$EV<-NULL
protoplast.data.melted<-melt(protoplast.data)
protoplast.data.melted

pdf("Protplast_assay.pdf",h=8, w=7)
ggplot(protoplast.data.melted) +
  geom_boxplot(aes(x=variable, y=value), fill="gold1") +
  geom_point(aes(x=variable, y=value, shape=Rep, size=0.001),position=position_jitterdodge(dodge.width = 0.2)) +
  scale_shape_manual(values=c(3, 16, 17,1,2,8,4,5)) +
  geom_vline(xintercept=1.5, linetype = "longdash" ) + ylab("relative luciferase activity") + theme_bw() +
  theme(axis.text = element_text(size = 20), axis.text.x=element_text(size = 18), axis.title.y=element_text(size=25), axis.title.x=element_blank()) +
  ylim(0,1.8)
dev.off()
