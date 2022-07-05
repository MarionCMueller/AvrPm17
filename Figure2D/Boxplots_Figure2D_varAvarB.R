########## Boxplot varB ###########

library(dplyr)
library(ggplot2)
library(reshape2)


varAvarB<-read.table("AllvarB.txt",h=T)
varAvarB

png("varAvsvarB.png", width=450, height=560)
ggplot(varAvarB,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=3,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20)) 
dev.off()

varAvarBWilxocon<-read.table("AllvarBWilkocon.txt",h=T)
varAvarBWilxocon
wilcox.test(x=varAvarBWilxocon$varA,y=varAvarBWilxocon$varB,paired=TRUE)
varAvarBWilxocon1 <- varAvarBWilxocon %>% filter(Experiment=="Exp04092020")
varAvarBWilxocon1 
varAvarBWilxocon2 <- varAvarBWilxocon %>% filter(Experiment=="Exp08062019")
varAvarBWilxocon2
varAvarBWilxocon3 <- varAvarBWilxocon %>% filter(Experiment=="Exp28082020")
varAvarBWilxocon3 
wilcox.test(x=varAvarBWilxocon1$varA,y=varAvarBWilxocon1$varB,paired=TRUE)
wilcox.test(x=varAvarBWilxocon2$varA,y=varAvarBWilxocon2$varB,paired=TRUE)
wilcox.test(x=varAvarBWilxocon3$varA,y=varAvarBWilxocon3$varB,paired=TRUE)
