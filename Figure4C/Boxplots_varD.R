########## Boxplot varD ###########

library(dplyr)
library(ggplot2)
library(reshape2)


varAvarD<-read.table("AllvarD.txt",h=T)
varAvarD

png("varAvsvarD.png", width=350, height=460)
ggplot(varAvarD,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=4,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=30)) 
dev.off()

pdf("varAvsvarD.pdf", width=3.5, height=4.6)
ggplot(varAvarD,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank()) 
dev.off()

varAvarDWilxocon<-read.table("AllvarDWilkocon.txt",h=T)
varAvarDWilxocon
wilcox.test(x=varAvarDWilxocon$varA,y=varAvarDWilxocon$varD,paired=TRUE)
varAvarDWilxocon1 <- varAvarDWilxocon %>% filter(Experiment=="Exp04092020")
varAvarDWilxocon1 
varAvarDWilxocon2 <- varAvarDWilxocon %>% filter(Experiment=="Exp10092020")
varAvarDWilxocon2
varAvarDWilxocon3 <- varAvarDWilxocon %>% filter(Experiment=="Exp31082020")
varAvarDWilxocon3 
wilcox.test(x=varAvarDWilxocon1$varA,y=varAvarDWilxocon1$varD,paired=TRUE)
wilcox.test(x=varAvarDWilxocon2$varA,y=varAvarDWilxocon2$varD,paired=TRUE)
wilcox.test(x=varAvarDWilxocon3$varA,y=varAvarDWilxocon3$varD,paired=TRUE)
