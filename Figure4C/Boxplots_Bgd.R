########## Boxplot varBgd ###########

library(dplyr)
library(ggplot2)
library(reshape2)


varAvarBgd<-read.table("AllBgd.txt",h=T)
varAvarBgd

png("varAvsvarBgd.png", width=350, height=460)
ggplot(varAvarBgd,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=4,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=30)) 
dev.off()


pdf("varAvsvarBgd.pdf", width=3.5, height=4.6)
ggplot(varAvarBgd,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank())  
dev.off()


varAvarBgdWilxocon<-read.table("AllBgdWilkocon.txt",h=T)
varAvarBgdWilxocon
wilcox.test(x=varAvarBgdWilxocon$varA,y=varAvarBgdWilxocon$varBgd,paired=TRUE)
varAvarBgdWilxocon1 <- varAvarBgdWilxocon %>% filter(Experiment=="Exp08082020")
varAvarBgdWilxocon1 
varAvarBgdWilxocon2 <- varAvarBgdWilxocon %>% filter(Experiment=="Exp30092019")
varAvarBgdWilxocon2
varAvarBgdWilxocon3 <- varAvarBgdWilxocon %>% filter(Experiment=="Exp22092020")
varAvarBgdWilxocon3 
wilcox.test(x=varAvarBgdWilxocon1$varA,y=varAvarBgdWilxocon1$varBgd,paired=TRUE)
wilcox.test(x=varAvarBgdWilxocon2$varA,y=varAvarBgdWilxocon2$varBgd,paired=TRUE)
wilcox.test(x=varAvarBgdWilxocon3$varA,y=varAvarBgdWilxocon3$varBgd,paired=TRUE)

