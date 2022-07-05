########## Boxplot varC ###########

library(dplyr)
library(ggplot2)
library(reshape2)


varAvarC<-read.table("AllvarC.txt",h=T)
varAvarC

png("varAvsvarC.png", width=350, height=460)
ggplot(varAvarC,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=4,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue","black")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=30)) 
dev.off()

### PDF
pdf("varAvsvarC.pdf", width=3.5, height=4.6)
ggplot(varAvarC,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue","black")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank()) 
dev.off()


wilcox.test(x=varAvarCWilxocon$varA,y=varAvarCWilxocon$varC,paired=TRUE)

varAvarCWilxocon<-read.table("AllvarCWilkocon.txt",h=T)
varAvarCWilxocon
varAvarCWilxocon1 <- varAvarCWilxocon %>% filter(Experiment=="Exp08082020")
varAvarCWilxocon1 
varAvarCWilxocon2 <- varAvarCWilxocon %>% filter(Experiment=="Exp14082020")
varAvarCWilxocon2 
varAvarCWilxocon3 <- varAvarCWilxocon %>% filter(Experiment=="Exp03102019")
varAvarCWilxocon3 
wilcox.test(x=varAvarCWilxocon1$varA,y=varAvarCWilxocon1$varC,paired=TRUE)
wilcox.test(x=varAvarCWilxocon2$varA,y=varAvarCWilxocon2$varC,paired=TRUE)
wilcox.test(x=varAvarCWilxocon3$varA,y=varAvarCWilxocon3$varC,paired=TRUE)

