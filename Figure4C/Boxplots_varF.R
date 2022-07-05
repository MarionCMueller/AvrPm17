########## Boxplot varF ###########

library(dplyr)
library(ggplot2)
library(reshape2)


varAvarF<-read.table("AllDatavarF.txt",h=T)
varAvarF

png("varAvsvarF.png", width=350, height=460)
ggplot(varAvarF,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=4,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=30)) 
dev.off()

pdf("varAvsvarF.pdf", width=3.5, height=4.6)
ggplot(varAvarF,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank()) 
dev.off()

varAvarFWilxocon<-read.table("AllDatavarF_wilkocon.txt",h=T)
varAvarFWilxocon
wilcox.test(x=varAvarFWilxocon$VarA,y=varAvarFWilxocon$varF,paired=TRUE)
varAvarFWilxocon1 <- varAvarFWilxocon %>% filter(Experiment=="Exp04092020")
varAvarFWilxocon1 
varAvarFWilxocon2 <- varAvarFWilxocon %>% filter(Experiment=="Exp21082020")
varAvarFWilxocon2
varAvarFWilxocon3 <- varAvarFWilxocon %>% filter(Experiment=="Exp22092020")
varAvarFWilxocon3 
wilcox.test(x=varAvarFWilxocon1$VarA,y=varAvarFWilxocon1$varF,paired=TRUE)
wilcox.test(x=varAvarFWilxocon2$VarA,y=varAvarFWilxocon2$varF,paired=TRUE)
wilcox.test(x=varAvarFWilxocon3$VarA,y=varAvarFWilxocon3$varF,paired=TRUE)
