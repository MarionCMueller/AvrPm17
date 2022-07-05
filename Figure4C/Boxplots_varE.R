########## Boxplot varE ###########

library(dplyr)
library(ggplot2)
library(reshape2)


setwd("C:/Users/Marion/Documents/AvrPm17_version4/Revisions_PNAS/HR_boxplots_new/VarA_vs_VarE/")
varAvarE<-read.table("AllDatavarE.txt",h=T)
varAvarE

png("varAvsvarE.png", width=350, height=460)
ggplot(varAvarE,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=4,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=30)) 
dev.off()
pdf("varAvsvarE.pdf", width=3.5, height=4.6)
ggplot(varAvarE,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank()) 
dev.off()

varAvarEWilxocon<-read.table("AllDatavarEWilcoson.txt",h=T)
varAvarEWilxocon
wilcox.test(x=varAvarEWilxocon$varA,y=varAvarEWilxocon$varE,paired=TRUE)
varAvarEWilxocon1 <- varAvarEWilxocon %>% filter(Experiment=="Exp08082020")
varAvarEWilxocon1 
varAvarEWilxocon2 <- varAvarEWilxocon %>% filter(Experiment=="Exp03102019")
varAvarEWilxocon2
varAvarEWilxocon3 <- varAvarEWilxocon %>% filter(Experiment=="Exp14082020")
varAvarEWilxocon3 
wilcox.test(x=varAvarEWilxocon1$varA,y=varAvarEWilxocon1$varE,paired=TRUE)
wilcox.test(x=varAvarEWilxocon2$varA,y=varAvarEWilxocon2$varE,paired=TRUE)
wilcox.test(x=varAvarEWilxocon3$varA,y=varAvarEWilxocon3$varE,paired=TRUE)
