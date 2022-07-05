########## Boxplot varB/varC ###########

library(dplyr)
library(ggplot2)
library(reshape2)



varBvarC<-read.table("AllvarBvarC.txt",h=T)
varBvarC

pdf("varBvsvarC.pdf", width=8, height=10)
ggplot(varBvarC,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=3,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + 
  theme(text = element_text(size=35),axis.title.x=element_blank()) +
  scale_x_discrete(labels=c("varB","varC"))
dev.off()

varAvarBCWilxocon<-read.table("AllvarBvarCWilkocon.txt",h=T)
varAvarBCWilxocon
wilcox.test(x=varAvarBCWilxocon$varB,y=varAvarBCWilxocon$varC,paired=TRUE)
varAvarBCWilxocon1 <- varAvarBCWilxocon %>% filter(Experiment=="Exp11102019")
varAvarBCWilxocon1 
varAvarBCWilxocon2 <- varAvarBCWilxocon %>% filter(Experiment=="Exp10032019")
varAvarBCWilxocon2
varAvarBCWilxocon3 <- varAvarBCWilxocon %>% filter(Experiment=="Exp01012022")
varAvarBCWilxocon3 
wilcox.test(x=varAvarBCWilxocon1$varB,y=varAvarBCWilxocon1$varC,paired=TRUE)
wilcox.test(x=varAvarBCWilxocon2$varB,y=varAvarBCWilxocon2$varC,paired=TRUE)
wilcox.test(x=varAvarBCWilxocon3$varB,y=varAvarBCWilxocon3$varC,paired=TRUE)

