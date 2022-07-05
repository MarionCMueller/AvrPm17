########## Boxplot varG ###########

library(dplyr)
library(ggplot2)
library(reshape2)



varAvarG<-read.table("AllvarG.txt",h=T)
varAvarG

pdf("varAvsvarG.pdf", width=3.5, height=4.6)
ggplot(varAvarG,aes(x=as.factor(Effector),y=Hrintensity)) + geom_boxplot(width=0.4,position=position_dodge(width = 0.5)) + geom_jitter(aes(x=as.numeric(Effector)+0.35,y=Hrintensity,colour=Experiment),size=2,width=0.2, show.legend = FALSE) +scale_colour_manual(values = c("orange","red","blue")) + theme_classic() + ylab("HR intensity") + theme(text = element_text(size=20), axis.title=element_blank(),axis.text.x=element_blank()) 
dev.off()

varAvarGWilxocon<-read.table("AllvarGWilkocon.txt",h=T)
varAvarGWilxocon
wilcox.test(x=varAvarGWilxocon$varA,y=varAvarGWilxocon$varG,paired=TRUE)
varAvarGWilxocon1 <- varAvarGWilxocon %>% filter(Experiment=="Exp04092020")
varAvarGWilxocon1 
varAvarGWilxocon2 <- varAvarGWilxocon %>% filter(Experiment=="Exp10092020")
varAvarGWilxocon2
varAvarGWilxocon3 <- varAvarGWilxocon %>% filter(Experiment=="Exp22092020")
varAvarGWilxocon3 
wilcox.test(x=varAvarGWilxocon1$varA,y=varAvarGWilxocon1$varG,paired=TRUE)
wilcox.test(x=varAvarGWilxocon2$varA,y=varAvarGWilxocon2$varG,paired=TRUE)
wilcox.test(x=varAvarGWilxocon3$varA,y=varAvarGWilxocon3$varG,paired=TRUE)
