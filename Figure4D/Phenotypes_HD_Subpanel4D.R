###### Boxplots Figure 4D ########

library(ggplot2)
library(dplyr)

### 97223


data97223 <- read.table("High_density_phenotypes_three_replicates_97223.txt",h=T)
data97223
data97223_34 <- data97223 %>% filter(Linecode<3)
data97223_34
wilcox.test(data97223_34$Score~data97223_34$Line, exact=FALSE)
data97223_181 <- data97223 %>% filter(Linecode>2)
data97223_181
wilcox.test(data97223_181$Score~data97223_181$Line, exact=FALSE)

png("HD_97223.png", width=200, height=300)
ggplot(data97223,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()


pdf("HD_97223.pdf", width=2, height=3)
ggplot(data97223,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()



### 49_1

data49_1 <- read.table("High_density_phenotypes_three_replicates_49_1.txt",h=T)
data49_1
data49_1
data49_1_34 <- data49_1 %>% filter(Linecode<3)
wilcox.test(data49_1_34$Score~data49_1_34$Line, exact=FALSE)
data49_1_181 <- data49_1 %>% filter(Linecode>2)
data49_1_181
wilcox.test(data49_1_181$Score~data49_1_181$Line, exact=FALSE)
exactRankTests::wilcox.exact(data49_1_34$Score~data49_1_34$Line)
exactRankTests::wilcox.exact(data49_1_181$Score~data49_1_181$Line)

png("HD_49_1.png", width=200, height=300)
ggplot(data49_1,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_49_1.pdf", width=2, height=3)
ggplot(data49_1,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()


### GZ_6

dataGZ_6 <- read.table("High_density_phenotypes_three_replicates_GZ-6.txt",h=T)
dataGZ_6
dataGZ_6_34 <- dataGZ_6 %>% filter(Linecode<3)
dataGZ_6_34
wilcox.test(dataGZ_6_34$Score~dataGZ_6_34$Line, exact=FALSE)
dataGZ_6_181 <- dataGZ_6 %>% filter(Linecode>2)
dataGZ_6_181
wilcox.test(dataGZ_6_181$Score~dataGZ_6_181$Line, exact=FALSE)
exactRankTests::wilcox.exact(dataGZ_6_34$Score~dataGZ_6_34$Line)
exactRankTests::wilcox.exact(dataGZ_6_181$Score~dataGZ_6_181$Line)

png("HD_GZ_6.png", width=200, height=300)
ggplot(dataGZ_6,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_GZ_6.pdf", width=2, height=3)
ggplot(dataGZ_6,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()



### Shiho

dataShiho <- read.table("High_density_phenotypes_three_replicates_Shiho.txt",h=T)
dataShiho
dataShiho_34 <- dataShiho %>% filter(Linecode<3)
dataShiho_34
wilcox.test(dataShiho_34$Score~dataShiho_34$Line, exact=FALSE)
dataShiho_181 <- dataShiho %>% filter(Linecode>2)
dataShiho_181
wilcox.test(dataShiho_181$Score~dataShiho_181$Line, exact=FALSE)

png("HD_Shiho.png", width=200, height=300)
ggplot(dataShiho,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_Shiho.pdf", width=2, height=3)
ggplot(dataShiho,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

### USA7

dataUSA7 <- read.table("High_density_phenotypes_three_replicates_USA7.txt",h=T)
dataUSA7
dataUSA7_34 <- dataUSA7 %>% filter(Linecode<3)
dataUSA7_34
wilcox.test(dataUSA7_34$Score~dataUSA7_34$Line, exact=FALSE)
dataUSA7_181 <- dataUSA7 %>% filter(Linecode>2)
dataUSA7_181
wilcox.test(dataUSA7_181$Score~dataUSA7_181$Line)
exactRankTests::wilcox.exact(dataUSA7_34$Score~dataUSA7_34$Line)
exactRankTests::wilcox.exact(dataUSA7_181$Score~dataUSA7_181$Line)

png("HD_USA7.png", width=200, height=300)
ggplot(dataUSA7,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_USA7.pdf", width=2, height=3)
ggplot(dataUSA7,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()
### IsR7

dataISR7 <- read.table("High_density_phenotypes_three_replicates_ISR7.txt",h=T)
dataISR7
dataISR7_34 <- dataISR7 %>% filter(Linecode<3)
dataISR7_34
wilcox.test(dataISR7_34$Score~dataISR7_34$Line, exact=FALSE)
dataISR7_181 <- dataISR7 %>% filter(Linecode>2)
dataISR7_181
wilcox.test(dataISR7_181$Score~dataISR7_181$Line, exact=FALSE)
exactRankTests::wilcox.exact(dataISR7_34$Score~dataISR7_34$Line)
exactRankTests::wilcox.exact(dataISR7_181$Score~dataISR7_181$Line)

png("HD_ISR7.png", width=200, height=300)
ggplot(dataISR7,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_ISR7.pdf", width=2, height=3)
ggplot(dataISR7,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue")) + 
  scale_x_discrete(breaks=c(1,2,3, 4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()
### THUN-12


dataTHUN12 <- read.table("High_density_phenotypes_three_replicates_THUN12.txt",h=T)
dataTHUN12
dataTHUN12_34 <- dataTHUN12 %>% filter(Linecode<3)
dataTHUN12_34
wilcox.test(dataTHUN12_34$Score~dataTHUN12_34$Line, exact=FALSE)
dataTHUN12_181 <- dataTHUN12 %>% filter(Linecode>2)
dataTHUN12_181

exactRankTests::wilcox.exact(dataTHUN12_34$Score~dataTHUN12_34$Line)
exactRankTests::wilcox.exact(dataTHUN12_181$Score~dataTHUN12_181$Line)

png("HD_THUN12.png", width=200, height=300)
ggplot(dataTHUN12,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue","cyan3","orange")) + 
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_THUN12.pdf", width=2, height=3)
ggplot(dataTHUN12,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue","cyan3","orange")) + 
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))  + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()


### 96224


data96224 <- read.table("High_density_phenotypes_three_replicates_96224.txt",h=T)
data96224
data96224_34 <- data96224 %>% filter(Linecode<3)
data96224_34
wilcox.test(data96224_34$Score~data96224_34$Line, conf.int=TRUE)
data96224_181 <- data96224 %>% filter(Linecode>2)
data96224_181

exactRankTests::wilcox.exact(data96224_181$Score~data96224_181$Line)
exactRankTests::wilcox.exact(data96224_34$Score~data96224_34$Line)



png("HD_96224.png", width=200, height=300)
ggplot(data96224,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=2, show.legend =FALSE) + 
  theme(text = element_text(size=20), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue","cyan3","orange")) + 
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()

pdf("HD_96224.pdf", width=2, height=3)
ggplot(data96224,aes(x=as.factor(Linecode),y=Score)) + 
  geom_boxplot(width=0.8,position=position_dodge(width =0.6)) +   
  geom_jitter(aes(x=Linecode,y=Score,color=Experiment), height=0.03, width=0.4, size=1.2, show.legend =FALSE) + 
  theme(text = element_text(size=12), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),panel.background = element_rect(fill = "white", colour="black",linetype ="solid")) + 
  scale_colour_manual(values = c("darkgrey","red","darkblue","cyan3","orange")) + 
  scale_x_discrete(breaks=c(1,2,3,4), labels=c("Sis#34", "Pm17#34", "Sis#181", "Pm17#181"), guide = guide_axis(angle = 90))   + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1), limits=c(-0.1,1.1))
dev.off()


      