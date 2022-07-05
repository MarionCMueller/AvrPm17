###############################################
#### Script to create Figures S16 #############
###############################################


library(ggplot2)

#### 96224 vs. 96224 ####

cut -d" " -f7-40 muscle-I20200828-163412-0498-28454690-p2m.aln | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f16-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > DR2spliced_vs_DR2spliced.txt


DR2spliced_vs_DR2spliced<-read.table("DR1spliced_vs_DR2spliced.txt")
DR2spliced_vs_DR2spliced

SlidingDR1vsDR2<-"NA"
SlidingDR1vsDR2$Position<-seq(1,length(DR2spliced_vs_DR2spliced$V1)-49,by=1)
SlidingDR1vsDR2$Identity<-"NA"
SlidingDR1vsDR2<-cbind(SlidingDR1vsDR2$Position,SlidingDR1vsDR2$Identity)
SlidingDR1vsDR2<-as.data.frame(SlidingDR1vsDR2)
names(SlidingDR1vsDR2)<-c("Position","Identity")
SlidingDR1vsDR2
for(i in 1:length(DR2spliced_vs_DR2spliced$V1-49)){SlidingDR1vsDR2$Identity[i]<-sum(DR2spliced_vs_DR2spliced[i:(i+49),1])/50}
SlidingDR1vsDR2$Position<-as.numeric(SlidingDR1vsDR2$Position)
SlidingDR1vsDR2$Identity<-as.numeric(SlidingDR1vsDR2$Identity)

write.csv(SlidingDR1vsDR2,"SlidingDR1vsDR2.csv")
png("DR1DR2.png",width = 900,height=250)
ggplot(SlidingDR1vsDR2) + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100)) + scale_y_continuous(limits=c(0.6,1.05)) 
dev.off()

#### THUN12 vs. THUN12 ####

cut -d" " -f7-52 muscle-I20200828-180329-0143-17185744-p2m.clw | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f30-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > GZ6DR2splice_vs_DR2spliced.txt

TH12DR1spliced_vs_TH12DR2spliced<-read.table("TH12DR1spliced_vs_TH12DR2spliced.txt")
TH12DR1spliced_vs_TH12DR2spliced


SlidingTH12DR1vsTH12DR2<-"NA"
SlidingTH12DR1vsTH12DR2$Position<-seq(1,length(TH12DR1spliced_vs_TH12DR2spliced$V1)-49,by=1)
SlidingTH12DR1vsTH12DR2$Identity<-"NA"
SlidingTH12DR1vsTH12DR2<-cbind(SlidingTH12DR1vsTH12DR2$Position,SlidingTH12DR1vsTH12DR2$Identity)
SlidingTH12DR1vsTH12DR2<-as.data.frame(SlidingTH12DR1vsTH12DR2)
names(SlidingTH12DR1vsTH12DR2)<-c("Position","Identity")
SlidingTH12DR1vsTH12DR2
for(i in 1:length(TH12DR1spliced_vs_TH12DR2spliced$V1-49)){SlidingTH12DR1vsTH12DR2$Identity[i]<-sum(TH12DR1spliced_vs_TH12DR2spliced[i:(i+49),1])/50}
SlidingTH12DR1vsTH12DR2$Position<-as.numeric(SlidingTH12DR1vsTH12DR2$Position)
SlidingTH12DR1vsTH12DR2$Identity<-as.numeric(SlidingTH12DR1vsTH12DR2$Identity)
png("TH12DR1TH12DR2.png",width = 900,height=250)
ggplot(SlidingTH12DR1vsTH12DR2) + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100))  + scale_y_continuous(limits=c(0.6,1.05))
dev.off()
write.csv(SlidingTH12DR1vsTH12DR2,"SlidingTH12DR1vsTH12DR2.csv")

### GZ-6 vs DR1

cut -d" " -f7-52 muscle-GZ6_DR1.clw | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f16-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > GZ6_DR1.txt

GZ6spliced_vs_DR1spliced<-read.table("GZ6_DR1.txt")
GZ6spliced_vs_DR1spliced


SlidingGZ6vsDR1<-"NA"
SlidingGZ6vsDR1$Position<-seq(1,length(GZ6spliced_vs_DR1spliced$V1)-49,by=1)
SlidingGZ6vsDR1$Identity<-"NA"
SlidingGZ6vsDR1<-cbind(SlidingGZ6vsDR1$Position,SlidingGZ6vsDR1$Identity)
SlidingGZ6vsDR1<-as.data.frame(SlidingGZ6vsDR1)
names(SlidingGZ6vsDR1)<-c("Position","Identity")
SlidingGZ6vsDR1
for(i in 1:length(GZ6spliced_vs_DR1spliced$V1-49)){SlidingGZ6vsDR1$Identity[i]<-sum(GZ6spliced_vs_DR1spliced[i:(i+49),1])/50}
SlidingGZ6vsDR1$Position<-as.numeric(SlidingGZ6vsDR1$Position)
SlidingGZ6vsDR1$Identity<-as.numeric(SlidingGZ6vsDR1$Identity)
png("GZ6DR1.png",width = 900,height=250)
ggplot(SlidingGZ6vsDR1)  + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100))  + scale_y_continuous(limits=c(0.6,1.05))
dev.off()
write.csv(SlidingGZ6vsDR1,"SlidingGZ6vsDR1.csv")



### GZ-6 vs DR2

cut -d" " -f7-52 GZ6_DR2.clw | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f16-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > GZ6_DR2.txt

GZ6spliced_vsDR2_spliced<-read.table("GZ6_DR2.txt")
GZ6spliced_vsDR2_spliced


SlidingGZ6vsDR2<-"NA"
SlidingGZ6vsDR2$Position<-seq(1,length(GZ6spliced_vsDR2_spliced$V1)-49,by=1)
SlidingGZ6vsDR2$Identity<-"NA"
SlidingGZ6vsDR2<-cbind(SlidingGZ6vsDR2$Position,SlidingGZ6vsDR2$Identity)
SlidingGZ6vsDR2<-as.data.frame(SlidingGZ6vsDR2)
names(SlidingGZ6vsDR2)<-c("Position","Identity")
SlidingGZ6vsDR2
for(i in 1:length(GZ6spliced_vsDR2_spliced$V1-49)){SlidingGZ6vsDR2$Identity[i]<-sum(GZ6spliced_vsDR2_spliced[i:(i+49),1])/50}
SlidingGZ6vsDR2$Position<-as.numeric(SlidingGZ6vsDR2$Position)
SlidingGZ6vsDR2$Identity<-as.numeric(SlidingGZ6vsDR2$Identity)
png("GZ6DR2.png",width = 900,height=250)
ggplot(SlidingGZ6vsDR2) + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100))  + scale_y_continuous(limits=c(0.6,1.05))
dev.off()
write.csv(SlidingGZ6vsDR2,"SlidingGZ6vsDR2.csv")


### GZ-6 vs THDR1

cut -d" " -f7-52 muscle-GZ6_TH12DR1.clw | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f30-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > GZ6_TH12DR1.txt

GZ6spliced_vs_TH12DR1spliced<-read.table("GZ6_TH12DR1.txt")
GZ6spliced_vs_TH12DR1spliced


SlidingGZ6vsTH12DR1<-"NA"
SlidingGZ6vsTH12DR1$Position<-seq(1,length(GZ6spliced_vs_TH12DR1spliced$V1)-49,by=1)
SlidingGZ6vsTH12DR1$Identity<-"NA"
SlidingGZ6vsTH12DR1<-cbind(SlidingGZ6vsTH12DR1$Position,SlidingGZ6vsTH12DR1$Identity)
SlidingGZ6vsTH12DR1<-as.data.frame(SlidingGZ6vsTH12DR1)
names(SlidingGZ6vsTH12DR1)<-c("Position","Identity")
SlidingGZ6vsTH12DR1
for(i in 1:length(GZ6spliced_vs_TH12DR1spliced$V1-49)){SlidingGZ6vsTH12DR1$Identity[i]<-sum(GZ6spliced_vs_TH12DR1spliced[i:(i+49),1])/50}
SlidingGZ6vsTH12DR1$Position<-as.numeric(SlidingGZ6vsTH12DR1$Position)
SlidingGZ6vsTH12DR1$Identity<-as.numeric(SlidingGZ6vsTH12DR1$Identity)

png("GZ6TH12DR1.png",width = 900,height=250)
ggplot(SlidingGZ6vsTH12DR1) + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100))  + scale_y_continuous(limits=c(0.6,1.05))
dev.off()
write.csv(SlidingGZ6vsTH12DR1,"SlidingGZ6vsTH12DR1.csv")


### GZ-6 vs THDR2


cut -d" " -f7-52 muscle-GZ6_TH12DR1.clw | awk 'NR %2 == 0' | awk 'NR %2 == 1' | cut -d" " -f30-100 | sed s'/\*/1\n/g' | sed s'/ /0\n/g' > GZ6_TH12DR1.txt

GZ6spliced_vs_TH12DR2spliced<-read.table("GZ6_TH12DR2.txt")
GZ6spliced_vs_TH12DR2spliced


SlidingGZ6vsTH12DR2<-"NA"
SlidingGZ6vsTH12DR2$Position<-seq(1,length(GZ6spliced_vs_TH12DR2spliced$V1)-49,by=1)
SlidingGZ6vsTH12DR2$Identity<-"NA"
SlidingGZ6vsTH12DR2<-cbind(SlidingGZ6vsTH12DR2$Position,SlidingGZ6vsTH12DR2$Identity)
SlidingGZ6vsTH12DR2<-as.data.frame(SlidingGZ6vsTH12DR2)
names(SlidingGZ6vsTH12DR2)<-c("Position","Identity")
SlidingGZ6vsTH12DR2
for(i in 1:length(GZ6spliced_vs_TH12DR2spliced$V1-49)){SlidingGZ6vsTH12DR2$Identity[i]<-sum(GZ6spliced_vs_TH12DR2spliced[i:(i+49),1])/50}
SlidingGZ6vsTH12DR2$Position<-as.numeric(SlidingGZ6vsTH12DR2$Position)
SlidingGZ6vsTH12DR2$Identity<-as.numeric(SlidingGZ6vsTH12DR2$Identity)

png("GZ6TH12DR2.png",width = 900,height=250)
ggplot(SlidingGZ6vsTH12DR2) + geom_rect(xmin=813,xmax=1202, ymin=0.95, ymax=1.05, fill="yellow", alpha=0.2) + geom_line(aes(Position,Identity), color="darkblue", size=1)  + theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),text = element_text(size=20)) + scale_x_continuous(limits=c(0,2100))  + scale_y_continuous(limits=c(0.6,1.05))
dev.off()
write.csv(SlidingGZ6vsTH12DR2,"SlidingGZ6vsTH12DR2.csv")

