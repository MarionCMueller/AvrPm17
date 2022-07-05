### Plot Figure 5D #####
library(reshape2)
library(dplyr)
library(ggplot2)
library(qtl)
library(gridExtra)

scanonePm17cg<-read.csv("scanonePm17cg.csv",h=T)
scanonePm17cg

scanonePm17cgch1<-scanonePm17cg %>% filter(chr==1)
scanonePm17cgch2<-scanonePm17cg %>% filter(chr==2)
scanonePm17cgch3<- scanonePm17cg %>% filter(chr==3)
scanonePm17cgch4<-scanonePm17cg %>% filter(chr==4)
scanonePm17cgch5<-scanonePm17cg %>% filter(chr==5)
scanonePm17cgch6<-scanonePm17cg %>% filter(chr==6)
scanonePm17cgch7<-scanonePm17cg %>% filter(chr==7)
scanonePm17cgch8<-scanonePm17cg %>% filter(chr==8)
scanonePm17cgch9<-scanonePm17cg %>% filter(chr==9)
scanonePm17cgch10<-scanonePm17cg %>% filter(chr==10)
scanonePm17cgch11<-scanonePm17cg %>% filter(chr==11)

plot1<-ggplot(scanonePm17cgch1,aes(x=pos,y=lod,)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD")
plot2<-ggplot(scanonePm17cgch2,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300,400))
plot3<-ggplot(scanonePm17cgch3,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200))
plot4<-ggplot(scanonePm17cgch4,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300))
plot5<-ggplot(scanonePm17cgch5,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300,400,500))
plot6<-ggplot(scanonePm17cgch6,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300,400))
plot7<-ggplot(scanonePm17cgch7,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300,400,500))
plot8<-ggplot(scanonePm17cgch8,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept =3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300))
plot9<-ggplot(scanonePm17cgch9,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept =3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300,400,500))
plot10<-ggplot(scanonePm17cgch10,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.4)) +ylab("LOD") + scale_x_discrete(limits=c(0,100,200,300))
plot11<-ggplot(scanonePm17cgch11,aes(x=pos,y=lod)) + geom_line(size=0.4) + geom_hline(yintercept = 3.28, color="red", linetype=2, size=0.5) + theme_bw() + ylim(c(0,16)) + theme(text = element_text(size=14),axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),axis.title.y=element_blank(),plot.margin = unit(c(0.02, 0.065, 0, 0), "cm"),axis.text.x = element_text(angle = 90, vjust = 0.75)) +ylab("LOD") + scale_x_discrete(limits=c(0,75,150))
plot11
grid.arrange(arrangeGrob(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11, nrow=1,ncol=11, widths=c(4.5,5.5,2.5,3.5,5.5,4.0,5.0,4.5,5,3,1.8)))

