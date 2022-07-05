####### Coverage all isolates ################

setwd("av_cov/")
filelist = list.files(pattern = "*.txt")
filelist
av_covA_new<-""
for(i in 1:length(filelist)){ av_cov<-read.table(filelist[i],h=F,sep=",")
av_val<-mean(av_cov$V2)
av_covA<-av_cov %>% mutate(Normalized=V2/av_val)
av_covA<-rbind(av_covA,av_covA_new)
av_covA_new<-av_covA

}
write.table(av_covA_new, "../av_covA_new.txt")

#### Commands run in bash: 
grep Bgt-51729 av_covA_new.txt > av_covA_new_Bgt-51729 ;
grep Bgt-51731 av_covA_new.txt > av_covA_new_Bgt-51731 ;
grep Bgt-712 av_covA_new.txt > av_covA_new_Bgt-712 ;
grep Bgt-715 av_covA_new.txt > av_covA_new_Bgt-715 ;
grep Bgt-940 av_covA_new.txt > av_covA_new_Bgt-940 ;
grep Bgt-1043 av_covA_new.txt > av_covA_new_Bgt-1043 ;
grep Bgt-1486 av_covA_new.txt > av_covA_new_Bgt-1486 ;
grep Bgt-1178 av_covA_new.txt > av_covA_new_Bgt-1178 ;

sed s'/av_cov_Bgt-51729_//g' av_covA_new_Bgt-51729 | cut -d" " -f2,3,4 > av_covA_new_Bgt-51729_renamed ;
sed s'/av_cov_Bgt-51731_//g' av_covA_new_Bgt-51731 | cut -d" " -f2,3,4 > av_covA_new_Bgt-51731_renamed ;
cat av_covA_new_Bgt-51729_renamed av_covA_new_Bgt-51731_renamed > av_covA_new_Bgt-51729_Bgt-51731

####

databgt712<-read.table("../av_covA_new_Bgt-712",h=F)
databgt712
png("Bgt-712.png")
ggplot(databgt712) + geom_histogram(aes(x=V4)) + scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) + ggtitle("Bgt-712, GAPDH") +
  ylab("Number of isolates") + theme(text = element_text(size=20)) +
  xlab("Number of copies")
dev.off()

  databgt940<-read.table("../av_covA_new_Bgt-940",h=F)
  png("Bgt-940.png")
  ggplot(databgt940) + geom_histogram(aes(x=V4)) + scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) + ggtitle("Bgt-940, a-Tubulin") +
  ylab("Number of isolates") + theme(text = element_text(size=20)) +
    xlab("Number of copies")
  dev.off()


  databgt1486<-read.table("../av_covA_new_Bgt-1486",h=F)
  png("Bgt-1486.png")
  ggplot(databgt1486) + geom_histogram(aes(x=V4)) + scale_x_continuous(limits=c(0,2), breaks=c(0,1,2)) + ggtitle("Bgt-1486, Actin") + 
  ylab("Number of isolates") + theme(text = element_text(size=20)) +
    xlab("Number of copies")
  dev.off()

dataAvrPm17<-read.table("../av_covA_new_Bgt-51729_Bgt-51731", h=F)
dataAvrPm17


dataAvrPm17new<- dataAvrPm17 %>% group_by(V1) %>% summarise(Both=sum(V3))
dataAvrPm17new
png("dataAvrPm17.png")
ggplot(dataAvrPm17new) + geom_histogram(aes(x=Both)) + scale_x_continuous(limits=c(0,7), breaks=c(0,1,2,3,4,5,6,7)) + ggtitle("AvrPm17 (Bgt-51729+Bgt-51731)") + 
  ylab("Number of isolates") + theme(text = element_text(size=20)) +
  xlab("Number of copies")
  dev.off()




