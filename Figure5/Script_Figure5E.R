###### Figure 5E ##########
library("qtl")

Pm17cg<-read.cross(dir="","Pm17cg.csv",format="csv",genotypes=c("AA","AB"))

mar1 <- find.marker(Pm17cg, chr=9, pos=340)
mar1
mar2 <- find.marker(Pm17cg, chr=1, index=4422)
mar2
plotPXG(Pm17cg, marker=c(mar1,mar2), pheno.col=2, colour="black",cex.axis=2, ylab=NULL)
