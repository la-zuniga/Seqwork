make.key <-
function(x,y) {
  
  n<-nrow(x)
  m<-ncol(x)
  
  seqs<-c(row.names(y))
  
  seq.tab<-x
  colnames(seq.tab)<-seqs
  
  taxa.key<-matrix(nrow = m, ncol = 11)
  
  key.col<-c("ASV", "Sequence", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "Sequence", "Match?")
  colnames(taxa.key)<-key.col
  taxa.key[,1]<-colnames(x)
  taxa.key[,2]<-seqs
  taxa.key[,3]<-y$Kingdom
  taxa.key[,4]<-y$Phylum
  taxa.key[,5]<-y$Class
  taxa.key[,6]<-y$Order
  taxa.key[,7]<-y$Family
  taxa.key[,8]<-y$Genus
  taxa.key[,9]<-y$Species
  taxa.key[,10]<-seqs
  
  taxa.key<-as.data.frame(taxa.key)
  
  for (i in 1:m) {
    if(taxa.key[i,2]==taxa.key[i,10]) {taxa.key[i,11]<-"YES"} 
    else {taxa.key[i,11]<-"NO"}
  }
  
  return(taxa.key)
}
