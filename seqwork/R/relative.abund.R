relative.abund <-
function (x,y, rank="Phylum", cutoff=50, palette="A") {
  
  rownames(y)<-colnames(x)
  
  m<-ncol(x)
  n<-nrow(x)
  
  common.asv.ids<-intersect(colnames(x), rownames(y))
  x<-x[,common.asv.ids]
  y<-y[common.asv.ids,]
  all.equal(rownames(y), colnames(x))
  

  x<-decostand(x, method="total")
  

  ps <- phyloseq(otu_table(x, taxa_are_rows=FALSE),  tax_table(as.matrix(y)))
  

  top.X<-names(sort(taxa_sums(ps), decreasing=TRUE))[1:cutoff]
  
  ps.topX <- prune_taxa(top.X, ps)
  
  p<-plot_bar(ps.topX,  fill = rank) + labs(x = "Sample ID", y = "Relative abundance")
  p<-p + scale_fill_viridis_d(option = palette)
  p + theme(text = element_text(size = 15))
  
  return(p)
  
}
