trans.to.prop <-
function(x, method="CA") {

  m<-length(colnames(x))
  n<-length(row.names(x))
  
  sum.row<-vector(length = m)
  
  sum.row<-rowSums(x)
  
 
  new.tab<-matrix(nrow = n, ncol = m)
  colnames(new.tab)<-colnames(x)
  row.names(new.tab)<-row.names(x)
  
  for (i in 1:n) {
    sum.row[i]<-(1/sum.row[i])
  }
  
  sum.row<-as.matrix(sum.row)
  x<-as.matrix(x)
  sum.row.mat<-matrix(nrow = n, ncol = m)
  

  for (i in 1:m) {
    sum.row.mat[,1:m]<-sum.row
  }
  
  new.tab<-x*sum.row.mat
  
  check<-rowSums(new.tab)
  
  if (method=="CA") {
    z<-cca(new.tab)
  } else if (method=="NMDS") {
    z<-metaMDS(new.tab)
  }
  
  output<-list(prop.table=new.tab, ordination=z)
  return(output)
}
