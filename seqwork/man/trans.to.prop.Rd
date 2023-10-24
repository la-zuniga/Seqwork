\name{trans.to.prop}
\alias{trans.to.prop}
\title{
Transforms a table of instances of a variable found in a sample to proportions of a variable in a sample
}
\description{
The input is an n by m table, where there are n samples and m is the number of variables. This function will take a table of counts of variable number m in sample number n, and change it to a proportion of variable number m in sample number n.
}
\usage{
trans.to.prop(x, method)
}
\arguments{
\item{x}{
An n by m data frame. n is the number of samples and m is the number of variables. The data is instances of a varible counted in a sample. 
 }
\item{method}{
An argument to conduct one of two methods of dimensional reduction. Non-metric dimensional scaling by making method="NMDS", or Correspondence analysis by making method="CA". The default is CA. 
} 
}

\value{
Returns An n by m data frame of the new proportional data. Each row will sum to 1. 
}
\author{
L. Zuniga
}
\examples{

data("sequence.table")

## calling the function to output a list 
## item 1 is the new table of proportions, and item 2 is the chosen dimensional reduction outpout

# chosing correspondence analysis
trans.to.prop(sequence.table, method="CA")

}


