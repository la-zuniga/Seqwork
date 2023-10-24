\name{make.key}
\alias{make.key}
\title{
Creates a custom table that functions as a "key" for ASV count data along with a taxanomic table with sequences
}
\description{
This function creates an ASV-to-sequence key for your conveinece when working with sequence count tables from next-generation sequencing data. By taking information from an input count table and taxanomic table (outputs of the dada2 pipeline), a table that tells you "ASV X" is sequence "ATCG..." is produced. This table also will tell you the taxanomic classification of each ASV/sequence. The last column of this table tells one whether the sequences from columns 2 and 10 are the same. This is important, so we do no accidentally assign incorrect taxanomy to a sequence. 
}
\usage{
make.key(x,y)
}
\arguments{
\item{x}{
An n by m data frame, where n is the number of samples, and m is the number of ASV's/sequences. The data is integer counts of how many times ASV number m  appears in sample number n. 
 }
\item{y}{
An m by 7 data frame which assigns taxanomy to each sequence. The rows here are nucleotide sequences, and the columns give taxanomic identities for each sequence for each of 7 taxanomic ranks. If dada2 was not able to assign an identity at some rank, it has an NA entry. 
 }
}

\value{
Returns an m by 11 table which acts as a sequence-taxanomy key, where the last column checks that the sequences the nth row match. 
}
\author{
L. Zuniga
}
\examples{

data("sequence.table", "taxa")

## calling the function and getting the new key 
make.key(sequence.table, taxa)


}

