\name{relative.abund}
\alias{relative.abund}
\title{
Plots the relative abundances of taxa present in your samples. 
}
\description{
This function will plot relative abundances of taxa present in your samples with a variety of options. This includes choosing which taxanomic rank to plot, choosing the top X most abundant taxa to plot, and choosing between the different viridis color-blind friendly color palettes. 
}
\usage{
relative.abund(x, y, rank="Phylum", cutoff=50, palette="A")
}
\arguments{
  \item{x}{
An n by m table of ASV counts in n samples. The rows are sample names and the columns are ASVs.
 }
 \item{y}{
An m by 7 table of m sequences with taxanomic identity assigned to each sequence. 
 }
 \item{rank}{
 This argument allows you to pick which taxanomic rank to look at in your plot. rank="Phylum"" is the default, but you can shoose any of 6 other ranks: "Kingdom", "Class", "Order", "Family", "Genus", "Species". 
 }
 \item{cutoff}{
 This argument allows you to "cutoff" the taxa plotted by choosing the top X most abundant. Top 50 taxa is the default. 
 }
 \item{palette}{
 This argument allows you to choose between the viridis package's color-blind friendly palettes. palette="A" is the default option, but you can make palette="A-I" for other options. palette="viridis" is also an option.
}
}
\value{
Returns an object that can be called to vizualize the plot. 
}
\author{
L. Zuniga
}
\examples{
data("sequence.table", "taxa")

## calling the function to output an example plot
## with defaults

relative.abund(sequence.table, taxa)

## choosing a different rank, cutoff, and palette

relative.abund(sequence.table, taxa, rank="Class", palette="F")
}