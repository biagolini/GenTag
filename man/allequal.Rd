% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combination_generators.R
\name{allequal}
\alias{allequal}
\title{All equal tag sequence sample}
\usage{
allequal(ntag, colorsname, nspecial = 0, name1 = "Metal",
  name2 = "EMPTY", location1 = 1, location2 = 2, nspecial1 = 1,
  nspecial2 = 1)
}
\arguments{
\item{ntag}{Number of tag to be used in each animal.}

\item{colorsname}{Names/Code of color tags to be sample.}

\item{nspecial}{Number of special tags/codes, such as metallic, 'EMPTY', or flag (min 0, max 2).}

\item{name1}{Name of special tag 1.}

\item{name2}{Name of special tag 2.}

\item{location1}{Position (or group of positions) to special band 1.}

\item{location2}{Position (or group of positions) to special band 2.}

\item{nspecial1}{Number of special tag 1 that will be present in all sequences genetated.}

\item{nspecial2}{Number of special tag 2 that will be present in all sequences genetated.}
}
\value{
A sequencie of tags
}
\description{
Create combinations with equal sample probability to all colors.
}
\examples{
# Create an object contain the name/code of tag colors
tcol<-c('Black','Blue','Brown','Gray','Green','Pink','Purple','Red','White','Yellow')
# Generate color tag combination without especial tags
genseq(30, 4, colorsname= tcol)
# Generate color tag combination with especial color (ex metallic tag for numeric identification)
genseq(30, 4, tcol, nspecial=1, name1='Metal',location1=c(2,4))
# For ongoing works, use the argument usedcombinations to informe the previus used combinations
data(pre_used) # Data example
genseq(100, 4, tcol, usedcombinations=pre_used[,1:4])
}
