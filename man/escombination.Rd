% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synonym_functions.R
\name{escombination}
\alias{escombination}
\title{Empty synonym combination}
\usage{
escombination(row_under_review, emptyname, g1 = NA, g2 = NA, g3 = NA,
  g4 = NA, g5 = NA, g6 = NA)
}
\arguments{
\item{row_under_review}{Color sequence to be analyzed.}

\item{emptyname}{Code used to define empty code.}

\item{g1}{Group of tags from group 1. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.}

\item{g2}{Group of tags from group 2. For example, in a code of 6 colors for bird tag, tags from 1 to 3 belongs to the left leg, and  tags from 1 to 3 belongs to the right. g1 must to address the left leg, and g2 must to address right leg. Thus: 'g1=1:3' and 'g2=4:6'.}

\item{g3}{Group of tags from group 3.}

\item{g4}{Group of tags from group 4.}

\item{g5}{Group of tags from group 5.}

\item{g6}{Group of tags from group 6.}
}
\value{
Matrix with all synonyms
}
\description{
Find synonyms of a sequence with code for empty tag.
}
\examples{
# Example of a full sequence
combination<- c('EMPTY','Red','Yellow','Red','Blue','Green')  # See synonym for the full sequence
escombination(combination,'EMPTY',g1=1:3,g2=4:6)
}
