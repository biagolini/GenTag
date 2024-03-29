# GenTag
**Author:**  Carlos Biagolini-Jr.

**License:** GPL (>= 2)

<!-- badges: start -->
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/GenTag)](https://cran.r-project.org/package=GenTag)
[![Downloads](https://cranlogs.r-pkg.org/badges/GenTag)](https://CRAN.R-project.org/package=GenTag)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/GenTag?color=orange)](https://CRAN.R-project.org/package=GenTag)
[![DOI](https://zenodo.org/badge/194592157.svg)](https://zenodo.org/badge/latestdoi/194592157)


[![Travis build status](https://travis-ci.org/biagolini/GenTag.svg?branch=master)](https://travis-ci.org/biagolini/GenTag)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/biagolini/GenTag?branch=master&svg=true)](https://ci.appveyor.com/project/biagolini/GenTag)
[![Codecov test coverage](https://codecov.io/gh/biagolini/GenTag/branch/master/graph/badge.svg)](https://codecov.io/gh/biagolini/GenTag?branch=master)
<!-- badges: end -->



> A computational tool to create sequences of animal color tags

## Introducing to the problem
This is a package which proposes an improved protocol for animal color tagging. Since the early 1980s, several studies have suggested a negative influence of color tags on animal social behavior. However, methodological approaches to deal with this issue are lacking.  The GenTag package was designed to produce sequences of color tag combinations. The package run on R base, without cross-package dependence. The computational routine is simple, implying low CPU usage. Despite the simplicity of the routine itself, the application of this method is exceptionally broad, as it can be applied in any project that uses visual identification of animals based on color marking, across a wide range of taxa, including birds, mammals, and fishes. 

## How GenTag deal with this issue
First, a single-color tag sequence is created from an algorithm selected by the user, followed by verification of the combination uniqueness. Three methods to produce color tag sequences are provided. Users can modify the main function core to allow a wide range of applications. 

## Algorithm to create color sequences 
Three algorithms are provide: 
 All equal: creates combinations of tags in which all colors have the same probability of being sampled. 
 Variable frequency: creates combinations of tags using different probabilities for each color, where the probabilities are defined by the user. 
 Life expectancy: this algorithm creates a restriction based upon color combinations, so that all colors will be represented in similar frequencies in the natural population under study. 

## Installing the release version of the `GenTag` R package
You can install `GenTag` from CRAN with:
``` r
install.packages("GenTag")
```

### Installing the development version of the `GenTag` R package
You can install the development version from GitHub with:
``` r
devtools::install_github("biagolini/GenTag")
```

## Usage
Download GenTag and load into R
``` r
install.packages("GenTag")
library("GenTag")
```
Examples below illustrate how to apply the three methods to generate color tag sequences. I exemplify how to generate the list of color tag combinations for both new and ongoing studies. All methods request the definition of the number of sequences desired, number of tags for each individual, and tag colors.
``` r
# Create an object to hold tag colors name/code
tcol<-c("Black","Blue","Brown","Gray","Green","Pink","Purple","Red","White","Yellow") 
# Generate sequences by All equal algorithm 
genseq(ncombinations=30, ntag=4,colorsname=tcol)
```
For the Variable frequency method it is also necessary to input the proportions of each color available.
``` r
# Create an object to hold the ratio for sampling 
p<-c(1,2,5,1,2,2,4,5,8,5) 
# Generate sequences by Variable frequency algorithm 
genseq(ncombinations=30, ntag=4, colorsname=tcol, gen_method="vfrequency", colorsf=p)
```
To take into account previously used sequences, set the argument usedcombinations to a dataframe or matrix that contains the list of used sequences. In our examples I used a simulated data of pre used combinations provided within the GenTag package.

``` r
# Load data example
data(pre_used)
# Generate sequences taking into account pre-used sequences
genseq(ncombinations=30, ntag=4, colorsname= tcol, usedcombinations=pre_used[,1:4])
```
For a quick adjustment in color representation, I recommend the use of the Life expectancy method. To improve accuracy, set the year survival (yearsurvival argument) and expected lifespan (lifespan argument). If these parameters are undefined, it will be assumed that animals never die, and that the proportion that occurs in the natural population equals the total number of tags used.
``` r
# Generate sequences by Life expectancy algorithm 
genseq(ncombinations=30, ntag=4, gen_method="lifexp", colorsname= tcol, usedcombinations=pre_used[,1:4],  yearusedcombinations=pre_used[,5], yearsurvival= 0.8, lifespan=5, currentyear=2019)
```
To create sequences that contain special code, such as metal for numbered tagging; set the argument nspecial to the number of special codes, and the argument name1 and location1 to inform the tag codes and where each special tag can be placed. 
``` r
# Generate sequences with Metal code for numbered tagging 
genseq(ncombinations=30, ntag=4, colorsname= tcol, nspecial=1, name1="Metal", location1=c(2,4))
```
To export data, users can store the sequences in an R object, then export this object as a .csv file.
``` r
# Generate sequences 
combinations <-genseq(100, 4, tcol)
# To export csv file
write.csv(combinations, file="Color_sequences.csv", row.names=F)
```
Two additional functions facilitate awareness of previously used color patterns. First, scy provides a historical log of used tag colors. To estimate the amount of remaining colors in the natural study population, the erc function can be used.
``` r
# Summary the number of color tags used by year
scy(usedcombinations=pre_used[,1:4],yearusedcombinations=pre_used[,5], hide_color="EMPTY")
# Estimates remaining color tags in the field
erc(usedcombinations=pre_used[,1:4],yearusedcombinations=pre_used[,5],yearsurvival=0.8, hide_color="EMPTY")
```
## Author information
Carlos Biagolini-Jr.  
email: <c.biagolini@gmail.com>  
CV: <http://lattes.cnpq.br/4086237188108947>  

## Contributing
 fork <https://github.com/biagolini/GenTag/fork>
 
##  Citation information
An article of GenTag appliance to field data is under publication process. 
Check out CRAN documentation at https://cran.r-project.org/package=GenTag
