\name{Constrained_regression}
\alias{Constrained_regression}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to carry out a constrained regression for a specific fleet segment
}
\description{
Function to carry out a constrained regression for a specific fleet segment and a specific type of costs, after setting the metier used by the fleet segment.
}
\usage{
Constrained_regression(COSTS, Fleet_segment, metier,type,path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{COSTS}{
Data frame built applying EA function on individual vessel data and automatically saved as COSTS.csv in the working directory.
} \item{Fleet_segment}{
Fleet segment to be investigated.
}\item{metier}{
Vector of the metier usedd by the specified fleet segment.
}\item{type}{
Type of variable costs on which carry out the constrained regression.
}
\item{path}{
Path where the results have to be saved.
}
}
\value{
  \item{COSTS}{
data.frame: see format in the package (type CO in the console).
} \item{Fleet_segment}{
String defining the fleet segment as reported in the COSTS data frame.
}\item{metier}{
String defining the metier as reported in the COSTS data frame.
}\item{type}{
String defining the type of variable costs as reported in the COSTS data frame.
}\item{path}{
Default path=tempdir()
}
}\author{
Isabella Bitetto
}
\examples{
library(optimization)
path=tempdir()
Constrained_regression(COSTS, "DTS_VL1218", c("OTB_DES_>=40_0_0",
"OTB_MDD_>=40_0_0"),"fuelcost",path)
}
