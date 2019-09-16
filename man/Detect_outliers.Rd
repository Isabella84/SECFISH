\name{Detect_outliers}
\alias{Detect_outliers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detecting outliers influencing the fitting of a GLM
}
\description{
Function to interactively detect outliers on the fGLM fitting of a specific fleet segment.
}
\usage{
Detect_outliers(COSTS,Fleet_segment,formula)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{COSTS}{
Data frame built applying EA function on individual vessel data and automatically saved as COSTS.csv in the working directory.
} \item{Fleet_segment}{
Fleet segment to be investigated.
}
\item{formula}{
GLM formula for which the outlier is detected.
}
}
\value{
\item{COSTS}{
See format in the package (type CO in the console).
} \item{Fleet_segment}{
A string defining the fleet segment.This string should be the same reported in the COSTS data frame.
}
\item{formula}{
Example:
fuelcost~factor(Met_LOA)+Effort+0
}
}\author{
Isabella Bitetto
}
\examples{
Detect_outliers(COSTS,"DTS_VL1218",fuelcost~factor(Met_LOA)+Effort+0)
}
