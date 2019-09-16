\name{Cons_check}
\alias{Cons_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Validation of the disaggregation applied on variable costs
}
\description{
Function to validate the results obtained using the Disaggr function. The original costs by fleet segment are compared with the sum of the dsaggregated costs by metier within the same fleet segment.
}
\usage{
Cons_check(Costs_FS,Costs_MET,path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Costs_FS}{
Data frame containing the variable costs time series by fleet segment.
} \item{Costs_MET}{
Data frame containing the variable costs time series by metier within the same fleet segment.
}\item{path}{
Path where the results have to be saved.
}
}
\value{
\item{Costs_FS}{
See example typing Co_FS in the R console.
} \item{Costs_MET}{
See example typing Co_MET in the R console.
}\item{path}{
Default path=tempdir()
}
}\author{
Isabella Bitetto
}
\examples{
Cons_check(Costs_FS,Costs_MET,path=tempdir())
}
