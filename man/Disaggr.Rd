\name{Disaggr}
\alias{Disaggr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Disaggregation of the variable costs
}
\description{
Function to disaggregate time series of variable costs by from the fleetsegment to the metier level, using the official time series of the costs and the official time series of transversal variables, combined with the  results obtained from GLM function applied on individual vessel data.
}
\usage{
Disaggr(Costs_or,key_table_or,Eff,path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Costs_or}{
Data frame containing the variable costs time series by fleet segment.
} \item{key_table_or}{
Data frame containing the coefficients of the best fitting GLM, describing the relationship between variable costs structure and transversal variables by metier.
} \item{Eff}{
Data frame containing the transversal variables (effort and revenues) by metier.
}\item{path}{
Path where the results have to be saved.
}
}
\value{
  \item{Costs_or}{
See example typing Co_or in the R console.
} \item{key_table_or}{
See example typing key_tab_or in the R console.
Details related to each column:
Type of cost: Allowed values: fuel_costs, other_costs, labour_costs, maintenance_costs.
Option: 1 additive model, 2 multiplicative model, 3 metier not significant.
To fill in the fields  Explanatory_variable and Coefficient, the output produced by the GLM.r script has to be considered for that fleet segment for the disaggregation.
} \item{Eff}{
See example typing Eff_ in the R console.
}\item{path}{
Default path=tempdir()
}
}\author{
Isabella Bitetto
}
\examples{
Disaggr(Costs_or,key_table_or,Eff,path=tempdir())
}
