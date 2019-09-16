\name{GLM}
\alias{GLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generalized Linear Modelling on variable costs of individual vessel data
}
\description{
Function to explore GLMs on the individual vessel data to test the significance of metier and transversal variables on the variable costs structure.

The function should be run on COSTS.csv file produced by EA function run previously on individual vessel data. This file should be correctly stored in the working directory.
Only the fleet segments with a minimum number of observations are considered. This value can be set by the user as input in the GLM function, defining thr_obs value.
It is also possible to define the most significant metier to be used in the GLM through the percentage of cumulative estimated on the number of vessels observed by metier. A buffer of 5 percent on the cumulative percentage is applied by default.
}
\usage{
GLM(COSTS,thr_obs,thr_cum,FORMULA_LAB1,FORMULA_LAB2,path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{COSTS}{
Data frame built applying EA function on individual vessel data and automatically saved as COSTS.csv in the working directory.
} \item{thr_obs}{
threshold to be used for carry out the modelling to a fleet segment.
} \item{thr_cum}{
threshold to be used for selecting the metier to be used in the modelling according to a cumulative metier-number of vessels observed.
} \item{FORMULA_LAB1}{
Option for labour costs modelling (additive model).
}\item{FORMULA_LAB2}{
Option for labour costs modelling (multiplicative model).
}\item{path}{
Path where the results have to be saved.
}
}
\value{
\item{COSTS}{
data.frame: see format in the package (type CO in the console).
} \item{thr_obs}{
minimum number of observations needed to run fit the GLM; e.g. 30 means that only the fleet segments with at least 30 vessels observed will be modelled.
} \item{thr_cum}{
a decimal number representing a proportion for the selection of the more significant metier: 0.8 means that only the metier representing the 80 percent of the vessels observed in the fleet segment will be considered, to avoid spurious relathionships.
} \item{FORMULA_LAB1}{
Options:
crewcost~factor(Met_LOA)+Rev_minus_Tot_var_costs+0
crewcost~factor(Met_LOA)+Sum_revenue+0
crewcost~factor(Met_LOA)+Effort+0
crewcost~factor(Met_LOA)+Rev_minus_fuel+0
}\item{FORMULA_LAB2}{
Options:
crewcost~factor(Met_LOA)*Rev_minus_Tot_var_costs+0
crewcost~factor(Met_LOA)*Sum_revenue+0
crewcost~factor(Met_LOA)*Effort+0
crewcost~factor(Met_LOA)*Rev_minus_fuel+0
}\item{path}{
Default path=tempdir()
}
}\author{
Isabella Bitetto
}
\examples{
formula1=crewcost~factor(Met_LOA)+Rev_minus_Tot_var_costs+0
formula2=crewcost~factor(Met_LOA)*Rev_minus_Tot_var_costs+0
GLM(COSTS,30,0.95,formula1,formula2,path=tempdir())
}
