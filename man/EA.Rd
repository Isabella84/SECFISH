\name{EA}
\alias{EA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exploratory Analysis on variable costs of individual vessel data
}
\description{
Function to carry out an exploratory analysis on the individual vessel data to derive the correlations between variable costs and transversal variables.

The user has to define the percentage of fishing activity (in hours) to associate a prevalent metier to each vessel (namely by setting the “thr” value) and the minimum number of observations to be required for each metier, within a fleet segment, for carrying out the simple linear correlations analysis.
For the metier for which less than “n_obs” observations are available, the simple linear regression is not fitted and the results are not produced.

Moreover, the user can decide to estimate the relationships with 2 options for the activity (Effort):
Option 1: hours at sea;
Option 2: Days_at_sea x KW.
The days at sea are estimated as the sum of the fishing hours divided by 24.

}
\usage{

EA(Effort,Landings,Trip,OperID,Operations,Costs_vess, Capacity,thr,n_obs,Eff_option,path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Effort}{
association trip-total hours at sea
} \item{Landings}{
association trip-landing and related revenue
} \item{Trip}{
association trip-vessel
} \item{OperID}{
association operation-trip
}\item{Operations}{
association fishing operation-number of fishing hours-metier
}\item{Costs_vess}{
data on fuel costs, fuel consumption, maintenance costs and other variable costs
}\item{Capacity}{
information vessel characteristics (KW, GT, LoA, etc…)
} \item{thr}{
percentage to fishing activity (hours) associated to the prevalent metier. If 50 is set, only the vessels with a percentage of fishing hours in a specific metier greater or equal to 50 will be retained (this metier will be defined as prevalent metier).
} \item{n_obs}{
minimum number of observations for each metier used by the fleet segment for carrying out the simple linear correlations.
} \item{Eff_option}{
1: Hours at sea
2: Days at sea X KW
}\item{path}{
Path where the results have to be saved.
}
}
\value{
   \item{Effort}{
data.frame: see format in the package (type Eff in the console).
} \item{Landings}{
data.frame: see format in the package (type Land in the console)
} \item{Trip}{
data.frame: see format in  the package (type Tr in the console)
} \item{OperID}{
data.frame: see format in  the package (type OpID in the console)
}\item{Operations}{
data.frame: see format in  the package (type Oper in the console)
}\item{Costs_vess}{
data.frame: see format in  the package (type Cos in the console)
}\item{Capacity}{
data.frame: see format in the package (type Cap in the console)
} \item{thr}{
a value (even decimal) from 1 to 100 representing a percentage for the definition of the prevalent metier for each vessel.
} \item{n_obs}{
number of minimum observations to obtain reliable correlations. A number from 2 to the maximum number of observations by metier in the dataset.
} \item{Eff_option}{
1 or 2.
}\item{path}{
Default path=tempdir()
}
}\author{
Isabella Bitetto
}
\examples{

EA(Effort,Landings,Trip,OperID,Operation,Costs_vess, Capacity,30,30,1,path=tempdir())

}
