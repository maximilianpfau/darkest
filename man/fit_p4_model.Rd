% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_p4_model.R
\name{fit_p4_model}
\alias{fit_p4_model}
\title{Fit a four parameter model}
\usage{
fit_p4_model(x, start_vals)
}
\arguments{
\item{x}{Dark adaptation with columns Time and Threshold}

\item{start_vals}{Start values for the parameters
ct Cone threshold at the cone plateau \link{LogUnits},
tf Final rod threshold \link{LogUnits},
S2 Rate of rod decay \link{decades/min},
Time of cone-rod break ('Kohlrausch kink') \link{min}}
}
\value{

}
\description{
Fit a four parameter model
}
\examples{
fit_p4_model(x, start_vals)
}
