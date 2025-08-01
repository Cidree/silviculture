% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics-lidar.R
\name{lid_lhdi}
\alias{lid_lhdi}
\title{LiDAR-derived Height Diversity Index (LHDI)}
\usage{
lid_lhdi(z, interval = 0.5)
}
\arguments{
\item{z}{coordinate Z (height) of the point}

\item{interval}{height of the intervals to calculate the metric}
}
\value{
numeric
}
\description{
LiDAR metric that calculates the LiDAR Height Diversity Index, which
can be used in \code{lidR} \code{*_metrics} functions
}
\examples{
1 + 1 ## TODO
}
\references{
Listopad, C. M. C. S., Masters, R. E., Drake, J., Weishampel, J.,
& Branquinho, C. (2015). Structural diversity indices based on airborne LiDAR
as ecological indicators for managing highly dynamic landscapes. Ecological
Indicators, 57, 268–279. \doi{10.1016/j.ecolind.2015.04.017}
}
