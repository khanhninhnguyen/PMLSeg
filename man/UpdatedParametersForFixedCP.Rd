% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UpdatedParametersForFixedCP.R
\name{UpdatedParametersForFixedCP}
\alias{UpdatedParametersForFixedCP}
\title{update the segmentation parameters based on the screening result}
\usage{
UpdatedParametersForFixedCP(
  OneSeries,
  ResScreening,
  FunctPart = TRUE,
  selectionF = FALSE
)
}
\arguments{
\item{OneSeries}{is a time series data frame with 2 columns, $signal and $date, each of size n x 1}

\item{ResScreening}{the output of the Cluster_screening() function}

\item{FunctPart}{a boolean indicating if the functional part should be modelled. Default is TRUE.}

\item{selectionF}{a boolean indicating if the statistically significant parameters of the functional part should be selected. The level of the test is by default 0.001. Default is FALSE.}
}
\value{
\itemize{
\item \code{Tmu} is a data frame containing the segmentation results, with 5 columns and a number of lines equal to the number of segments of the time series.
The columns are: \code{$begin, $end, $tbegin, $tend, $mean, $se, $np}. They represent the date index (integer) of begin and end of each segment, their corresponding dates (tbegin and tend), the estimated mean of the segment (\code{mean}) and its standard error (\code{se}), and the number of "valid" points (\code{np}), i.e. non-NA $signal values in the segment.
\item \code{FitF} is the functional part predicted from the estimated Fourier coefficients, a numeric vector of size n x 1. Note: if \code{FunctPart=FALSE}, \code{FitF} is FALSE.
\item \code{CoeffF} is the vector of coefficients of the Fourier series, a numeric vector of size 1 x 8 if \code{selectionF=FALSE}.
Note: If \code{selectionF=TRUE} the size of \code{CoeffF} correspods to the number of selected coefficients.
If \code{FunctPart=FALSE}, \code{CoeffF} is FALSE.
\item \code{MonthVar} contains the estimated monthly variances, a numeric vector of size 1 x 12.
\item \code{SSR} is the Sum of Squared Residuals of the fit.
}
}
\description{
update the segmentation parameters based on the screening result
}
