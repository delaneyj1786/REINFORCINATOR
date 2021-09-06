\name{elevator}
\alias{elevator}
\docType{data}
\title{
elevator data
}
\description{
An example dataset to illustrate how the arrival of an elevator may affect subsequent operation of this elevator.
}
\usage{data("elevator")}
\format{
  A data frame with 14 observations on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{a numeric vector indicating the segment or episode of observations}
    \item{\code{TAR}}{a character vector indicating the ID of the person using the elevator}
    \item{\code{BEH}}{a character vector indicating whether the elevator was called (o), non-arrival (x), or arrival (A). }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(elevator)
## maybe str(elevator) ; plot(elevator) ...
}
\keyword{datasets}
