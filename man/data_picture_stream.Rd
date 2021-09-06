\name{picture_stream}
\alias{picture_stream}
\docType{data}
\title{
picture
}
\description{
An example dataset to illustrate how reinforcement detection may appear in a one person observation stream. This is based on the single person example in James DeLaney's masters thesis 'Detecting Reinforcement Patterns in the Stream of Observations'.
}
\usage{data("picture_stream")}
\format{
  A data frame with 25 observations on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{a numeric vector indicating the observational episode}
    \item{\code{TAR}}{a character vector indicating the actor being observed}
    \item{\code{BEH}}{a character vector indicating the behavior is target ('o'), reinforcer ('A') or behavioral non-target ('x')}
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
data(picture_stream)
## maybe str(picture_stream) ; plot(picture_stream) ...
}
\keyword{datasets}
