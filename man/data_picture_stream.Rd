\name{picture_stream}
\alias{picture_stream}
\docType{data}
\title{
Thesis Picture Data
}
\description{
An example dataset to illustrate how reinforcement detection may appear in a one person observation stream. This is based on the single person example in James DeLaney's masters thesis 'Detecting Reinforcement Patterns in the Stream of Observations'.
}
\usage{data("picture_stream")}
\format{
  A data frame with 25 observations on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{A numeric vector indicating the observational episode}
    \item{\code{TAR}}{A character vector indicating the actor being observed}
    \item{\code{BEH}}{A character vector indicating the behavior is target ('o'), reinforcer ('A') or behavioral non-target ('x')}
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

## Recounted Single Data Frame
recounted_picture<-recounter(picture_stream,BEH,"o","A","TAR")$recounted_data_frame

## Recounted Contingency Table Stats
recounted_picture_tables <- tables_recount_table(recounted_picture)

$output_list$avg_prob
     B_NT       B_T      A_NT       A_T
0.6348039 0.3651961 0.2684524 0.7315476
}
\keyword{datasets}
