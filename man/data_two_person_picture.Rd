\name{two_person_picture}
\alias{two_person_picture}
\docType{data}
\title{
An example dataset to illustrate how reinforcement detection may appear in a two person observation stream. This is based on the single person example in James DeLaney's masters thesis 'Detecting Reinforcement Patterns in the Stream of Observations'.

This data intended to illustrate the split apply strategy (see group_splitter and group_split_recounter)
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("two_person_picture")}
\format{
  A data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{a numeric vector indicating the observational episode}
    \item{\code{TAR}}{a character vector indicating the actor being observed }
    \item{\code{BEH}}{a character vector indicating the behavior is target ('o'), reinforcer ('A') or behavioral non-target ('x')}
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
data(two_person_picture)
## maybe str(two_person_picture) ; plot(two_person_picture) ...
}
\keyword{datasets}
