\name{reinforcement}
\alias{reinforcement}
\docType{data}
\title{
Reinforcement Data
}
\description{
An example dataset to illustrate how reinforcement may appear in a one person observation stream.
}
\usage{data("reinforcement")}
\format{
  A data frame with 11 observations (10 behaviors) on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{a numeric vector indicating the observational episode}
    \item{\code{TAR}}{a character vector indicating the actor being observed}
    \item{\code{BEH}}{a character vector indicating the behavior is target ('o'), reinforcer ('A') or behavioral non-target ('x')}
}
}
