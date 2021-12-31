\name{combiner}
\alias{combiner}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combiner
}
\description{
This function creates a new code using observations of two chosen behavior codes from the behavior stream

}
\usage{
combiner(data, behaviorstream,code1,code2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A dataframe or a tibble}
  \item{behaviorstream}{A character vector representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
  \item{code 1}{A character indicating the first code to be combined. The new code uses code 1.}
  \item{code 2}{A character indiciating the second code to be combined}
%%     ~~Describe \code{x} here~~
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a tibble where indices in \bold{behaviorstream} corresponding to code 1 and code 2 are now replaced with code1
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
