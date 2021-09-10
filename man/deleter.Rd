\name{deleter}
\alias{deleter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Deleter
}
\description{
This function deletes all observations of a chosen behavior code from the behavior stream
}
\usage{
deleter(behaviorstream,code1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behaviorstream}{A character vector}
  \item{code 1}{A character indicating the code to be deleted}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns \bold{behaviorstream} where indices corresponding to code 1 are now removed from the behaviorstream
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
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
