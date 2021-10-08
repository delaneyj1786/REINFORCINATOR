\name{partner_recoder}
\alias{partner_recoder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Partner Recoder
%%  ~~function to do ... ~~
}
\description{
The partner_recoder creates a new variable (newcol) which places a consequence_partner code for observations where a desired consequence comes from a desired partner.
}
\usage{
partner_recoder(data,behaviorstream, type, consequence, partner)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A dataframe or tibble}
  \item{behaviorstream}{A column (character vector) representing the observation stream. Contains actor behaviors and partner consequences}
  \item{type}{A column (character vector) representing the social partners}
  \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
  \item{partner}{A character string indicating the code for desired partner (from the type column)}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a tibble where \bold{newcol} holds the code corresponding to the consequence as delivered by a particular partner (Specified in args).
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
