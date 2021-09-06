\name{group_splitter}
\alias{group_splitter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
group_splitter
}
\description{
A helper function converting a behaviorstream dataframe into a list split by a grouping factor (e.g., Video ID or Target ID). The resulting dataset is to be used by the recounter function
}
\usage{
group_splitter(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{A dataframe or tibble}
    \item{behaviorstream}{A column representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
    \item{behavior}{A character string indicating the target behavior to be reinforced}
    \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
    \item{group}{A column representing the variable used to group observation}
    \item{actor}{An optional argument indicating the column containing the actor ID}
    \item{missing_data}{An optional argument specifying if missing observations have been removed}
    \item{contingency}{An optional argument indicating if reinforcers must be contingent or contiguous to target behaviors}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
James DeLaney
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
