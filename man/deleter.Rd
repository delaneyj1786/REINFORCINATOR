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
deleter(data, behaviorstream,code1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A dataframe or tibble}
  \item{behaviorstream}{A character vector representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
  \item{code 1}{A character indicating the code to be deleted}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a tibble where indices in \bold{behaviorstream} corresponding to code 1 are now removed from the behaviorstream. The behaviorstream is
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
> elevator
# A tibble: 14 x 3
   VIDELT TAR   BEH
    <dbl> <chr> <chr>
 1      1 S     o
 2      1 S     x
 3      1 S     x
 4      1 S     x
 5      1 S     A
 6      1 S     o
 7      1 S     x
 8      2 S     x
 9      2 S     x
10      2 S     A
11      2 S     o
12      2 S     o
13      2 S     o
14      2 S     x
> deleter(elevator,BEH,"o")
  VIDELT TAR BEH
1      1   S   x
2      1   S   x
3      1   S   x
4      1   S   A
5      1   S   x
6      2   S   x
7      2   S   x
8      2   S   A
9      2   S   x
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
