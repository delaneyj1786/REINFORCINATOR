\name{group_split_recounter}
\alias{group_split_recounter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
group_split_recounter
}
\description{
Applies the recounter to a dataset split by a grouping variable (see group_splitter). The purpose is to recount observations for each level of a grouping factor

}
\usage{
group_split_recounter(data,"behaviorstream","behavior","consequence","actor" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{A dataframe or tibble}
  \item{behaviorstream}{A column representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
  \item{behavior}{A character string indicating the target behavior to be reinforced}
  \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
  \item{group}{A column representing the variable used to group observation}
  \item{actor}{An optional argument indicating the column containing the actor ID}

%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~tt
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
%%  ~~who you are~~
}
\note{
Function sources Recounter5 (a non-tidyverse friendly legacy version of the recounter)
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
test<-group_splitter(two_person_picture,BEH, "x","A",group = "VIDELT", actor = "TAR")
group_split_recounter(test,behaviorstream = "BEH","x","A",actor = "TAR")
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
