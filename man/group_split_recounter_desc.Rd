\name{group_split_recounter_desc}
\alias{group_split_recounter_desc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group Split Recounter Descriptives
}
\description{
Contains basic descriptive info for each level of a single grouped recounted dataset

}
\usage{
group_split_recounter_desc(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{list}{A split data frame}
  \item{behaviorstream}{A column representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
  \item{behavior}{A character string indicating the target behavior to be reinforced}
  \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
  \item{actor}{An optional argument indicating the column containing the actor ID}
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

# Create a split DF
split_df_1 <- group_splitter(two_person_picture,behaviorstream = BEH,
                             behavior = "o", consequence = "A", group = "TAR")

group_split_recounter_desc(split_df_1,"BEH","o","A","TAR")

## The function is currently defined as
group_split_recounter_desc <- function(list, behaviorstream, behavior,consequence,actor){
  list %>% map(~Recounter5(.x,behaviorstream,behavior,consequence,actor)$descriptive_statistics)
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
