\name{recounter}
\alias{recounter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The recounting function
}
\description{
The heart of the reinforcinator. This function replicates the rows of the dataframe according to the number of reinforcers in the behaviorstream variable. The function produces the 'recounted dataframe' which contains two new columns, 'Sub-Series', which indicates which reinforcer the observations are recounted by, and 'Sequence' which indicates whether the behavior occurs before or after the given reinforcer corresponding to that sub-series.

If there are 3 reinforcers, then there will be 3 sub-series based on reinforcers with a unique index in the original series. All observations will be classified as occuring before or after the index position of a given reinforcer corresponding to a given sub-series.
}
\usage{
recounter(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{data}{A dataframe or tibble}
    \item{behaviorstream}{A column representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
    \item{behavior}{A character string indicating the target behavior to be reinforced}
        \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
    \item{actor}{An optional argument indicating the column containing the actor ID}
    \item{missing_data}{An optional argument specifying if missing observations have been removed}
    \item{contingency}{An optional argument indicating if reinforcers must be contingent or contiguous to target behaviors}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list containing
 \item{descriptive_statistics}{A list containing the following descriptive statistics}
 \item{descriptive_statistics_n_obs} {(number of total observations)}
 \item{descriptive_statistics_n_tar} {number of target behaviors}
 \item{descriptive_statistics_n_reinf} {number of reinforcers}
 \item{descriptive_statistics_n_actor} {number of unique actor ID}
 \item{descriptive_statistics_n_cont} {number of reinforcers occuring contiguous with target behavior}
 \item{descriptive_statistics_reinf_index} {the observation corresponding to each reinforcer}
 \item{descriptive_statistics_contingency_index} {The observation corresponding to each contiguous reinforcer}
 \item{descriptive_statistics_na_index} {The observations corresponding to missing values}
  \item{descriptive_statistics_prob_tar_uncorrected} {The probability of target behavior across all observations}
    \item{descriptive_statistics_prob_tar_prob_tar_corrected} {The probability of target behavior across all observations}
 \item{recounted_data_frame}{The dataframe with n*r reclassified observations. Contains the following variables}
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
