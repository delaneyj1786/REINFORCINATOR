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
recounter(data, behaviorstream, "behavior", "consequence",
actor = NULL, missing_data = NULL, contingency = NULL )
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
Returns a two item list of lists, containing \bold{descriptive statistics} and \bold{recounted data frame}. \bold{Descriptive statistics} is a list that contains:
 \item{n_obs}{number of total observations}
 \item{n_tar}{number of target behaviors}
 \item{n_reinf}{number of reinforcers}
 \item{n_actor}{number of unique actor ID}
 \item{n_cont}{number of reinforcers occuring contiguous with target behavior}
 \item{reinf_index}{the observation corresponding to each reinforcer}
 \item{contingency_index}{The observation corresponding to each contiguous reinforcer}
 \item{na_index}{The observations corresponding to missing values}
  \item{prob_tar_uncorrected}{The probability of target behavior across all observations}
    \item{prob_tar_corrected}{The probability of target behavior across all observations}
    The \bold{recounted_data_frame} is a dataframe with n*r reclassified observations containing the following variables:
 \item{recount_stream}{The original observation stream replicated \bold{n_obs}*\bold{n_reinf times}}
 \item{sub_series}{The repetition number (corresponding to a reinforcer)}
 \item{recount_stream_index}{The index in the recounted stream (resets for each sub-series)}
 \item{recount_recode_stream}{The DV for a reinforcer analysis (target and non-target observations are recoded as 1 and 0, respectively)}
 \item{recount_sequence}{A factor variable that contains the classification of observations as Before (B) or After (A) the reinforcer j corresponding to the jth sub-series }
 \item{recount_actor}{The actor variable which has been recounted \bold{n_reinf}*\bold{n_obs} times}
 \item{regression_recount_sequence}{The recounted sequence where all reinforcers have been recoded as NA (to be removed when performing regression analysis)}
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
recounter(elevator, BEH,"o","A")
recounter(picture_stream, BEH,"o","A")

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
