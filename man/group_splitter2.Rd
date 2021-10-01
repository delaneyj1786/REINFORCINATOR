\name{group_splitter_2}
\alias{group_splitter_2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Splits a data frame by two variables
}
\description{
A helper function converting a behaviorstream dataframe into a list split by two grouping factors (e.g., Video ID AND Target ID). The resulting dataset is to be used by the recounter function
}
\usage{

# Filtering out empty dataframes
group_splitter_2(data, behaviorstream, "behavior", "consequence",
group1 = "group1", group2 = "group2", actor = "actor",
filt = TRUE)

# Leave all data frames
group_splitter_2(data, behaviorstream, "behavior", "consequence",
group1 = "group1", group2 = "group2", actor = "actor")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{A dataframe or tibble}
    \item{behaviorstream}{A column representing the observation stream. Contains actor behaviors and environmental (or partner) consequences}
    \item{behavior}{A character string indicating the target behavior to be reinforced}
    \item{consequence}{A character string indicating the environmental consequence acting as a reinforcer}
    \item{group1}{A column representing the first variable used to group observation}
    \item{group2}{A column representing the second variable used to group observations}
    \item{actor}{An optional argument indicating the column containing the actor ID}
    \item{missing_data}{An optional argument specifying if missing observations have been removed}
    \item{contingency}{An optional argument indicating if reinforcers must be contingent or contiguous to target behaviors}
    \item{filt}{An optional argument used to filter empty dataframes}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a list of dataframes grouped by GROUP1 and GROUP2
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
# Filtering to non-empty dataframes for piping to group_split_recounter
group_splitter2(two_person_picture,behaviorstream = "BEH", behavior = "o", consequence = "A", group1 = "VIDELT", group2 = "TAR", filt = TRUE)

# Not filtering
group_splitter2(two_person_picture,behaviorstream = "BEH", behavior = "o", consequence = "A", group1 = "VIDELT", group2 = "TAR")

## The function is currently defined as
function(data, behaviorstream, behavior,consequence,
                    group1,group2,
                    actor = NULL, missing_data = NULL,
                    contingency = NULL, filt = NULL){
  # this just splits the data frame
  group1 <- as.factor(group1)
  group2 <- as.factor(group2)
  split_dat_2 <-split(data, f = (c(data[,group1],data[,group2])))
if(is.null(filt) == FALSE){
  #double_list<-double_split_test[sapply(double_split_test, function(x) dim(x)[1]) > 0]
  split_dat_2[sapply(split_dat_2, function(x) dim(x)[1]) > 0]
}else{
  split_dat_2
}

}
