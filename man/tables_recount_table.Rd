\name{tables_recount_table}
\alias{tables_recount_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table Recount
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tables_recount_table(recounted_data_frame)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Produces two lists. \bold{process_list} and \bold{output_list}. \bold{process_list}

 \item{sub_series_with_margins}{A list containing a contingency table for each sub-series and the sum across both}
 \item{sum_sub_series_with_margins}{A table containing the sum across all sub series tables}
  \item{sub_series_without_margins}{A list containing a contingency table for each sub-series and the sum across both (margins excluded)}
 \item{sum_sub_series_with_margins}{A table containing the sum across all sub series tables (margins excluded)}
   \item{sub_series_prop_tabs_without_margins}{A list containing a proportion (row wise by row margins) for each sub-series (margins excluded)}
 \item{average_tab_with_margins}{A table containing the average cell frequency across sub-series}
 \item{row_average_totals}{A vector of the row margins from average table (used for recomputing frequencies)}
 \item{recompute_margins}{The product of the prop_tabs_without_margins and row_average_totals}
 \item{average_tab_no_margins}{A table containing the average cell frequency across sub-series (With margins omitted)}
 \item{recomputed_sub_series_cell_wise}{?}
 \item{recompute_margins_sum}{?}
 \item{recompute_margins_table}{?}
 \item{recompute_margins_table_avg}{?}



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
