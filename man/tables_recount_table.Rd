\name{tables_recount_table}
\alias{tables_recount_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table Recount
}
\description{
Provides frequency based statistics and probabilities for recounted_data_frame
}
\usage{
tables_recount_table(recounted_data_frame)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{recounted_data_frame}{An object from the recounter function}
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
 \bold{Output List}
 \item{avg_prob}{average probability across all sub-series (check what table)}
 \item{avg_diff}{average difference between before and after (check)}
 \item{sum_recount_table}{The sum of the recounted frequencies across sub-series}
 \item{average_table}{The average value across the sub-series (check)}
 \item{recompute frequencies}{check}
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
# Obtain recounted data frame
# recounted data frame
recounted_elevator_recounted_df<-recounter(elevator, BEH,"o","A")$recounted_data_frame

For process list:
tables_recount_table(elevator_recounted)$process_list

For output list:
tables_recount_table(elevator_recounted)$output_list
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
