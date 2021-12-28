\name{plotting_restructure}
\alias{plotting_restructure}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plotting_restructure
}
\description{
Helper function to add graphing / plotting variables to the recounted data frame}
\usage{
plotting_restructure(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
A recounted data frame
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with 4 variables:
\item{sub_series_sum}{The cumulative sum of the target variable across the behavior stream}
\item{sub_series_total}{The total number of events in the behavior stream}
\item{sub_series_cum_run_prob}{Running probability using total events in denominator}
\item{sub_series_run_prob}{Running probability divided by the number of events to a point in the series}

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
test_df_rc <- recounter(two_person_picture,BEH,"o","A")$recounted_data_frame

# To plot the running probability for each sub-series.
# This shows the shift in the distribution across the sequence.

#ggplot2::ggplot(test_df_rc,ggplot2::aes(x = recount_stream_index, y = sub_series_run_prob, color = #
#(recount_sequence), group = sub_series)) + ggplot2::geom_point() +
#ggplot2::facet_grid(~sub_series) +
#  ggplot2::ggtitle("Running Sequence Probabilities By Sub-Series")+
#  ggplot2::xlab("Observation Sequence") +
#  ggplot2::ylab("Running Probability")


# To show the average sequence probabilities for each sub-series

## Summarize the means per sub-series

#sum_one<- test_df_rc %>% group_by(sub_series, recount_sequence) %>%
#  summarize(sub_series_mean = mean(sub_series_run_prob)) %>% ungroup()

#ggplot(filter(sum_one, recount_sequence != "R"),aes(x = sub_series, y = #sub_series_mean, color = (recount_sequence))) + geom_point() + geom_line() +
#  ggtitle("Average Sub-Series Probabilities By Sequence") +
#  xlab("Sub-Series") +
#  ylab("Average Sequence Probabilities")


## To plot the overall series with the overall Sequence Probabilities #juxtaposed on the series

# Find the overall average by sequence
#overall_average <- sum_one %>% group_by(recount_sequence) %>% summarize
#(mean_sub_mean = mean(sub_series_mean)) %>% ungroup() %>% filter
#(recount_sequence != "R")

# Merge summary with the original dataset so we can add the means to the plot

#average_df<-left_join(test_df_rc, overall_average, by = "recount_sequence")


#ggplot(average_df,aes(x = recount_stream_index, y = sub_series_run_prob, #color = (recount_sequence))) + geom_point() + geom_line(aes(y = #mean_sub_mean)) +
#  ggtitle("Overall Sequence Average") +
#  xlab("Observation Sequence") +
#  ylab("Running Probabilities of Target")


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
