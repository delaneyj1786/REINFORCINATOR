\name{elevator}
\alias{elevator}
\docType{data}
\title{
Elevator data
}
\description{
An example dataset to illustrate how the arrival of an elevator may affect subsequent operation of this elevator.

}
\usage{data("elevator")}
\format{
  A data frame with 14 observations on the following 3 variables.
  \describe{
    \item{\code{VIDELT}}{a numeric vector indicating the segment or episode of observations}
    \item{\code{TAR}}{a character vector indicating the ID of the person using the elevator}
    \item{\code{BEH}}{a character vector indicating whether the elevator was called (o), non-arrival (x), or arrival (A). }
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(elevator)


## Overall Probabilities (using "o" as Target and "A" as reinforcer)
$descriptive_statistics$prob_tar_uncorrected
[1] 0.3571429

$descriptive_statistics$prob_tar_corrected
[1] 0.4166667

## Recounted Single Data frame
recounted_elevator<-recounter(elevator,BEH,"o","A","TAR")$recounted_data_frame

## Recounted Contingency Table Stats
recount_elevator_tables<-tables_recount_table(recounted_elevator)

$output_list$avg_prob
     B_NT       B_T      A_NT       A_T
0.7638889 0.2361111 0.4027778 0.5972222

## Split by VIDLET
elevator_split<-group_splitter(elevator, BEH, "o","x","A", group = "VIDELT")
elevator_split
$`1`
# A tibble: 7 x 3
  VIDELT TAR   BEH
   <dbl> <chr> <chr>
1      1 S     o
2      1 S     x
3      1 S     x

$`2`
# A tibble: 7 x 3
  VIDELT TAR   BEH
   <dbl> <chr> <chr>
1      2 S     x
2      2 S     x
3      2 S     A

## Recounted for each VIDELT
elevator_split_recount<-group_split_recounter(elevator_split, behaviorstream = "BEH","o","A", actor = "TAR" )

## Recounted Split Contingency Table Stats
tables_recount_table(elevator_split_recount)

$output_list$avg_prob
     B_NT       B_T      A_NT       A_T
0.8333333 0.1666667 0.3333333 0.6666667

}
\keyword{datasets}
