\name{group_filter}
\alias{group_filter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Group Filter
}
\description{
A helper function which provides counts and proportions of all levels of a categorical DV according to two nesting variables.

This is to be used in conjunction with filter to remove possibly problematic groups (e.g., those with low base rates of target reinfroced behaviors)

}
\usage{
group_filter(data, GROUP1, GROUP2, DV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataframe or tibble}
  \item{GROUP1}{
A column / vector describing the highest order grouping factor (e.g., person )
}
  \item{GROUP2}{
A column / vector describing the second order grouping factor (e.g., segment)
}
  \item{DV}{
A column containing the behavioral dependent variable
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a grouped data frame containing the 1) primary grouping variable, 2) the secondary grouping variable,
3) the DV, 4) count of DV (per secondary within primary grouping variable), 5) n (number of total observations within both groupings) 5) proportion of each level of DV per primary / secondary variable combination
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

## The function is currently defined as
function(dat, GROUP1, GROUP2, DV)
{
  #  https://stackoverflow.com/questions/58846126/using-rlang-quasiquotation-with-dplyr-join-functions
 require(tidyverse)
  data <- data.frame(dat)
  GROUP1 <- dplyr::enquo(GROUP1)
  GROUP2 <- dplyr::enquo(GROUP2)
  DV <- dplyr::enquo(DV)

  #  data <- data.frame(data)
  # dat
  # FRQ OF DV by group 2
  FRQ_G2_DV <- dat %>%
    dplyr::group_by(!!GROUP1, !!GROUP2, !!DV) %>%
    dplyr::summarize(count = n()) %>%
    ungroup()
  # FRQ_G2_DV
  # Number of cases of group2
  FRQ_F2 <- dat%>%
    dplyr::group_by(!!GROUP1,!!GROUP2) %>%
    dplyr::summarize(n = n()) %>%
    ungroup()
  #FRQ_F2
  # Summary by video
  # vid_sum<-dplyr::left_join(FRQ_G2_DV,FRQ_F2, by = c(!!GROUP1,!!GROUP2) )
  vid_sum<-dplyr::left_join(FRQ_G2_DV,FRQ_F2)
  vid_sum<-vid_sum %>% dplyr::mutate(prop = count/n)
  vid_sum
  }
  group_filter(elevator,VIDELT, TAR,BEH)
# A tibble: 6 x 6
  VIDELT TAR   BEH   count     n  prop
   <dbl> <chr> <chr> <int> <int> <dbl>
1      1 S     A         1     7 0.143
2      1 S     o         2     7 0.286
3      1 S     x         4     7 0.571
4      2 S     A         1     7 0.143
5      2 S     o         3     7 0.429
6      2 S     x         3     7 0.429


> two_person_picture
# A tibble: 30 x 3
   VIDELT TAR   BEH
    <dbl> <chr> <chr>
 1      1 ABE   x
 2      1 ABE   x
 3      1 ABE   x
 4      1 ABE   o

 > group_filter(two_person_picture,VIDELT, TAR,BEH)
`summarise()` has grouped output by 'VIDELT', 'TAR'. You can override using the `.groups` argument.
`summarise()` has grouped output by 'VIDELT'. You can override using the `.groups` argument.
Joining, by = c("VIDELT", "TAR")
# A tibble: 6 x 6
  VIDELT TAR   BEH   count     n  prop
   <dbl> <chr> <chr> <int> <int> <dbl>
1      1 ABE   A         2    15 0.133
2      1 ABE   o         7    15 0.467
3      1 ABE   x         6    15 0.4
4      2 JAN   A         2    15 0.133
5      2 JAN   o         7    15 0.467
6      2 JAN   x         6    15 0.4
}


