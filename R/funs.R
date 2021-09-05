myfun <- function(x) {x+1}

myfun2 <-function(x,y) { (x+y) / y}

myfun3 <-function(a,b,c) {tibble(a,b,c)}



#################
## DATASETS #############
elevator<-tibble(
  VIDELT = rep(2,14),
  TAR = rep("S",14),
  BEH = c("o","x",
          "x","x",
          "A",
          "o","x","x","x",
          "A",
          "o","o","o","x"),
  LAG_BEH = lag(BEH)
)
