to_gs <- function(x) {
  apply(x, c(1,2), mean)
}