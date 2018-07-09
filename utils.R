grayscale <- function(x) {
  dims <- c(dim(x)[-3], 1)
  x <- (x[,,1, drop = FALSE] + x[,,2, drop = FALSE] + x[,,3, drop = FALSE])/3
  array(as.integer(x), dim = dims)
}

downsample <- function(x, by = 2) {
  dims <- dim(x)
  x[seq(1, to = dims[1], by = by), seq(1, to = dims[2], by = by),]
}

preprocess <- function (x) {
  x %>%
    downsample(by = 4) %>%
    grayscale()
}

restart_env <- function(env) {
  list(
    s_t = env$reset() %>%
      preprocess() %>%
      abind::abind(., ., ., ., along = 3) %>%
      abind::abind(along = 0.1),
    reward = 0
  )
}
