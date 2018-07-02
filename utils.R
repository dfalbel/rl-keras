to_gs <- function(x) {
  (x[,,1] + x[,,2] + x[,,3])/3
}

restart_env <- function(env) {
  list(
    s_t = env$reset() %>%
      abind::abind(., ., ., ., along = 3) %>%
      abind::abind(along = 0.1),
    reward = 0
  )
}
