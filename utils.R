to_gs <- function(x) {
  (x[,,1] + x[,,2] + x[,,3])/3
}

restart_env <- function(env) {
  list(
    s_t = env$reset() %>%
      keras::image_array_resize(height = 65, width = 40) %>%
      to_gs() %>% 
      abind::abind(., ., ., ., along = 3) %>%
      abind::abind(along = 0.1),
    reward = 0
  )
}
