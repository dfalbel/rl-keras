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
      abind::abind(., ., ., ., along = 3),
    terminal = FALSE
  )
}

play_episode <- function(env, epsilon, models, experience, train) {
  
  c(s_t, terminal) %<-% restart_env(env)
  score <- 0
  while(!terminal) {
    
    if (runif(1) < epsilon) {
      action <- env$action_space$sample()
    } else {
      score_predictions <- predict(models$score_model, abind(s_t, along = 0.1))
      action <- which.max(score_predictions) - 1L
    }
    
    c(frame, reward, terminal, lives) %<-% env$step(action)
    s_t1 <- abind::abind(preprocess(frame), s_t[,,1:3], along = 3)
    
    experience$push(
      s_t = s_t,
      s_t1 = s_t1,
      terminal = terminal,
      action = action,
      reward = reward
    )
    
    if (train) 
      train_step(models, experience, env$action_space$n)
    
    s_t <- s_t1
    score <- score + reward
  }
  
  score
}

train_step <- function(models, experience, n_actions) {

  c(s_t, s_t1, terminal, action, reward) %<-% experience$sample(32)
  
  
  expected_reward <- predict(models$score_model, s_t1) %>% apply(1, max)
  
  
  y <- reward + (!terminal)*gamma*expected_reward
  action <- to_categorical(action, n_actions)
  
  
  loss <- train_on_batch(
    models$action_model, 
    x = list(s_t, action),
    y = y
  )
  
  return(NULL)
}
