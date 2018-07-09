# inspiration: https://yanpanlau.github.io/2016/07/10/FlappyBird-Keras.html

library(reticulate)
library(zeallot)
library(keras)
library(kextra)
library(purrr)
gym <- import("gym")
source("model.R")
source("utils.R")
library(tictoc)

# parameters --------------------------------------------------------------

observe_iter <- 100000 # number of steps to observe before starting training
explore_iter <- 1000000 # number of iterations to do exploration
replay_memory <- 100000 # number of transitions to remember
environment_name <- "BreakoutDeterministic-v4"
final_epsilon <-  0.02 # final value of epsilon
initial_epsilon <- 1 # starting value of epsilon
batch_size <- 32
gamma <- 0.99

# setup -------------------------------------------------------------------

env <- gym$make(environment_name)
env_shape <- unlist(env$observation_space$shape)
experience <- vector("list", replay_memory)

t <- 0
state_t <- restart_env(env)
epsilon <- initial_epsilon
models <- build_model(dim(state_t$s_t)[-1], n_actions = env$action_space$n)
experience <- vector("list", replay_memory)
loss <- Inf
episode <- 0
frames <- 0
acc_reward <- 0
reward_vec <- c()
start <- lubridate::now()

while (TRUE) {

  if (is.null(state_t$terminal) || state_t$terminal) {
    reward_vec <- c(acc_reward, head(reward_vec, 100))
    end <- lubridate::now()
    cat(glue::glue("\n Episode: {episode} | frames: {frames} | t: {t} | score: {acc_reward} | mean_score: {round(mean(reward_vec), 4)} | {round(as.numeric(end - start, units = 'secs'), 2)} seconds"), "\n")
    episode <- episode + 1
    state_t <- restart_env(env)
    frames <- 0
    acc_reward <- 0
    start <- lubridate::now()
    
  }
  
  if (runif(1) < epsilon || t < observe_iter) {
    
    action <- env$action_space$sample()
    
  } else {
    
    readout_t <- predict(models$model, state_t$s_t)
    action <- which.max(readout_t) - 1L
    
  }
  
  if (epsilon > final_epsilon & t > observe_iter) {
    
    epsilon <- epsilon - (initial_epsilon/final_epsilon) / explore_iter
    
  }
  
  c(frame, reward, terminal, lives) %<-% env$step(action)
  
  s_t1 <- abind::abind(
    preprocess(frame), 
    state_t$s_t[,,,1:3], 
    along = 3
  ) %>%
    abind::abind(along = 0.1)
  
  state_t <- list(
    s_t = state_t$s_t,
    action = action,
    reward = sign(reward),
    s_t1 = s_t1,
    terminal = terminal
  )
  
  acc_reward <- acc_reward + state_t$reward

  experience[[t %% replay_memory + 1]] <- state_t
  
  if (t > observe_iter) {
    
    ids <- sample.int(replay_memory, batch_size)
    batch <- experience[ids]
    batch <- purrr::transpose(batch)
    
    batch_s_t <- abind::abind(batch$s_t, along = 1)
    batch_action <- flatten_int(batch$action) %>% to_categorical(env$action_space$n)
    batch_s_t1 <- abind::abind(batch$s_t1, along = 1)
    batch_reward <- flatten_dbl(batch$reward)
    batch_terminal <- flatten_lgl(batch$terminal)
    
    batch_prediction <- predict(models$model, batch_s_t1) %>% apply(1, max)
  
    y <- batch_reward + (!batch_terminal)*gamma*batch_prediction
    
    loss <- train_on_batch(
      models$model2, 
      x = list(batch_s_t, batch_action),
      y = y
    )
    
  }
    
  t <- t + 1 
  state_t$s_t <- state_t$s_t1
  
  if (t %% 100000 == 0) {
    save_model_weights_hdf5(models$model, "model_weigths.hdf5", overwrite = TRUE)
  }
  
  frames <- frames + 1
}




