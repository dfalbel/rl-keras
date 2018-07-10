# inspiration: https://yanpanlau.github.io/2016/07/10/FlappyBird-Keras.html

library(reticulate)
library(zeallot)
library(keras)
library(purrr)
library(abind)
gym <- import("gym")
source("model.R")
source("utils.R")
source("experience.R")

# parameters --------------------------------------------------------------

observe <- 10 # number of episodes to observe before starting training
explore <- 2000 # nuber of episodes to do exploration
episodes <- 5000 # total number of episodes
replay_memory <- 100000 # number of transitions to remember
environment_name <- "BreakoutDeterministic-v4"
initial_epsilon <- 1
final_epsilon <- 0.05
batch_size <- 32
gamma <- 0.99

# setup -------------------------------------------------------------------

input_shape <- c(53, 40, 4)
env <- gym$make(environment_name)
experience <- Experience$new(replay_memory)
history <- rep(NA, episodes)
models <- build_model(input_shape, env$action_space$n)
epsilon <- 1L

for (i in seq_len(episodes)) {
  
  if (i >= observe & i < (observe + explore)) 
    epsilon <- epsilon - (initial_epsilon/final_epsilon) / explore
  
  if (i >= observe)
    train <- TRUE
  else 
    train <- FALSE
  
  time <- system.time({
    history[i] <- play_episode(env, epsilon, models, experience, train)  
  })
  
  last100 <- tail(history[!is.na(history)], 100) %>% mean() %>% round(2)
  time <- round(time[3], 2)
  
  cat(glue::glue("Episode: {i} | Score: {history[i]} | Mean: {last100} | Elapsed: {time} sec"), "\n")
}
