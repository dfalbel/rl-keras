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

observe <- 500 # number of episodes to observe before starting training
explore <- 9500 # nuber of episodes to do exploration
episodes <- 10000 # total number of episodes
replay_memory <- 1000000 # number of transitions to remember
path_replay_buffer <- "buffer"
environment_name <- "BreakoutDeterministic-v4"
initial_epsilon <- 1
final_epsilon <- 0.1
batch_size <- 32
gamma <- 0.99


# setup -------------------------------------------------------------------

input_shape <- c(53, 40, 4)
env <- gym$make(environment_name)
experience <- Experience$new(replay_memory, path = path_replay_buffer)
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
  
  # save model weights
  if(i %% 500 == 0)
    save_model_weights_hdf5(
      models$score_model, 
      "model_weights.hdf5", 
      overwrite = TRUE
    )
  
  cat(glue::glue("Episode: {i} | Score: {history[i]} | Mean: {last100} | Elapsed: {time} sec"), "\n")
}

saveRDS(history, "history.rds")
