# inspiration: https://yanpanlau.github.io/2016/07/10/FlappyBird-Keras.html

library(reticulate)
library(keras)
gym <- import("gym")
source("model.R")

# parameters --------------------------------------------------------------

observe_iter <- 1000 # number of steps to observe before starting training
explore_iter <- 200000 # number of iterations to do exploration
replay_memory <- 1000 # number of transitions to remember
environment_name <- "Breakout-v0"
final_epsilon = 0.0001 # final value of epsilon
initial_epsilon = 0.1 # starting value of epsilon

# setup -------------------------------------------------------------------

env <- gym$make(environment_name)
env_shape <- unlist(env$observation_space$shape)
experience <- vector("list", replay_memory)

to_gs <- function(x) {
  apply(x, c(1,2), mean)
}



state_t <- env$reset()
state_t <- to_gs(state_t)
state_t <- abind::abind(state_t, state_t, state_t, state_t, along = 3)
dim(state_t) <- c(1, dim(state_t))

model <- build_model(dim(state_t)[-1], n_actions = env$action_space$n)

t <- 0
while (TRUE) {
  
  
  
  
  
}




