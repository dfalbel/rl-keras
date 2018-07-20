build_model <- function(input_shape = c(210, 160, 4), n_actions = 4) {
  input <- layer_input(input_shape)
  
  output <- input %>%
    layer_lambda(function(x) {x/255}) %>%
    layer_conv_2d(filters = 16, kernel_size = c(8,8), strides = c(4,4), activation = "relu") %>%
    layer_conv_2d(filters = 32, kernel_size = c(4,4), strides = c(2,2), activation = "relu") %>%
    layer_flatten() %>%
    layer_dense(256, activation = "relu") %>%
    layer_dense(n_actions)
  
  score_model <- keras_model(input, output)
  
  action <- layer_input(shape = n_actions)
  
  scalar <- layer_multiply(list(output, action)) %>%
   layer_lambda(f = function(x) k_sum(x, axis = 2, keepdims = TRUE))
  
  action_model <- keras_model(list(input, action), scalar)
  
  
  
  action_model %>%
    compile(
      loss = "mse",
      optimizer = keras::optimizer_rmsprop(
        lr=0.00025, 
        rho=0.95, 
        epsilon=0.01
      )
    )
  
  list(
    score_model = score_model,
    action_model = action_model
  )
}





