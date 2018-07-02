
build_model <- function(input_shape = c(210, 160, 4), n_actions = 4) {
  input <- layer_input(input_shape)
  
  output <- input %>%
    layer_image_resize(size = c(65, 40)) %>%
    layer_image_rgb_to_grayscale() %>%
    layer_conv_2d(filters = 32, kernel_size = c(4,4)) %>%
    layer_max_pooling_2d(pool_size = c(4,4)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2,2)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(2,2)) %>%
    layer_max_pooling_2d(pool_size = c(2,2)) %>%
    layer_flatten() %>%
    layer_dense(128, activation = "relu") %>%
    layer_dense(n_actions)
  
  model <- keras_model(input, output)
  
  action <- layer_input(shape = n_actions)
  
  scalar <- layer_multiply(list(output, action)) %>%
   layer_lambda(f = function(x) k_sum(x, axis = 2, keepdims = TRUE))
  
  model2 <- keras_model(list(input, action), scalar)
  
  model2 %>%
    compile(
      loss = "mse",
      optimizer = "adam"
    )
  
  list(
    model = model,
    model2 = model2
  )
}





