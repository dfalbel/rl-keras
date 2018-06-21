
build_model <- function(input_shape = c(210, 160, 4), n_actions = 4) {
  input <- layer_input(input_shape)
  
  output <- input %>%
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
  model  
}





