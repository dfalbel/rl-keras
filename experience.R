get_path <- function(base_path, ids) {
  int <- sprintf("%05d", ids %/% 1000)
  fin <- sprintf("%03d", ids %% 1000)
  paste0(base_path, "/", int, "/", fin)
}


Experience <- R6::R6Class(
  "Experience",
  public = list(
    
    initialize = function(max_size = 100000) {
      
      private$memory <- vector(mode = "list", length = max_size)
      
    },
    
    sample = function(size = 32) {
      
      n <- ifelse(private$full, private$max_size, private$i - 1)
      ids <- sample.int(n, size)
      
      batch <- get_path(private$path, ids) %>%
        map(readRDS) %>%
        transpose()
      
      list(
        s_t = abind::abind(batch$s_t, along = 0.1),
        s_t1 = abind::abind(batch$s_t1, along = 0.1),
        terminal = unlist(batch$terminal),
        action = unlist(batch$action),
        reward = unlist(batch$reward)
      )
    },
    
    push = function(frame, terminal, action, reward) {
      
      private$memory[[private$i]] <- list(
        s_t = s_t,
        terminal = as.logical(terminal),
        action = as.integer(action),
        reward = reward
      )
      
      if (private$i == private$max_size) {
        private$i <- 1L
        private$full <- TRUE
      } else {
        private$i <- private$i + 1 
      }
      
      invisible(TRUE)
    }
    
  ),
  
  private = list(
    path = NULL,
    i = 1,
    full = FALSE,
    max_size = NULL
  )
  
)
