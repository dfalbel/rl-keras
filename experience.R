Experience <- R6::R6Class(
  
  "Experience",
  
  public = list(
    
    i = 1,
    full = FALSE,
    max_size = NULL,
    
    initialize = function(max_size = 100000) {
      private$memory <- vector(mode = "list", max_size)
      self$max_size <- max_size
    },
    
    sample = function(size = 32) {
      
      n <- ifelse(self$full, self$max_size, self$i - 1)
      ids <- sample.int(n, size)
      
      batch <- transpose(private$memory[ids])
      
      list(
        s_t = abind::abind(batch$s_t, along = 0.1),
        s_t1 = abind::abind(batch$s_t1, along = 0.1),
        terminal = unlist(batch$terminal),
        action = unlist(batch$action),
        reward = unlist(batch$reward)
      )
    },
    
    push = function(s_t, s_t1, terminal, action, reward) {
      
      private$memory[[self$i]] <- list(
        s_t = s_t,
        s_t1 = s_t1,
        terminal = as.logical(terminal),
        action = as.integer(action),
        reward = reward
      )
      
      if (self$i == self$max_size) {
        self$i <- 1L
        self$full <- TRUE
      } else {
        self$i <- self$i + 1 
      }
      
      invisible(TRUE)
    }
    
  ),
  
  private = list(
    memory = NULL
  )
  
)




