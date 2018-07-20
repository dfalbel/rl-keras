Experience <- R6::R6Class(
  "Experience",
  public = list(
    
    initialize = function(max_size = 100000, path = NULL) {
      
      if (is.null(path)) {
        path <- tempfile("storr_")
      }
      
      private$path <- path
      private$memory <- storr::storr_rds(private$path)
      private$max_size <- max_size
    },
    
    sample = function(size = 32) {
      
      n <- ifelse(private$full, private$max_size, private$i - 1)
      ids <- sample.int(n, size)
      
      batch <- transpose(private$memory$mget(ids))
      
      list(
        s_t = abind::abind(batch$s_t, along = 0.1),
        s_t1 = abind::abind(batch$s_t1, along = 0.1),
        terminal = unlist(batch$terminal),
        action = unlist(batch$action),
        reward = unlist(batch$reward)
      )
    },
    
    push = function(s_t, s_t1, terminal, action, reward) {
      
      private$memory$set(
        private$i,
        list(
          s_t = s_t,
          s_t1 = s_t1,
          terminal = as.logical(terminal),
          action = as.integer(action),
          reward = reward
        )
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
    memory = NULL,
    i = 1,
    full = FALSE,
    max_size = NULL
  )
  
)




