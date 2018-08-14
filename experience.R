Experience <- R6::R6Class(
  "Experience",
  public = list(
    
    initialize = function(max_size = 100000) {
      private$memory <- DBI::dbConnect(RSQLite::SQLite(), "")
      private$max_size <- max_size
    },
    
    sample = function(size = 32) {
      
      n <- ifelse(private$full, private$max_size, private$i - 1)
      ids <- sample.int(n, size) %>% paste(collapse = ", ")
      
      df <- DBI::dbGetQuery(
        private$memory, 
        glue::glue("select * from where i in ({ids})")
      )
      
      batch <- df$obj %>% 
        map(unserialize) %>% 
        transpose(private$memory$mget(ids))
      
      list(
        s_t = abind::abind(batch$s_t, along = 0.1),
        s_t1 = abind::abind(batch$s_t1, along = 0.1),
        terminal = unlist(batch$terminal),
        action = unlist(batch$action),
        reward = unlist(batch$reward)
      )
    },
    
    push = function(s_t, s_t1, terminal, action, reward) {
      
      obj <- list(
        s_t = s_t,
        s_t1 = s_t1,
        terminal = as.logical(terminal),
        action = as.integer(action),
        reward = reward
      )
      
      df <- dplyr::data_frame(i = private$i, obj = list(serialize(obj, NULL)))
      
      if (private$full) {
        
        DBI::dbSendQuery(
          private$memory, 
          glue::glue("delete from memory where i = {private$i}")
        )
        
      }
      
      DBI::dbWriteTable(private$memory, "memory", df, append = TRUE)
    
      if (private$i == private$max_size) {
        private$i <- 1L
        private$full <- TRUE
      } else {
        private$i <- private$i + 1L 
      }
      
      invisible(TRUE)
    }
    
  ),
  
  private = list(
    path = NULL,
    memory = NULL,
    i = 1L,
    full = FALSE,
    max_size = NULL
  )
  
)




