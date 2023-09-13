#' A set of dice
#'
#' A set of dice class compatible with Interfacing
#'
#'@section Fields:
#'\describe{
#'  \item{state}{The current state of all pools.}
#'  \item{pools}{The list of dice pools currently formed.}
#'}
#'
#'@section Methods:
#'\code{$new()} Creates a new Dice object.
#'\code{$print()} Returns a list containing the Deck state and list of hands.
#'\code{$add_pool()} Creates a named pool with the specified dice
#'
#'
#'@importFrom R6 R6Class
#'@name Dice
NULL


#'@export
Dice <- R6Class(
  "Dice",
  lock_objects = FALSE,
  public = list(
    # state = character(0),
    pools = list(),
    print = function() {
      list(
        # state=self$state,
           pools=self$pools)
    },
    `add pool` = function(
    # name = "",
                        d4 = 0L, d6 = 0L, d8 = 0L,
                        d10 = 0L, d12 = 0L, d20 = 0L
                        # , SHINY_RELOAD = TRUE
                        ) {
      # coerce to integers
      val_names <- paste0('d', c(4, 6, 8, 10, 12, 20))
      vals <- abs(as.integer(c(d4 = d4, d6 = d6, d8 = d8, d10 = d10, d12 = d12, d20 = d20)))
      vals[is.na(vals) | is.null(vals)] <- 0
      names(vals) <- val_names

      # If the name collides or is empty, we generate a name.
      # if(name == "" | name %in% names(self$pools)) {
      #   name0 <- paste0("pool_", str_flatten(vals, collapse = '.'))
      #   named_pools <- names(self$pools)[grepl(name0, names(self$pools))]
      #   name <- paste0(name0, '_', length(named_pools))
      # }

      # Add values to the list of pools
      self$pools <- c(self$pools, list(vals))

      # Add a rolling function
      # roll_fn_name <- paste("roll", name)
      # self[[roll_fn_name]] <- function() {
      #   vals <- self$pools[[str_sub(roll_fn_name, 6)]]
      #   lapply(
      #     names(vals),
      #     function(nom) {
      #       sample(as.integer(str_sub(nom, 2)), vals[[nom]], replace = TRUE)
      #     }
      #   )
      # }
      #
      # # Add a removing function
      # remove_fn_name <- paste("remove ", name)
      # self[[remove_fn_name]] <- function(SHINY_RELOAD = TRUE) {
      #   self$pools[[name]] <- NULL
      #   self[[roll_fn_name]] <- NULL
      #   self[[remove_fn_name]] <- NULL
      # }

      self$state
    },
    `roll pool` = function(`pool index` = 0L) {
      pool_index <- as.integer(`pool index`)
      stopifnot(pool_index >= 0, pool_index <= length(self$pools))
      if(pool_index == 0) {
        lapply(
          self$pools,
          function(vals) {
            lapply(
              names(vals),
              function(nom) {
                sample(as.integer(str_sub(nom, 2)), vals[[nom]], replace = TRUE)
              }
            ) |>
              setNames(names(vals))
          }
        )
      } else {
        vals = self$pools[[pool_index]]
        lapply(
          names(vals),
          function(nom) {
            sample(as.integer(str_sub(nom, 2)), vals[[nom]], replace = TRUE)
          }
        ) |>
          setNames(names(vals))
      }
    },
    `remove pool` = function(`pool index` = 0L) {
      pool_index <- as.integer(`pool index`)
      stopifnot(pool_index >= 0, pool_index <= length(self$pools))
      if(pool_index == 0) {
        self$pools <- NULL
      } else {
        self$pools[[pool_index]] <- NULL
      }
    }
  )
)
