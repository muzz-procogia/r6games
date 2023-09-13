#' A deck of cards
#'
#' A deck of cards class compatible with Interfacing
#'
#'@section Fields:
#'\describe{
#'  \item{max_state}{The baseline state of Deck that it may return to when `reshuffle(0L)`d.}
#'  \item{state}{The current state of the Deck.}
#'  \item{hands}{The list of hands currently drawn from the Deck.}
#'}
#'
#'@section Methods:
#'\code{$new()} Creates a new Deck object.
#'\code{$print()} Returns a list containing the Deck state and list of hands.
#'\code{$reshuffle(hand_index = 0L)} If hand_index is 0, returns Deck to its max_state. Otherwise, reshuffles the hand at `hand_index` into the Deck `state`.
#'\code{$draw(ncards = 5)} Draws a hand of `ncards` from `state` and adds them to `hands`.
#'
#'
#'@importFrom R6 R6Class
#'@name Deck
NULL

#'@export
Deck <- R6Class(
  "Deck",
  list(
    max_state = paste0(rep(c("A", 2:10, "J", "Q", "K"), 4), c("♠", "♥", "♦", "♣")), # gcd(4,13)=1
    state = paste0(rep(c("A", 2:10, "J", "Q", "K"), 4), c("♠", "♥", "♦", "♣")), # gcd(4,13)=1,
    hands = list(),
    reshuffle = function(`hand index` = 0L) {
      hand_index <- as.integer(`hand index`)
      stopifnot(hand_index >= 0, hand_index <= length(self$hands))
      if(hand_index == 0) {
        self$state = self$max_state
        self$hands = list()
      } else {
        self$state = c(self$state, self$hand[[hand_index]])
        self$hands = self$hands[-hand_index]
      }
      self$state
    },
    draw = function(`number of cards` = 5L) {
      ncards <- as.integer(`number of cards`)
      stopifnot(ncards <= length(self$state))
      hand <- sample(self$state, ncards)
      self$state <- setdiff(self$state, hand)
      self$hands <- c(self$hands, list(hand))
      hand
    },
    print = function() {
      list(state=self$state,hands=self$hands)
    }
  )
)
