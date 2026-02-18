library(R6)

# gameboard and decks -----------------------------------------------------
# Do not change this code

gameboard <- data.frame(
  space = 1:40,
  title = c(
    "Go", "Mediterranean Avenue", "Community Chest", "Baltic Avenue",
    "Income Tax", "Reading Railroad", "Oriental Avenue", "Chance",
    "Vermont Avenue", "Connecticut Avenue", "Jail", "St. Charles Place",
    "Electric Company", "States Avenue", "Virginia Avenue",
    "Pennsylvania Railroad", "St. James Place", "Community Chest",
    "Tennessee Avenue", "New York Avenue", "Free Parking",
    "Kentucky Avenue", "Chance", "Indiana Avenue", "Illinois Avenue",
    "B & O Railroad", "Atlantic Avenue", "Ventnor Avenue", "Water Works",
    "Marvin Gardens", "Go to jail", "Pacific Avenue",
    "North Carolina Avenue", "Community Chest", "Pennsylvania Avenue",
    "Short Line Railroad", "Chance", "Park Place", "Luxury Tax",
    "Boardwalk"), stringsAsFactors = FALSE)
chancedeck <- data.frame(
  index = 1:15,
  card = c(
    "Advance to Go", "Advance to Illinois Ave.",
    "Advance to St. Charles Place", "Advance token to nearest Utility",
    "Advance token to the nearest Railroad",
    "Take a ride on the Reading Railroad",
    "Take a walk on the Boardwalk", "Go to Jail", "Go Back 3 Spaces",
    "Bank pays you dividend of $50", "Get out of Jail Free",
    "Make general repairs on all your property", "Pay poor tax of $15",
    "You have been elected Chairman of the Board",
    "Your building loan matures"), stringsAsFactors = FALSE)
communitydeck <- data.frame(
  index = 1:16,
  card = c(
    "Advance to Go", "Go to Jail",
    "Bank error in your favor. Collect $200", "Doctor's fees Pay $50",
    "From sale of stock you get $45", "Get Out of Jail Free",
    "Grand Opera Night Opening", "Xmas Fund matures", "Income tax refund",
    "Life insurance matures. Collect $100", "Pay hospital fees of $100",
    "Pay school tax of $150", "Receive for services $25",
    "You are assessed for street repairs",
    "You have won second prize in a beauty contest",
    "You inherit $100"), stringsAsFactors = FALSE)

# RandomDice class --------------------------------------------------------
# Do not change this code

RandomDice <- R6Class(
  classname = "RandomDice",
  public = list(
    verbose = NA,
    initialize = function(verbose = FALSE){
      stopifnot(is.logical(verbose))
      self$verbose = verbose
    },
    roll = function() {
      outcome <- sample(1:6, size = 2, replace = TRUE)
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)

# Preset Dice -------------------------------------------------------------
# Do not change this code

PresetDice <- R6Class(
  classname = "PresetDice",
  public = list(
    verbose = NA,
    preset_rolls = double(0),
    position = 1,
    initialize = function(rolls, verbose = FALSE){
      stopifnot(is.logical(verbose))
      stopifnot(is.numeric(rolls))
      self$preset_rolls = rolls
      self$verbose = verbose
    },
    roll = function(){
      if(self$position > length(self$preset_rolls)){
        stop("You have run out of predetermined dice outcomes.")
      }
      outcome <- c(self$preset_rolls[self$position],
                   self$preset_rolls[self$position + 1])
      self$position <- self$position + 2
      if(self$verbose){
        cat("Dice Rolled:", outcome[1], outcome[2], "\n")
      }
      outcome
    }
  )
)


# Chance and Community Decks ----------------------------------------------
# Do not change this code

# This R6 class object shuffles the card deck when initialized.
# It has one method $draw(), which will draw a card from the deck.
# If all the cards have been drawn (position = deck length), then it will
# shuffle the cards again.
# The verbose option cats the card that is drawn on to the screen.
CardDeck <- R6Class(
  classname = "CardDeck",
  public = list(
    verbose = NA,
    deck_order = double(0),
    deck = data.frame(),
    position = 1,
    initialize = function(deck, verbose = FALSE){
      stopifnot(is.data.frame(deck),
                is.numeric(deck[[1]]),
                is.character(deck[[2]]))
      self$deck_order <- sample(length(deck[[1]]))
      self$verbose <- verbose
      self$deck <- deck
    },
    draw = function(){
      if(self$position > length(self$deck_order)){
        # if we run out of cards, shuffle deck
        # and reset the position to 1
        if(self$verbose){
          cat("Shuffling deck.\n")
        }
        self$deck_order <- sample(length(self$deck[[1]]))
        self$position <- 1
      }
      outcome <- c(self$deck_order[self$position]) # outcome is the value at position
      self$position <- self$position + 1 # advance the position by 1
      if(self$verbose){
        cat("Card:", self$deck[outcome, 2], "\n")
      }
      outcome # return the outcome
    }
  )
)


# R6 Class SpaceTracker ---------------------------------------------------
# Do not change this code

SpaceTracker <- R6Class(
  classname = "SpaceTracker",
  public = list(
    counts = rep(0, 40),
    verbose = TRUE,
    tally = function(x){
      self$counts[x] <- self$counts[x] + 1
      if(self$verbose){
        cat("Added tally to ", x, ": ", gameboard$title[x], ".\n", sep = "")
      }
    },
    initialize = function(verbose){
      self$verbose <- verbose
    }
  )
)

# R6 Class Player ---------------------------------------------------------

Player <- R6Class(
  classname = "Player",
  public = list(
    pos = 1,
    verbose = TRUE,
    in_jail = FALSE,
    jail_turns = 0,
    initialize = function(verbose = FALSE, pos = 1) {
      self$verbose <- verbose
      self$pos <- pos
    },
    move_fwd = function(n) {
      self$pos <- self$pos + n
      if (self$pos > 40) {
        self$pos <- self$pos - 40
      }
      if (self$verbose) {
        cat("Player moves forward ", n, ".\n", sep = "")
        cat("Player is now at ", self$pos, ": ",
            gameboard$title[self$pos], ".\n", sep = "")
      }
    },
    go_to_jail = function() {
      self$pos <- 11
      self$in_jail <- TRUE
      self$jail_turns <- 0
      if (self$verbose) {
        cat("Player goes to jail.\n")
      }
    }
  )
)


# Helper functions --------------------------------------------------------

# Find the nearest railroad moving forward from current position
nearest_railroad <- function(pos) {
  railroads <- c(6, 16, 26, 36)
  ahead <- railroads[railroads > pos]
  if (length(ahead) > 0) return(ahead[1])
  return(railroads[1])  # wrap around
}

# Find the nearest utility moving forward from current position
nearest_utility <- function(pos) {
  utilities <- c(13, 29)
  ahead <- utilities[utilities > pos]
  if (length(ahead) > 0) return(ahead[1])
  return(utilities[1])  # wrap around
}

# Handle drawing and acting on a Chance card
# Returns TRUE if player was sent to jail
handle_chance <- function(player, spacetracker) {
  verbose <- player$verbose
  if (verbose) cat("Draw a Chance card.\n")
  card <- chance$draw()

  new_pos <- NULL
  go_jail <- FALSE

  if (card == 1) {        # Advance to Go
    new_pos <- 1
  } else if (card == 2) { # Advance to Illinois Ave.
    new_pos <- 25
  } else if (card == 3) { # Advance to St. Charles Place
    new_pos <- 12
  } else if (card == 4) { # Advance token to nearest Utility
    new_pos <- nearest_utility(player$pos)
  } else if (card == 5) { # Advance token to the nearest Railroad
    new_pos <- nearest_railroad(player$pos)
  } else if (card == 6) { # Take a ride on the Reading Railroad
    new_pos <- 6
  } else if (card == 7) { # Take a walk on the Boardwalk
    new_pos <- 40
  } else if (card == 8) { # Go to Jail
    go_jail <- TRUE
  } else if (card == 9) { # Go Back 3 Spaces
    new_pos <- player$pos - 3
    if (new_pos < 1) new_pos <- new_pos + 40
  }
  # Cards 10-15: no movement

  if (go_jail) {
    player$go_to_jail()
    spacetracker$tally(11)
    return(TRUE)
  }

  if (!is.null(new_pos)) {
    player$pos <- new_pos
    if (verbose) {
      cat("Player moves to ", new_pos, ": ",
          gameboard$title[new_pos], ".\n", sep = "")
    }
    spacetracker$tally(new_pos)
    # Going back 3 from Chance at 37 lands on Community Chest at 34
    if (new_pos %in% c(3, 18, 34)) {
      return(handle_community_chest(player, spacetracker))
    }
  }

  return(FALSE)
}

# Handle drawing and acting on a Community Chest card
# Returns TRUE if player was sent to jail
handle_community_chest <- function(player, spacetracker) {
  verbose <- player$verbose
  if (verbose) cat("Draw a Community Chest card.\n")
  card <- community$draw()

  if (card == 1) {        # Advance to Go
    player$pos <- 1
    if (verbose) cat("Player moves to 1: Go.\n")
    spacetracker$tally(1)
  } else if (card == 2) { # Go to Jail
    player$go_to_jail()
    spacetracker$tally(11)
    return(TRUE)
  }
  # Cards 3-16: no movement

  return(FALSE)
}

# Process what happens when a player lands on a space
# Returns TRUE if player was sent to jail (turn should end)
process_landing <- function(player, spacetracker) {
  # Go to Jail space
  if (player$pos == 31) {
    player$go_to_jail()
    spacetracker$tally(11)
    return(TRUE)
  }

  # Tally the space
  spacetracker$tally(player$pos)

  # Chance spaces
  if (player$pos %in% c(8, 23, 37)) {
    return(handle_chance(player, spacetracker))
  }

  # Community Chest spaces
  if (player$pos %in% c(3, 18, 34)) {
    return(handle_community_chest(player, spacetracker))
  }

  return(FALSE)
}


# take_turn function ------------------------------------------------------

take_turn <- function(player, spacetracker) {
  verbose <- player$verbose

  # --- Player starts turn in jail ---
  if (player$in_jail) {
    dice_rolls <- dice$roll()
    player$jail_turns <- player$jail_turns + 1
    is_doubles <- dice_rolls[1] == dice_rolls[2]

    if (is_doubles) {
      # Rolled doubles: exit jail, move, but do NOT roll again
      if (verbose) cat("In jail but rolled doubles.\n")
      if (verbose) cat("Player exits jail.\n")
      player$in_jail <- FALSE
      player$jail_turns <- 0
      if (verbose) {
        cat("Player starts at ", player$pos, ": ",
            gameboard$title[player$pos], ".\n", sep = "")
      }
      player$move_fwd(sum(dice_rolls))
      process_landing(player, spacetracker)
    } else if (player$jail_turns >= 3) {
      # Third turn in jail: forced exit, move regardless
      if (verbose) {
        cat("Player's third turn in jail. Player must exit jail.\n")
      }
      if (verbose) cat("Player exits jail.\n")
      player$in_jail <- FALSE
      player$jail_turns <- 0
      if (verbose) {
        cat("Player starts at ", player$pos, ": ",
            gameboard$title[player$pos], ".\n", sep = "")
      }
      player$move_fwd(sum(dice_rolls))
      process_landing(player, spacetracker)
    } else {
      # Stay in jail
      if (verbose) cat("Player stays in jail.\n")
      spacetracker$tally(player$pos)
    }
    return(invisible(NULL))
  }

  # --- Normal play (not in jail) ---
  doubles_count <- 0

  repeat {
    dice_rolls <- dice$roll()
    is_doubles <- dice_rolls[1] == dice_rolls[2]

    if (is_doubles) {
      doubles_count <- doubles_count + 1
      if (verbose) {
        cat("Doubles count is now ", doubles_count, ".\n", sep = "")
      }
    }

    # Three consecutive doubles: go directly to jail
    if (doubles_count == 3) {
      player$go_to_jail()
      spacetracker$tally(11)
      return(invisible(NULL))
    }

    # Normal move
    if (verbose) {
      cat("Player starts at ", player$pos, ": ",
          gameboard$title[player$pos], ".\n", sep = "")
    }
    player$move_fwd(sum(dice_rolls))

    # Process the space landed on (Go to Jail, Chance, CC)
    went_to_jail <- process_landing(player, spacetracker)
    if (went_to_jail) return(invisible(NULL))

    # If not doubles, turn ends
    if (!is_doubles) break

    # Rolled doubles: take another turn
    if (verbose) {
      cat("\nPlayer rolled doubles, so they take another turn.\n")
    }
  }
}
