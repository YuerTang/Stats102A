by_type <- function(x, sort=FALSE) {
  integers <- integer(0)
  doubles <- double(0)
  characters <- character(0)
  
  for (value in x){
    if (is.na(value)) {
      characters <- c(characters, NA)
      next
    }
    if (is.logical(value)){
      characters[length(characters)+1] <- as.character(value)
    } 
    num <- suppressWarnings(as.numeric(value))
    # if it appears NA,then read it as character type
    if (is.na(num)) {
      characters[length(characters)+1] <- as.character(value)
    }
    else if (num%%1 ==0){
      integers[length(integers)+1] <- as.integer(value)
    }else{
      doubles[length(doubles)+1] <- as.double(value)
    }
  }
  if (sort){
    integers <- sort(integers)
    doubles <- sort(doubles)
    characters <- sort(characters, na.last = TRUE)
  }
  list(
    integers = integers,
    doubles = doubles,
    character = characters
  )
}

prime_factor <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }
  if (x < 2) {
    stop("Input must be greater than or equal to 2.")
  }
  if (x %% 1 != 0) {
    stop("Input must be an integer with no decimal values.")
  }
  
  factors <- numeric(0)
  n <- x
  d <- 2
  
  while (n > 1) {
    if (n %% d == 0) {
      factors[length(factors)+1] <- d
      n <- n / d
    } else {
      d <- d + 1
    }
  }
  
  factors
}

month_convert <- function(x, from_lang, to_lang) {
  
  month_names <- read.delim("month_names.txt",encoding = "UTF-8",row.names = 1)
  
  from_row <- as.character(month_names[from_lang, ])
  to_row   <- as.character(month_names[to_lang, ])
  
  out <- rep(NA, length(x))
  
  for (i in 1:length(x)) {
    # to get the position of that specific month on the month row
    pos <- which(from_row == as.character(x[i]))
    # if x[i] is indeed some months
    if (length(pos) > 0) {
      # append all the corresponding values (like repetitive)
      out[i] <- to_row[pos]
    }
  }
  # Catch the old levels and convert the new levels to translated levels
  old_levels <- levels(x)
  new_levels <- c()
  for (j in 1:length(old_levels)) {
    # the same logic to remain the order
    pos <- which(from_row == old_levels[j])
    if (length(pos) > 0) {
      new_levels <- c(new_levels, to_row[pos[1]])
    }
  }
  
  factor(out, levels = new_levels)
}
