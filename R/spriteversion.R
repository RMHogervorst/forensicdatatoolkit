rSprite.huge <- 1e15
rSprite.dust <- 1e-12

# See if a mean is GRIM-consistent. If not, return the nearest mean that is.
rSprite.checkGrim <- function (N, tMean, dp) {
  gMean <- tMean
  int <- round(tMean * N)           # nearest integer; doesn't matter if this rounds up or down
  frac <- int / N
  dif <- abs(tMean - frac)
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  if (dif > gran) {
    gMean <- round(int / N, dp)
    warning(paste0('Fails GRIM test for mean ',tMean, ', N=',N, ' nearest mean is: ',gMean))
  }

  return(gMean)
}

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
# --- returns minimum and maximum standard deviation

rSprite.sdLimits <- function (N, tMean, scaleMin, scaleMax, dp) {
  result <- c(rSprite.huge, -rSprite.huge)      # impossible values

  aMax <- scaleMin                              # "aMax" means "value of a to produce the max SD"
  aMin <- floor(tMean)
  bMax <- max(scaleMax, scaleMin + 1, aMin + 1) # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1

  total <- round(tMean * N)
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]

    k <- round((total - (N * b)) / (a - b))
    k <- min(max(k, 1), N - 1)   # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, N - k))
    result[m] <- round(sd(vec), dp)
  }

  return(result)
}


# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
# -- this is a massive function that can be extracted into separate ones, must be
# --- where does the vector come from?
## returns a vector of result
rSprite.delta <- function (vec, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
  # Most of the time we change a pair of numbers by +/- 1, but 2 allows us to jump over fixed numbers.
  delta <- 1
  if (    (runif(1) < 0.2)
          && ((length(vec[vec > (scaleMin + 1)])) > 0)    # Check there is a number we can decrement by 2!
  ) {
    delta <- 2
  }

  # Select an element to decrement. This should be greater than the minimum, and not
  #  1 greater than a fixed value (because decrementing it would give us the fixed value).
  # For better performance, at the risk of modest bias, we select from unique elements only.
  ## ---- which elements can be decreased, with early stopping if non are able to decrease
  uniqueCanDec <- !duplicated(vec)
  notFixedDec <- if (length(fixed) > 0) !(vec %in% (fixed + delta)) else TRUE
  indexCanDec <- uniqueCanDec & (vec > (scaleMin + delta - 1)) & notFixedDec
  if (length(indexCanDec) == 0) {
    return(vec)           # Go back and try again.
  }

  # Decide if we need to increase or decrease the SD.
  # --- is sd smaller than required sd?
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < sd)

  # If we want to make the SD larger, there is no point in decrementing the largest item,
  #  unless we have no choice.
  # --- not sure what this step does
  if (increaseSD) {
    indexCanDecTry1 <- indexCanDec & (vec < max(vec))
    if (sum(indexCanDecTry1) > 0) {
      indexCanDec <- indexCanDecTry1
    }
  }
  # this can be done smarter, there is no need for which the trues will show
  whichCanDec <- which(indexCanDec)
  whichWillDec <- whichCanDec[as.integer(runif(1) * length(whichCanDec)) + 1];
  willDec <- vec[whichWillDec]

  # Select an element to increment. This should be smaller than the maximum,
  #  and not 1 less than a fixed value.
  vecInc <- vec
  vecInc[whichWillDec] <- rSprite.huge        # mark the element that we decremented, so we can exclude it
  uniqueCanInc <- !duplicated(vecInc)
  notFixedInc <- if (length(fixed) > 0) !(vecInc %in% (fixed - delta)) else TRUE
  indexCanInc <- uniqueCanInc & (vecInc < (scaleMax - delta + 1)) & (vecInc != rSprite.huge) & notFixedInc

  # If we want to make the SD smaller, there is no point in incrementing an item larger than
  #   the one that we are going to decrement, unless we have no other choice.
  if (!increaseSD) {
    indexCanIncTry1 <- indexCanInc & (vec < willDec)
    if (sum(indexCanIncTry1) > 0) {
      indexCanInc <- indexCanIncTry1
    }
  }

  # There is no point in incrementing an element that is <delta> smaller than the one
  #  that we are going to decrement, unless we have no other choice.
  dontInc <- willDec - delta
  indexCanIncTry2 <- indexCanInc & (vecInc != dontInc)
  if (sum(indexCanIncTry2) > 0) {
    indexCanInc <- indexCanIncTry2
  }

  # If we can't find an element to increment, just return the current vector unchanged and let our caller sort it out.
  if (sum(indexCanInc) < 1) {
    return(vec)           # Go back and try again.
  }

  whichCanInc <- which(indexCanInc)
  whichWillInc <- whichCanInc[as.integer(runif(1) * length(whichCanInc)) + 1];

  # Another option is to only change one of the cells (decrement one without incrementing another,
  #  or vice versa).
  # This enables us to explore different means that still round to the same target value.
  # I could have probably written this more elegantly if I'd thought of this issue before I wrote
  #  the code above to select candidates to be both incremented and decremented, but there we are.
  # So what we do here is to perform either the decrement or the increment first, and then see if
  #  the mean is still GRIM-consistent with the target mean. If it is, in a proportion of cases,
  #  we don't adjust the other cell.
  # This can probably be optimised further, by considering whether we want to increase or decrease the SD.
  # For some reason that I don't understand, the most "even" exploration of the means seems to occur
  #  if the proportion of cases where we change the mean is not too large.

  decFirst <- (runif(1) < 0.5)
  if (decFirst) {
    vec[whichWillDec] <- vec[whichWillDec] - delta
  }
  else {
    vec[whichWillInc] <- vec[whichWillInc] + delta
  }

  doInc <- TRUE
  if (runif(1) < 0.1) {
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) == tMean) {         # New mean is GRIM-consistent, so we will keep it.
      doInc <- FALSE
    }
  }

  if (doInc) {
    if (decFirst) {
      vec[whichWillInc] <- vec[whichWillInc] + delta
    }
    else {
      vec[whichWillDec] <- vec[whichWillDec] - delta
    }
  }

  return(vec)
}

