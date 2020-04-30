# A series of incremental improvements to the run-time and flexibility are
# possible, as SPRITE spends almost all its computational time in the swapping
# procedure. Presently, all versions (a) perform one swap at a time, (b) swap by
# a unit of 1 every time (or 2 in the case of rSPRITE), and (c) determine one
# solution set at a time. A future version could dramatically reduce run-time by
# swapping an amount of values proportional to the difference between the
# starting solution and the eventual solution, or by larger numbers. An example:
# if we consider normative IQ scores in a large sample (n=500, mean=100, SD=15),
# and begin the procedure from mean=100 and SD=0, shuffling a single value one
# unit will initially produce SD=0.06. Run on mSPRITE, it takes 179 successful
# individual swaps to reach SD=1, and 5681 to reach SD=15. This takes
# approximately 3.5 seconds - viable for a single solution, but not for any
# simulation where thousands of runs are necessary. Another possibility is
# modeling additional unique solutions using existing solutions as a template,
# which will more quickly enumerate other similar but unique solutions. However,
# none of the above are required to provision solutions for sample parameters
# typical to the behavioral and medical sciences.


rSprite.huge <- 1e15
rSprite.dust <- 1e-12

#' Copy from script, slight modifications to keep argument names consistent.
make_mean_grim <- function(mean, n, dp = 2) {
  gMean <- mean
  int <- round(mean * n) # nearest integer; doesn't matter if this rounds up or down
  frac <- int / n
  dif <- abs(mean - frac)
  # allow for rounding errors
  gran <- set_granularity(dp)
  if (dif > gran) {
    gMean <- round(frac, dp)
    warning(paste0("Fails GRIM test for mean ", mean, ", N=", n, " nearest mean is: ", gMean))
  }
  return(gMean)
}

determine_sd_limits <- function(mean, n, scale_min, scale_max, dp = 2) {
  if (scale_min >= scale_max) {
    stop("scale_min cannot be larger than scale_max")
  }
  maxtotal <- round(mean * n)
  aMax <- scale_min # "aMax" means "value of a to produce the max SD"
  aMin <- floor(mean)
  bMax <- max(scale_max, scale_min + 1, aMin + 1)
  bMin <- aMin + 1
  result <- c(
    calc_sd(aMin, bMin, maxtotal, n, dp),
    calc_sd(aMax, bMax, maxtotal, n, dp)
  )
  result
}


calc_sd <- function(a, b, total, n, dp) {
  k <- round((total - (n * b)) / (a - b))
  k <- min(max(k, 1), n - 1) # don't get it yet
  vec <- c(rep(a, k), rep(b, n - k))
  round(sd(vec), dp)
}

set_granularity <- function(dp) {
  ((0.1^dp) / 2) + rSprite.dust
}



seek_vector <- function(mean, sd, n, scale_min, scale_max, dp = 2, fixed = c()) {
  rN <- n - length(fixed)
  vec <- pmax(pmin(as.integer(runif(rN) * 2 * mean), scale_max), scale_min)
  # replace any of the fixed numbers with a random non-fixed number
  if (length(fixed) > 0) {
    whichFixed <- which(vec %in% fixed)
    notFixed <- sample(setdiff(scale_min:scale_max, fixed), length(whichFixed), replace = TRUE)
    vec[whichFixed] <- notFixed
  }
  vec <- loop_until_mean_is_ok(vec, mean, scale_min, scale_max, dp, fixed)
  vec <- loop_until_sd_is_ok(vec,mean, sd, scale_min, scale_max, dp, fixed)
  vec
}

loop_until_mean_is_ok <- function(vec, mean, scale_min, scale_max, dp, fixed = c()) {
  n <- length(c(vec, fixed))
  meanOK <- FALSE
  maxStartLoops <- n * (scale_max - scale_min)
  gran <- set_granularity(dp)
  for (i in 1:maxStartLoops) {
    fullVec <- c(vec, fixed)
    cMean <- mean(fullVec)
    dif <- abs(cMean - mean)
    increaseMean <- (cMean < mean)
    if (dif < gran) {
      meanOK <- TRUE
      break
    }

    # Identify numbers that we can increment or decrement.
    # This should exclude numbers that would become one of the fixed values.
    deltaMean <- step_size()
    fixed_indicator <- (!(vec %in% (fixed - deltaMean)))
    if (increaseMean) {
      filter <- (vec < (scale_max - deltaMean + 1)) & fixed_indicator
    }
    else {
      filter <- (vec > (scale_min + deltaMean - 1)) & fixed_indicator
    }

    canBumpMean <- which(filter)
    bumpMean <- canBumpMean[as.integer(runif(1) * length(canBumpMean)) + 1] # select a changeable number
    vec[bumpMean] <- vec[bumpMean] + (if (increaseMean) deltaMean else -deltaMean)
  }
  if (!meanOK) {
    stop("Couldn't initialize data with correct mean")
  }
  vec
}

loop_until_sd_is_ok <- function(vec,mean, sd, scale_min, scale_max, dp, fixed = c()) {
  n <- length(c(vec, fixed))
  gran <- set_granularity(dp)
  #  original notes: this maybe needs some more testing for pathological conditions
  maxLoops <- max(round(n * ((scale_max - scale_min)^2)), 1000)
  for (i in 1:maxLoops) {
    cSD <- sd(c(vec, fixed))
    if (abs(cSD - sd) <= gran) {
      result <- vec
      break
    }
    result <- sprite_delta(vec, mean, sd, scale_min, scale_max,dp = dp, fixed=fixed)
    if (length(result) == 0) { # rSprite.delta() failed (but may have generated its own message(s)).
      break
    }
  }
  result
}

#' Function that outputs 1 most of the times but <20% of the time 2
step_size <- function() {
  if ((runif(1) < 0.2)) {
    2
  } else {
    1
  }
}

# Select an element to decrement. This should be greater than the minimum, and not
#  1 greater than a fixed value (because decrementing it would give us the fixed value).
# For better performance, at the risk of modest bias, we select from unique elements only.
# -- returns logical vector to select the values we want to decrement
select_elements_to_decrement <- function(vec, fixed, delta, scale_min) {
  uniqueCanDec <- !duplicated(vec)
  notFixedDec <- if (length(fixed) > 0) !(vec %in% (fixed + delta)) else TRUE
  indexCanDec <- uniqueCanDec & (vec > (scale_min + delta - 1)) & notFixedDec
  indexCanDec
}

select_elements_to_increment <- function(vec, fixed, delta, scale_max) {
  uniqueCanInc <- !duplicated(vec)
  notFixedInc <- if (length(fixed) > 0) !(vec %in% (fixed - delta)) else TRUE
  indexCanInc <- uniqueCanInc & (vec < (scale_max - delta + 1)) & notFixedInc
  indexCanInc
}

#--- maybe if we keep all the indexes together is gets less messy
determine_sprite_logical_indexes <- function(vec, fixed, delta, scale_min, scale_max) {
  non_duplicates <- !duplicated(vec)
  if (length(fixed) > 0) {
    notFixedDec <- !(vec %in% (fixed + delta))
    notFixedInc <- !(vec %in% (fixed - delta))
  } else {
    notFixedInc <- notFixedDec <- TRUE
  }
  logical_indexes <- list(
    decreasable = non_duplicates & (vec > (scale_min + delta - 1)) & notFixedDec,
    increasable = non_duplicates & (vec < (scale_max - delta + 1)) & notFixedInc,
    non_duplicate = non_duplicates,
    not_max_vec = vec < max(vec)
  )
  logical_indexes
}

#' select a random TRUE position out of logical vector
pick_one_position <- function(logical_vector) {
  if (sum(logical_vector) == 0) stop("There are no TRUE values in vector")
  if (class(logical_vector) != "logical") stop("Not a logical vector")
  pos <- which(logical_vector)
  pos[as.integer(runif(1) * length(pos)) + 1]
}

# Original notes:
# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
sprite_delta <- function(vec, mean_value, sd, scale_min, scale_max, dp, fixed = c()) {
  delta <- if ((length(vec[vec > (scale_min + 1)])) > 0) {
    step_size()
  } else {
    1
  }
  # Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < sd)

  logical_indexes <- determine_sprite_logical_indexes(vec, fixed, delta, scale_min, scale_max)
  # indexCanDec <- select_elements_to_decrement(vec=vec, fixed=fixed, delta =delta, scale_min=scale_min)
  ### Early stopping, if  no increasing candidates nor decreasing candidates stop.
  if (
    (sum(logical_indexes$decreasable) == 0) &
      (sum(logical_indexes$increasable) == 0)
  ) {
    return(vec) # Go back and try again.
  }
  #### determining positions
  # pick decrease element (
  # if decrease: not largest item unless
  # end if
  # )
  # - pick increase element (
  #   ignore decrease one,
  #   ignore values of decrease-delta unless
  # If decrease sd: ---
  #   ignore values larger than decrease element unless
  # end if---
  #  )

  # execute actions
  # random: first decrement or increment
  # random: check if mean is ok, if so break out
  # do other  (--- optionally so we make fixed version and specifically random arm.)
  # return vec

  # If we want to make the SD larger, there is no point in decrementing the largest item,
  #  unless we have no choice.
  # -- move this logic together. set of actions to undertake when increase sd
  if (increaseSD) {
    indexCanDecTry1 <- logical_indexes$decreasable & logical_indexes$not_max_vec
    if (sum(indexCanDecTry1) > 0) {
      logical_indexes$decreasable <- indexCanDecTry1
    }
  }

  #### -- pick candidate value to decrease.
  whichWillDec <- pick_one_position(logical_indexes$decreasable)
  willDec <- vec[whichWillDec]

  # # Select an element to increment. This should be smaller than the maximum,
  # #  and not 1 less than a fixed value.
  # vecInc <- vec
  # vecInc[whichWillDec] <- rSprite.huge # mark the element that we decremented, so we can exclude it
  # uniqueCanInc <- !duplicated(vecInc)
  # notFixedInc <- if (length(fixed) > 0) !(vecInc %in% (fixed - delta)) else TRUE
  # indexCanInc <- uniqueCanInc & (vecInc < (scale_max - delta + 1)) & (vecInc != rSprite.huge) & notFixedInc
  # --- do this one separate
  ###### & (vecInc != rSprite.huge)
  vecInc <- vec
  vecInc[whichWillDec] <- rSprite.huge
  indexCanInc <- select_elements_to_increment(vecInc, fixed, delta, scale_max)

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



  whichCanInc <- which(indexCanInc)
  whichWillInc <- whichCanInc[as.integer(runif(1) * length(whichCanInc)) + 1]
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
  # ----- this is the logic that decides if we decrease or increase.
  # -- we decrease all and increase all
  decFirst <- (runif(1) < 0.5)
  if (decFirst) {
    vec[whichWillDec] <- vec[whichWillDec] - delta
  }
  else {
    vec[whichWillInc] <- vec[whichWillInc] + delta
  }

  doInc <- TRUE
  # --- some extra logic in 10% of cases we set check if newmean is mean
  # --- and we skip changing of values.
  if (runif(1) < 0.1) {
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) == mean_value) { # New mean is GRIM-consistent, so we will keep it.
      doInc <- FALSE
    }
  }

  if (doInc) {
    change_value_in_vec(decFirst, vec, delta, whichWillInc, whichWillDec)
  }
  vec
}


increment <- function(vec, ind_vec, delta) {
  vec[whichWillInc] <- vec[whichWillInc] + delta
  vec
}

change_value_in_vec <- function(decFirst, vec, delta, whichWillInc, whichWillDec) {
  if (decFirst) {
    vec <- increment(vec, whichWillInc, delta)
  }else {
    vec <- increment(vec, whichWillDec, -delta)
  }
  vec
}


#' Get sprite samples
#'
#' Create samples that match the mean and sd.
#' @param max_cases maximum cases
#' @param n total cases
#' @param mean mean value
#' @param sd standard deviation
#' @param scale_min minimum of scale
#' @param scale_max maximum of scale
#' @param dp degrees of precision
#' @param fixed a vector of values that you know are in the sprite sample
#' @export
get_samples <- function(max_cases, n, mean, sd, scale_min, scale_max, dp = 2, fixed = c()) {
  # Check mean is possible with GRIM; if not, identify the nearest valid mean.
  mean_new <- make_mean_grim(mean, n, dp)
  # Determine minimum and maximum SDs.
  sd_limits <- determine_sd_limits(mean_new, n, scale_min, scale_max, dp)
  if(max_cases <1)stop("max_cases needs to be a positive integer")
  ## -- here pre allocate
  looptotal <- max_cases * 8 # 8 is arbitrary; break early if we find enough unique cases.
  results <- matrix(0L, nrow = looptotal,ncol = n)
  for (i in 1:looptotal) {
    vec <- seek_vector(mean_new, sd, n, scale_min, scale_max, dp, fixed)
    # If no solution was found on this run, skip
    if (length(vec) == 0) {
      next
    }
    full_vec <- sort(c(vec, fixed))
    results[i,] <- as.integer(full_vec)
    # break early if n results is reached
    if (n_unique_rows(results)==max_cases) {
      break
    }
  }
  ## return error if no results
  if (sum(non_zero_rows(results))==0) stop("No results were found")
  # return matrix with only unique results. Don't simplify so we always return a matrix.
  unique_rows_found <- n_unique_rows(results)
  if(unique_rows_found < max_cases){
    s = paste0(max_cases," requested but only ", unique_rows_found, " found.")
    warning(s)
  }
  results[unique_rows(results),,drop = FALSE]
}

# convenience functions to clearly show intent
#' Count unique results
#' @return number.
n_unique_rows<- function(matrix){
  sum(unique_rows(matrix))
}
#' All unique columns
#' combine non zero and not duplicate cols
#' @return a logical vector indicating unique columns in a matrix
unique_rows <- function(matrix){
  non_zero_rows(matrix) & not_duplicate_rows(matrix)
}
#' Find non duplicate rows in a matrix
#' @return logical vector of rows that are not duplicates
not_duplicate_rows <- function(matrix){
  !duplicated.matrix(matrix,MARGIN = 1)
}
#' Find rows that are not zero
#' Which we began with.
#' @return logical vector of rows that are not zero
non_zero_rows <- function(matrix){
  rowSums(matrix) != 0L
}
