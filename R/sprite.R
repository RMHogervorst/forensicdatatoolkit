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
  vec <- loop_until_sd_is_ok(vec, mean, sd, scale_min, scale_max, dp, fixed)
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

loop_until_sd_is_ok <- function(vec, required_mean, required_sd, scale_min, scale_max, dp, fixed = c(), max_rounds = 1000L) {
  n <- length(c(vec, fixed))
  gran <- set_granularity(dp)
  result <- rep(0L, length(vec))
  #  original notes: this maybe needs some more testing for pathological conditions
  maxLoops <- max(round(n * ((scale_max - scale_min)^2)), max_rounds)
  for (i in 1:maxLoops) {
    current_sd <- sd(c(vec, fixed))
    if (abs(current_sd - required_sd) <= gran) {
      result <- vec
      break
    } else {
      vec <- sprite_delta(vec, required_mean, required_sd, scale_min, scale_max, dp = dp, fixed = fixed)
    }
    if (i == maxLoops) {
      warning(
        paste0(
          "Maximum number of rounds reached for sd: ", maxLoops,
          " try upping the limit or check if the sd is possible."
        )
      )
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


dont_pick_exact_opposite <- function(vec, increasable, decreasing_pick, delta) {
  diff <- decreasing_pick - delta
  result <- increasable & (vec != diff)
  if (sum(result) == 0) {
    result <- increasable
  }
  result
}



#' select a random TRUE position out of logical vector
pick_one_position <- function(logical_vector) {
  if (sum(logical_vector) == 0) stop("There are no TRUE values in vector")
  if (class(logical_vector) != "logical") stop("Not a logical vector")
  pos <- which(logical_vector)
  pos[as.integer(runif(1) * length(pos)) + 1]
}


not_pick <- function(pick, n) {
  truths <- rep(TRUE, n)
  truths[pick] <- FALSE
  truths
}
#' Logic to determine the positions
#'
#' @return a list of 2 numbers, the indexes (positions)
#' of the item in the vector we want to increase and the
#' one we want to decrease.
#' @noRd
determine_positions <- function(increaseSD, delta, vec, fixed, scale_min, scale_max) {
  if (length(fixed) > 0) {
    notFixedDec <- !(vec %in% (fixed + delta))
    notFixedInc <- !(vec %in% (fixed - delta))
  } else {
    notFixedInc <- notFixedDec <- TRUE
  }
  len_vec <- length(vec)
  non_duplicates <- !duplicated(vec)
  decreasable <- non_duplicates & (vec > (scale_min + delta - 1)) & notFixedDec
  increasable <- non_duplicates & (vec < (scale_max - delta + 1)) & notFixedInc
  not_max_vec <- vec < max(vec)
  incr_decreasable <- (decreasable & not_max_vec)
  if (sum(incr_decreasable) == 0) {
    incr_decreasable <- decreasable
  }
  if (sum(decreasable) == 0 | sum(increasable) == 0) {
    warning("could not find positions")
    increaseSD <- NA
  }
  if (is.na(increaseSD)) {
    increasing_pick <- NA
    decreasing_pick <- NA
  } else if (increaseSD) {
    decreasing_pick <- pick_one_position(incr_decreasable)
    not_decrease <- not_pick(decreasing_pick, len_vec)
    not_decrease_delta <- dont_pick_exact_opposite(vec, increasable, decreasing_pick, delta = delta)
    candidates <- not_decrease & not_decrease_delta & non_duplicates
    if (sum(candidates) == 0) {
      candidates <- not_decrease & not_decrease_delta
    }
    if (sum(candidates) == 0) {
      candidates <- not_decrease
    }
    increasing_pick <- pick_one_position(candidates)
  } else if (!increaseSD) {
    # so decrease sd
    decreasing_pick <- pick_one_position(decreasable)
    not_decrease <- not_pick(decreasing_pick, len_vec)
    not_decrease_delta <- dont_pick_exact_opposite(vec, increasable, decreasing_pick, delta = delta)
    smaller_then_pick <- (vec < vec[decreasing_pick])
    candidates <- not_decrease & not_decrease_delta & smaller_then_pick & non_duplicates
    if (sum(candidates) == 0) {
      candidates <- not_decrease & not_decrease_delta
    }
    if (sum(candidates) == 0) {
      candidates <- not_decrease
    }
    increasing_pick <- pick_one_position(candidates)
  }
  pick <- list(incr = increasing_pick, decr = decreasing_pick)
  pick
}

# Original notes:
# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
sprite_delta <- function(vec, mean_value, sd, scale_min, scale_max, dp, fixed = c()) {
  # vec <- sort(vec) # possible speed improvement on large vectors. ?
  delta <- if ((length(vec[vec > (scale_min + 1)])) > 0) {
    step_size()
  } else {
    1
  }
  # Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < sd)
  picks <- determine_positions(
    increaseSD = increaseSD,
    delta = delta,
    vec = vec,
    fixed = fixed,
    scale_min = scale_min,
    scale_max = scale_max
  )
  ### Early stopping, if  no increasing candidates nor decreasing candidates stop.
  if (is.na(picks$incr) & is.na(picks$decr)) {
    return(vec) # Go back and try again.
  }

  # execute actions
  # random: check if mean is ok, if so break out
  mean_check <- runif(1) < 0.1
  decFirst <- runif(1) < 0.5
  if (!mean_check) {
    # ordering of actions doesn't matter
    vec <- increment(vec, picks$incr, delta)
    vec <- increment(vec, picks$decr, -delta)
  } else if (mean_check & decFirst) {
    # first decrement
    vec <- increment(vec, picks$decr, -delta)
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) != mean_value) { # New mean is GRIM-consistent, so we will keep it.
      vec <- increment(vec, picks$incr, delta)
    }
  } else if (mean_check & !decFirst) {
    # first increment
    vec <- increment(vec, picks$incr, delta)
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) != mean_value) { # New mean is GRIM-consistent, so we will keep it.
      vec <- increment(vec, picks$incr, delta)
    }
  } else {
    stop("Error, something happened during execution phase of sprite delta")
  }
  vec
}


increment <- function(vec, ind_vec, delta) {
  vec[ind_vec] <- vec[ind_vec] + delta
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
  # TODO: protect against scale_max < scale_min, missing values in max_cases, n, mean, etc.
  # Check mean is possible with GRIM; if not, identify the nearest valid mean.
  mean_new <- make_mean_grim(mean, n, dp)
  # Determine minimum and maximum SDs.
  sd_limits <- determine_sd_limits(mean_new, n, scale_min, scale_max, dp)
  if (max_cases < 1) stop("max_cases needs to be a positive integer")
  ## -- here pre allocate
  looptotal <- max_cases * 8 # 8 is arbitrary; break early if we find enough unique cases.
  results <- matrix(0L, nrow = looptotal, ncol = n)
  for (i in 1:looptotal) {
    vec <- seek_vector(mean_new, sd, n, scale_min, scale_max, dp, fixed)
    # If no solution was found on this run, skip
    if (length(vec) == 0) {
      next
    }
    full_vec <- sort(c(vec, fixed))
    results[i, ] <- as.integer(full_vec)
    # break early if n results is reached
    if (n_unique_rows(results) == max_cases) {
      break
    }
  }
  ## return error if no results
  if (sum(non_zero_rows(results)) == 0) stop("No results were found")
  # return matrix with only unique results. Don't simplify so we always return a matrix.
  unique_rows_found <- n_unique_rows(results)
  if (unique_rows_found < max_cases) {
    s <- paste0(max_cases, " requested but only ", unique_rows_found, " found.")
    warning(s)
  }
  results[unique_rows(results), , drop = FALSE]
}

# convenience functions to clearly show intent
#' Count unique results
#' @return number.
n_unique_rows <- function(matrix) {
  sum(unique_rows(matrix))
}
#' All unique columns
#' combine non zero and not duplicate cols
#' @return a logical vector indicating unique columns in a matrix
unique_rows <- function(matrix) {
  non_zero_rows(matrix) & not_duplicate_rows(matrix)
}
#' Find non duplicate rows in a matrix
#' @return logical vector of rows that are not duplicates
not_duplicate_rows <- function(matrix) {
  !duplicated.matrix(matrix, MARGIN = 1)
}
#' Find rows that are not zero
#' Which we began with.
#' @return logical vector of rows that are not zero
non_zero_rows <- function(matrix) {
  rowSums(matrix) != 0L
}
