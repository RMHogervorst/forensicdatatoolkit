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

#' Get sprite samples data.frame
#'
#' Create samples that match the mean and sd.
#' @param max_cases maximum cases
#' @param n total cases
#' @param mean mean value
#' @param sd standard deviation
#' @param scale_min minimum of scale
#' @param scale_max maximum of scale
#' @param dp degrees of precision, places behind the comma
#' @param fixed a vector of values that you know are in the sprite sample
#' @details ## Advanced use
#' It is also possible to use the underlying functions directly.
#' - [seek_sprite_vector()][forensicdatatoolkit::seek_sprite_vector()] if you need a single vector only
#' - [get_sprite_samples_matrix()][forensicdatatoolkit::get_sprite_samples_matrix] if you want to work with a matrix of vectors
#' -  if you would like to turn the matrix into a dataframe use the [sprite_into_df()][forensicdatatoolkit::sprite_into_df()] function
#'
#' @export
#' @family sprite
#' @examples
#' sprite_samples(10, 10, 2.5, 2, 1, 5)
#' sprite_samples(2, 20, 2.5, 2, 1, 5, fixed = c(1, 1))
sprite_samples <- function(max_cases, n, mean, sd, scale_min, scale_max, dp = 2, fixed = c()) {
  matrix_ <- get_sprite_samples_matrix(max_cases = max_cases, n = n, mean = mean, sd = sd, scale_min = scale_min, scale_max = scale_max, dp = dp, fixed = fixed)
  sprite_into_df(matrix_)
}



#' Find single vector of mean and sd.
#'
#' Return a vector of length n with an average close to mean and
#' standard deviation close to sd. In most cases you want more than one
#' sample, to see what are feasable solutions see [sprite_samples()][forensicdatatoolkit::sprite_samples()]
#' @param n total cases
#' @param mean mean value
#' @param sd standard deviation
#' @param scale_min minimum of scale
#' @param scale_max maximum of scale
#' @param dp degrees of precision, places behind the comma
#' @param fixed a vector of values that you know are in the sprite sample
#' @family sprite
#' @export
seek_sprite_vector <- function(mean, sd, n, scale_min, scale_max, dp = 2, fixed = c()) {
  rN <- n - length(fixed)
  vec <- create_init_vec(rN, mean, scale_max, scale_min)
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
#' @family sprite
#' @export
get_sprite_samples_matrix <- function(max_cases, n, mean, sd, scale_min, scale_max, dp = 2, fixed = c()) {
  # TODO: protect against missing values in max_cases, n, mean, etc.
  if (scale_max < scale_min) {
    stop("Scale max cannot be smaller than scale min")
  }
  # Check mean is possible with GRIM; if not, identify the nearest valid mean.
  mean_new <- make_mean_grim(mean, n, dp)
  # Determine minimum and maximum SDs.
  sd_limits <- determine_sd_limits(mean_new, n, scale_min, scale_max, dp)
  if (max_cases < 1) stop("max_cases needs to be a positive integer")
  ## -- here pre allocate
  looptotal <- max_cases * 8 # 8 is arbitrary; break early if we find enough unique cases.
  results <- matrix(0L, nrow = looptotal, ncol = n)
  for (i in 1:looptotal) {
    vec <- seek_sprite_vector(mean_new, sd, n, scale_min, scale_max, dp, fixed)
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
    bumpMean <- canBumpMean[as.integer(stats::runif(1) * length(canBumpMean)) + 1] # select a changeable number
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
      candidates <- not_decrease_delta & non_duplicates
    }
    if (sum(candidates) == 0) {
      candidates <- non_duplicates
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
      candidates <- not_decrease & not_decrease_delta & non_duplicates
    }
    if (sum(candidates) == 0) {
      candidates <- not_decrease_delta & non_duplicates
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
  mean_check <- stats::runif(1) < 0.1
  decFirst <- stats::runif(1) < 0.5
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
      vec <- increment(vec, picks$decr, -delta)
    }
  } else {
    stop("Error, something happened during execution phase of sprite delta")
  }
  vec
}
