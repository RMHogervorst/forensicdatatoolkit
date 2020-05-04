test_that("check sd binary works", {
  size=30
  p = runif(size)
  n = as.integer(runif(size) * 40)+2
  means=rep(0, size)
  sds = rep(0, size)

  for (i in 1:size) {
    vec <- rbinom(n[i],1,p[i])
    mean <- mean(vec)
    sd <- sd(vec)
    message = paste0("m=",mean,"n=",n[i],"sd=",sd)
    expect_true(check_sd_binary(mean = mean, sd=sd,n=n[i], dp=3),label = message)
  }

})
