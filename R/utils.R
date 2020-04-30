
warn_sample_size_gt<- function(n, limit, test=NULL){
  n <- relative_integer(n)
  limit <- relative_integer(limit)
  if(n>=limit){
    end <- paste0(" works best with n<",limit)
    warning <- paste0(ifelse(is.null(test),"This test", test),end)
    warning(warning)
  }
}



extract_fraction <- function(number){
  number-floor(number)
}

relative_integer <- function(int){
  if(as.integer(int)!=int){stop(paste0(int, " is not an integer"))}
  as.integer(int)
}
