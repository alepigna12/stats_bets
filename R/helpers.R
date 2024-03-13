to_fraction <- function(x) {
  frac <- MASS::as.fractions(x)
  if (x%%1==0) {
    return(paste(x, "/1", sep = ""))
  } else {
    return(frac)
  }
}

is_prob <- function(x){
  return(is.numeric(x) & x>=0 & x<=1)
}

