#'  @export
american <- function(odds){
  return(odds$american)
}

#'  @export
european <- function(odds){
  return(odds$european)
}

#'  @export
fractional <- function(odds){
  return(odds$fractional)
}

#'  @export
implied_prob <- function(odds){
  prob = 1/odds$european
  return(prob[[1]])
}

#' @export
no_vig_prob <- function(prob_home, prob_away){
  p_sum = prob_home+prob_away
  return(list(home = prob_home/p_sum, away=prob_away/p_sum))
}

