#'  @export
create_odds_object <- function(input, type="american odds"){
  if(type %in% c("american odds", "moneyline", "american")){
    american = input
    if(input >= 100){
      european = (input + 100)/100
      }
    else if(input <= -100){
      european = (abs(input) + 100)/abs(input)
    }
    else{
      stop("Invalid american odds provided. Expecting value >= +100 or < -100")
    }
    potential_ROI = sprintf("%.2f%%", (european-1) * 100)
    fractional = to_fraction(european - 1)
    european = round(european, 2)
  }
  else if(type %in% c("european", "european odds", "decimal")){
    european = input
    if(input > 1){
      if(input > 2){
        american = (european - 1) * 100
      }
      else{
        american = -100/(european - 1)
      }
    }
    else{
      stop("Invalid european odds provided. Expecting value > 1")
    }
    potential_ROI = sprintf("%.2f%%", (european-1) * 100)
    fractional = to_fraction(european - 1)
    european = round(european, 2)
  }
  else if(type=="fractional"){
    numerator = as.numeric(unlist(strsplit("10/17","/"))[1])
    denominator = as.numeric(unlist(strsplit("10/17","/"))[2])
    european = 1+numerator/denominator
    potential_ROI = sprintf("%.2f%%", (european-1) * 100)
    fractional = to_fraction(european - 1)
    if(european > 2){
      american = (european - 1) * 100
    }
    else{
      american = -100/(european - 1)
    }
    european = round(european, 2)
  }
  odds = list(american=american, european=european, potential_ROI=potential_ROI, fractional=fractional)
  class(odds) = "odds"
  return(odds)
}


