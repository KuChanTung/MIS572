#' To convert factor variable into character variable
#' @description To convert factor variable into character variable.
#' @author Yihuang Kang
#' @param factorVar A factor variable to convert.
#' @examples factor2char(CO2$Type)

factor2char = function(factorVar)
{
  if(! is.factor(factorVar)){
    stop('factor2char(): The input variable, factorVar, is not a factor.')
  }
  return(levels(factorVar)[as.numeric(factorVar)]);
}
