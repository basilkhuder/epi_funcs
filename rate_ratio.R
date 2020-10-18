#' Calculates rate ratios (using standarized rates and population amounts) and associated confidence intervals
#' @param rates A numeric vector of rates or the name of data.frame columns that contain rates
#' @param pop A numeric vector of population amounts or the name of data frame columns that contain
#' populations
#' @param df If rates and population are given as column names, the name of the data frame must be 
#' specified
#' @param digits The number of digits the confidence intervals should be rounded to 
#' @param return_df If you'd like the results returned as a data frame or not. If the inputs are numeric
#' vectors, return_df will return a 1 by 2 data frame. If inputs are data frame columns, return_df will
#' return the input data frame with two new columns for rates and CIs added. 
#' @param conf The confidence interval you'd like
#' @return A character vector or data frame with rates and confidence intervals
#' @export
#'
#' @examples rate_ratio(rates = c(150, 100), pops = c(1000,5000), digits = 3, conf = "95")

rate_ratio <- function(rates,
                       pop,
                       digits,
                       conf = "95",
                       df,
                       return_df...) {
  UseMethod("rate_ratio")
}

#' @export
rate_ratio.numeric <- function(rates,
                               pop,
                               digits = 5,
                               conf = "95",
                               return_df = FALSE) {
  
  conf_cv <- dplyr::case_when(conf == "90" ~ 1.65,
                           conf == "95" ~ 1.96,
                           conf == "99" ~ 2.58)
  
  rates <- rates[[1]] / rates[[2]]
  se <- sqrt(1 / pop[[1]] + 1 / pop[[2]])
  ul <- round(exp(log(rates) + (conf_cv * se)), digits = digits)
  ll <- round(exp(log(rates) - (conf_cv * se)), digits = digits)
  return(glue::glue("Ratio: {rates} \n {conf}% CI: {ul} - {ll}"))
}

#' @export
rate_ratio.character <- function(rates,
                                 pop,
                                 df,
                                 digits = 5,
                                 conf = "95",
                                 return_df = TRUE) {
  
  if(class(df)[1] == "data.frame"){ 
    return(rate_ratio.data.frame(rates, pop, df, digits, conf, return_df))
  } else if(class(df)[1] == "tbl_df"){ 
    return(rate_ratio.tbl_df(rates, pop, df, digits, conf, return_df))
  } else { 
    stop("Df must be either a data frame or a tibble")
    }

}
  
rate_ratio.data.frame <- function(rates,
                                 pop,
                                 df,
                                 digits = 5,
                                 conf = "95",
                                 return_df = TRUE) { 

  conf_cv <- dplyr::case_when(conf == "90" ~ 1.65,
                           conf == "95" ~ 1.96,
                           conf == "99" ~ 2.58)
  
  rates <- magrittr::extract(df, rates)
  rates <- rates[[1]] / rates[[2]]
  pop <- magrittr::extract(df, pop)
  pop <- sqrt(1 / pop[[1]] + 1 / pop[[2]])
  ul <- round(exp(log(rates) + (conf_cv * pop)), digits = digits)
  ll <- round(exp(log(rates) - (conf_cv * pop)), digits = digits)
  if (isFALSE(return_df)) {
    return(data.frame(Ratio = rates, CI = glue::glue("{ll} - {ul}")))
  } else {
    df <- dplyr::mutate(df, Ratio = rates, CI = glue::glue("{ll} - {ul}"))
  }
  return(df)
}

rate_ratio.tbl_df <- function(rates,
                                  pop,
                                  df,
                                  digits = 5,
                                  conf = "95",
                                  return_df = TRUE) { 
  
  conf_cv <- dplyr::case_when(conf == "90" ~ 1.65,
                              conf == "95" ~ 1.96,
                              conf == "99" ~ 2.58)
  
  rates <- magrittr::extract(df, rates)
  rates <- rates[[1]] / rates[[2]]
  pop <- magrittr::extract(df, pop)
  pop <- sqrt(1 / pop[[1]] + 1 / pop[[2]])
  ul <- round(exp(log(rates) + (conf_cv * pop)), digits = digits)
  ll <- round(exp(log(rates) - (conf_cv * pop)), digits = digits)
  if (isFALSE(return_df)) {
    return(tibble::tibble(Ratio = rates, CI = glue::glue("{ll} - {ul}")))
  } else {
    df <- dplyr::mutate(df, Ratio = rates, CI = glue::glue("{ll} - {ul}"))
  }
  return(df)
}
