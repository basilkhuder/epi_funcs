#' Calculates rate ratios (using standardized rates) and associated confidence intervals (using population or death amounts and assuming a poisson distribution.) 
#' @param rates A numeric vector of rates or the name of data.frame columns that contain rates
#' @param pop A numeric vector of population or death amounts or the name of data frame columns that contain
#' populations/deaths
#' @param df If rates and population/deaths are given as column names, the name of the data frame must be 
#' specified
#' @param rr_digits The number of digits the rate ratio should be rounded to
#' @param digits The number of digits the confidence intervals should be rounded to 
#' @param return_df If you'd like the results returned as a data frame or not. If the inputs are numeric
#' vectors, return_df will return a 1 by 2 data frame. If inputs are data frame columns, return_df will
#' return the input data frame with two new columns for rates and CIs added. 
#' @param conf The confidence interval you'd like
#' @return A character vector or data frame with rates and confidence intervals
#' @examples rate_ratio(rates = c(150, 100), pop = c(1000,5000), digits = 3, conf = "95")
#' @examples rate_ratio(df = rate_ratio_df, rates = c("rate_column_1", "rate_column_2"), pop = c("pop_column_1", "pop_column_2"),
#` conf = "95", return_df = TRUE, digits = 5) 
#' @export

rate_ratio <- function(rates,
                       pop,
                       digits,
                       conf = "95",
                       df,
                       return_df...) {
  
  conf_cv <- setNames(c(1.65, 1.96, 2.58), c("90", "95", "99"))
  df_type <- setNames(c("tibble","data.frame"), 
                      c("tbl_df", "data.frame"))
  UseMethod("rate_ratio")
}

#' @export
rate_ratio.list <- function(rates, 
                            pop, 
                            digits, 
                            conf,
                            rr_digits) {
  
  for (i in seq_along(rates[-1])) { 
    rr = round(rates[[i]] / rates[[1]], digits = rr_digits)
    se = sqrt(1 / pop[[i]] + 1 / pop[[1]])
    ul <- round(exp(log(rates[[i]]) + (conf_cv[[conf]] * se)), digits = digits)
    ll <- round(exp(log(rates[[i]]) - (conf_cv[[conf]] * se)), digits = digits)
    return(glue::glue("{rr} ({ul} to {ll})"))
  }
}

#' @export
rate_ratio.numeric <- function(rates,
                               pop,
                               digits = 5,
                               rr_digits = 3, 
                               conf = "95") {
  
  rates <- round(rates[[1]] / rates[[2]], digits = rr_digits)
  se <- sqrt(1 / pop[[1]] + 1 / pop[[2]])
  ul <- round(exp(log(rates) + (conf_cv[[conf]] * se)), digits = digits)
  ll <- round(exp(log(rates) - (conf_cv[[conf]] * se)), digits = digits)
  return(glue::glue("{rr} ({ul} to {ll})"))
  
} 

#' @export
rate_ratio.character <- function(rates,
                                 pop,
                                 df,
                                 digits = 5,
                                 rr_digits = 3,
                                 conf = "95",
                                 return_df = TRUE) {
  
  rates <- magrittr::extract(df, rates)
  rates <- round(rates[[1]] / rates[[2]], digits = rr_digits)
  pop <- magrittr::extract(df, pop)
  pop <- sqrt(1 / pop[[1]] + 1 / pop[[2]])
  ul <- round(exp(log(rates) + (conf_cv[[conf]] * pop)), digits = digits)
  ll <- round(exp(log(rates) - (conf_cv[[conf]] * pop)), digits = digits)
  if (isFALSE(return_df)) {
    df_cols <- '(Ratio = rates, CI = glue::glue("{ll} - {ul}"))'
    expr_call <- paste0(df_type[[class(df)[1]]], df_cols)
    df <- eval(rlang::parse_expr(expr_call))
  } else {
    df <- dplyr::mutate(df, Ratio = rates, CI = glue::glue("{ll} - {ul}"))
  }
  return(df)
}

rate_ratio.data.frame <- function(rates,
                                  pop,
                                  df,
                                  digits = 5,
                                  rr_digits,
                                  conf = "95",
                                  return_df = TRUE) { 
  
  #TODO
 
}

rate_ratio.tbl_df <- function(rates,
                              pop,
                              df,
                              digits = 5,
                              conf = "95",
                              return_df = TRUE) { 
 
  #TODO 
}
