rate_ratio <- function(rates, pop, digits, conf = "95", ...){ 
  UseMethod("rate_ratio")
  }

rate_ratio.numeric <- function(rates, 
                               pops, 
                               digits = 5, 
                               conf = "95"){ 
  
  CONF <- dplyr::case_when(conf == "90" ~ 1.65,
            conf == "95" ~ 1.96,
            conf == "99" ~ 2.58)
  
  rates <- log(rates[[1]]/rates[[2]])
  se <- sqrt(1/pop[[1]] + 1/pop[[2]])
  ul <- round(exp(rates + (CONF * se)), digits = 5)
  ll <- round(exp(rates - (CONF * se)), digits = 5)
  return(glue::glue("{ul} - {ll}"))
}

rate_ratio.tbl_df <- function(df, 
                              rate_cols, 
                              pop_cols, 
                              digits = 5, 
                              conf = "95",
                              return_df = TRUE){ 
  
  CONF <- dplyr::case_when(conf == "90" ~ 1.65,
                           conf == "95" ~ 1.96,
                           conf == "99" ~ 2.58)
  
  rate_cols <- magrittr::extract(df, rate_cols) 
  rate_cols <- log(rate_cols[[1]] / rate_cols[[2]])
  pop_cols <- magrittr::extract(df, pop_cols)
  pop_cols <- sqrt(1/pop_cols[[1]] + 1/pop_cols[[2]])
  ul <- round(exp(rate_cols + (CONF * pop_cols)), digits = 5)
  ll <- round(exp(rate_cols - (CONF * pop_cols)), digits = 5)
  if(isFALSE(return_df)) {
    df <- (glue::glue("{ll} - {ul}"))
  } else { 
    df <- dplyr::mutate(df, CI = glue::glue("{ll} - {ul}"))
    }
  return(df)
} 

rate_ratio.data.frame <- function(df, 
                              rate_cols, 
                              pop_cols, 
                              digits = 5, 
                              conf = "95",
                              return_df = TRUE){ 
  
  CONF <- dplyr::case_when(conf == "90" ~ 1.65,
                           conf == "95" ~ 1.96,
                           conf == "99" ~ 2.58)
  
  rate_cols <- magrittr::extract(df, rate_cols) 
  rate_cols <- log(rate_cols[[1]] / rate_cols[[2]])
  pop_cols <- magrittr::extract(df, pop_cols)
  pop_cols <- sqrt(1/pop_cols[[1]] + 1/pop_cols[[2]])
  ul <- round(exp(rate_cols + (CONF * pop_cols)), digits = 5)
  ll <- round(exp(rate_cols - (CONF * pop_cols)), digits = 5)
  if(isFALSE(return_df)) {
    df <- (glue::glue("{ll} - {ul}"))
  } else { 
    df <- dplyr::mutate(df, CI = glue::glue("{ll} - {ul}"))
  }
  return(df)
} 
