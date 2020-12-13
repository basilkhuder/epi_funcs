pairmatch_chunks <- function(d, prev, form, controls = 1){ 
  if(nrow(d) == 0) { 
    return(prev)
  } 
  n <- try(optmatch::pairmatch(as.formula(form), controls = controls, data = d), 
           silent = TRUE)
  if(class(n)[1] == "optmatch") { 
    n <- tibble::enframe(n)
    d <- dplyr::mutate(d, matches = n$value)
    d <- tidyr::drop_na(d)
    result <- rbind(prev, d)
    return(result)
  } else {
    return(prev)
  }
} 

optmatch_chunk <- function(data, formula, controls = 1) { 
  data <- LaF::laf_open(LaF::detect_dm_csv(data, header = TRUE))
  return(LaF::process_blocks(data, fun = pairmatch_chunks, 
                        form =  formula,
                        controls = controls,
                        progress = TRUE))


} 
