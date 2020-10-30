poutput <- function(df,
                    cap = "",
                    fs = 15,
                    cw = .75,
                    ch = .25,
                    font = "Arial",
                    output_type = NULL,
                    file_name = NULL) {
  
  df <- flextable::flextable(df)
  
  if(length(cw) == 1){
    df <- flextable::width(df, width = cw)
  } else { 
    for(i in seq_along(cw)){
      df <- flextable::width(df, j = i, width = cw[[i]])
    }
  }
  
  df <- flextable::bold(df, part = "header")
  df <- flextable::fontsize(df, size = fs, part = "all")
  df <- flextable::align(df, align = "center", part = "all")
  df <- flextable::set_caption(df, cap)
  df <- flextable::font(df, fontname = font, part = "all")
  
  if (is.null(output_type)) {
    return(df)
  }
  
  if (is.null(file_name)) {
    file_name <- gsub("-", "", Sys.Date())
    file_name <- paste0(file_name, "_table_output")
  } else {
    file_name <- stringr::str_extract(file_name, pattern = "^\\w*")
  }
  
  if (output_type == "png") {
    file_name <- paste0(file_name, ".png")
    flextable::save_as_image(df, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
  }
  
  if (output_type == "pdf") {
    file_name <- paste0(file_name, ".pdf")
    flextable::save_as_image(df, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
  }
  
  if (output_type == "word") {
    file_name <- paste0(file_name, ".docx")
    flextable::save_as_docx(df, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
  }
  
}
