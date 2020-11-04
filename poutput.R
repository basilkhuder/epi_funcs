poutput <- function(df,
                    cap = "",
                    fs = 15,
                    cw = .75,
                    ch = .25,
                    output_type = NULL,
                    file_name = NULL,
                    font = NULL,
                    theme = "default") {
  pobj <- flextable::flextable(df)
  
  if (theme == "default") {
    pobj <- poutput.theme(pobj, "default")
  }
  
  if (length(cw) == 1) {
    pobj <- flextable::width(pobj, width = cw)
  } else {
    for (i in seq_along(cw)) {
      pobj <- flextable::width(pobj, j = i, width = cw[[i]])
    }
  }
  
  pobj <- flextable::fontsize(pobj, size = fs, part = "all")
  pobj <- flextable::set_caption(pobj, cap)
  
  if (!is.null(font)) {
    pobj <- flextable::font(pobj, fontname = font, part = "all")
  }
  
  if (is.null(output_type)) {
    return(pobj)
  }
  
  if (is.null(file_name)) {
    file_name <- gsub("-", "", Sys.Date())
    file_name <- paste0(file_name, "_table_output")
  } else {
    file_name <- stringr::str_extract(file_name, pattern = "^\\w*")
  }
  
  if (output_type == "png") {
    file_name <- paste0(file_name, ".png")
    flextable::save_as_image(pobj, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
    browseURL(file_name)
  }
  
  if (output_type == "pdf") {
    file_name <- paste0(file_name, ".pdf")
    flextable::save_as_image(pobj, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
    browseURL(file_name)
  }
  
  if (output_type == "word") {
    file_name <- paste0(file_name, ".docx")
    flextable::save_as_docx(pobj, path = file_name, webshot = "webshot2")
    print(glue::glue("File saved as {file_name}"))
    browseURL(file_name)
  }
  
}


poutput.theme <- function(pobj, theme) {
  if (theme == "default") {
    pobj <- flextable::bold(pobj, part = "header")
    pobj <- flextable::align(pobj, align = "center", part = "all")
    pobj <- flextable::font(pobj, fontname = "Arial", part = "all")
  }
  
  return(pobj)
  
}
