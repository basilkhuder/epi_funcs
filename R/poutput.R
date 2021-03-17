poutput <- function(df,
                    cap = "",
                    fs = 15,
                    cw = .75,
                    ch = .25,
                    fch = "black",
                    output_type = NULL,
                    file_name = NULL,
                    font = NULL,
                    theme = "default",
                    add_footnote = NULL,
                    add_header_bot = NULL,
                    extra_align = NULL,
                    #first param of extra_align is for type of alignment, second is for column number
                    #extra_align = list(c("left", "center"), c(1,2))
                    inner_border = NULL) {
  
  pobj <- flextable::flextable(df)
  
  if (!is.null(add_footnote)) {
    pobj <- flextable::footnote(
      pobj,
      i = 1,
      j = add_footnote[[1]],
      ref_symbols = add_footnote[[2]],
      value = flextable::as_paragraph(add_footnote[[3]]),
      part = "header"
    )
  }
  
  if (!is.null(add_header_bot)) {
    pobj <-
      flextable::add_header_row(pobj, top = FALSE, values = add_header_bot)
    theme <- "header_bot_default"
  }
  
  pobj <-
    poutput.theme(pobj,
                  theme = theme,
                  font = font,
                  add_footnote,
                  extra_align,
                  inner_border)
  
  if (length(cw) > ncol(df) | length(cw) < ncol(df)) {
    stop("Column width must equal the amount of columns or be a single number")
  } else if (length(cw) == 1) {
    pobj <- flextable::width(pobj, width = cw)
  } else {
    for (i in seq_along(cw)) {
      pobj <- flextable::width(pobj, j = i, width = cw[[i]])
    }
  }
  
  pobj <- flextable::fontsize(pobj, size = fs, part = "all")
  pobj <- flextable::color(pobj, color = fch, part = "header")
  pobj <- flextable::set_caption(pobj, cap)
  
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

poutput.theme <-
  function(pobj,
           theme,
           font,
           add_footnote,
           extra_align,
           inner_border) {
    if (is.null(font)) {
      font <- "Arial"
    }
    
    if (theme == "default") {
      pobj <- flextable::bold(pobj, part = "header")
      pobj <- flextable::align(pobj, align = "center", part = "all")
      pobj <- flextable::font(pobj, fontname = font, part = "all")
    }
    
    if (theme == "header_bot_default") {
      pobj <- flextable::border_remove(pobj)
      border <- officer::fp_border(color = "black", width = 2)
      pobj <- flextable::hline_top(pobj, border = border)
      pobj <-
        flextable::hline_top(pobj, border = border, part = "all")
      pobj <- flextable::bold(pobj, part = "header")
      pobj <- flextable::align(pobj, align = "center", part = "all")
      pobj <- flextable::font(pobj, fontname = font, part = "all")
    }
    
    if (!is.null(add_footnote)) {
      pobj <- flextable::align(pobj, align = "left", part = "footer")
    }
    
    if (!is.null(extra_align)) {
      for (i in seq_along(extra_align)) {
        pobj <-
          flextable::align(pobj, align = extra_align[[1]][[i]], j = extra_align[[2]][[i]])
      }
      
    }
    
    if (!is.null(inner_border)) {
      ib <- officer::fp_border(color = "gray", width = 1)
      pobj <-
        flextable::border_inner_h(pobj, border = ib, part = "body")
      
    }
    
    return(pobj)
    
  }
