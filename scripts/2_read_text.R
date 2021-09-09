datasets[["input_text"]] <- paths[["input_text"]] %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = paths[["input_text"]])


create_html <- function(md) {
  htmltools::HTML(
    markdown::markdownToHTML(
      text = md, fragment.only = TRUE
    )
  )
}


text_output <- function(worksheet_name,
                        section_name,
                        subtitle = T) {
  
  text <- datasets[["input_text"]][[worksheet_name]] %>%
    filter(section == section_name)
  
  if (subtitle == T) {
    paste(sep = "\r\n\r\n",
          paste("###", pull(text, "subtitle")),
          pull(text, "body")) %>%
      create_html()
    
  } else if (subtitle == F) {
    paste(sep = "\r\n\r\n",
          pull(text, "body")) %>%
      create_html()
  }
}





