output$downloadDocument <- downloadHandler(
  filename <- "documentation_29032020.pdf",
  
  
  content <- function(file) {
    file.copy("documentation_29032020.pdf", file)
  },
)