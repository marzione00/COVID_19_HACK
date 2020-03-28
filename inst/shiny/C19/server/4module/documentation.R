output$downloadDocument <- downloadHandler(
  filename <- function() {
    paste("documentation_29032020", "pdf", sep=".")
  },
  
  content <- function(file) {
    file.copy("documentation_29032020.pdf", file)
  },
  contentType = "application/pdf"
)