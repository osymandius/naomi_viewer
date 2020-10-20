introduction_server <- function(input, output, session) {
  
  observeEvent(input$api_button, {
    
    adr_url <<- "https://dev.adr.fjelltopp.org/"
    my_adr_key <- input$api_key
    # my_adr_key <<- "c9acd056-2872-4b77-aa56-cfd1a8b34076"
    
    pkg_options <<- package_search(q = "type:naomi-data-package", as = "table",
                                  url = adr_url, key = my_adr_key)
    
    updateSelectInput(session, "package",
                      choices = pkg_options$results$title,
                      selected = head(pkg_options$results$title, 1)
    )
    
  })

  output$value <- renderText({ input$api_key })
  
  

}