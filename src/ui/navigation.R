navigationPanel <- function() {
  navlistPanel(HTML("<b>Naomi viewer</b>"),
    widths = c(2, 10), well = FALSE,
    tabPanel("Introduction", introduction()),
    tabPanel("Visualise data", viewer())
    # tabPanel("About", about())
  )
}
                                
