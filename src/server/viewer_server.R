viewer_server <- function(input, output) {
  
  nrow_art <- reactive(
    ceiling(length(unique(art$area_id))/6)
  )
  
  plot_height_art <- reactive(200*nrow_art())


  output$art_plot <- renderPlot({
    
    ggplot(art, aes(calendar_quarter, art_total, group = 1)) +
      geom_line() +
      geom_point() +
      facet_wrap(~area_label, ncol = 6, scales = "free_y") +
      scale_y_continuous(labels = scales::label_number()) +
      expand_limits(y = 0) +
      theme_minimal() +
      labs(title = "Total receiving ART", x = element_blank(), y = element_blank()) +
      theme(strip.text = element_text(face = "bold", size=13),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1))
      
  }, height = reactive(plot_height_art()))
  
}