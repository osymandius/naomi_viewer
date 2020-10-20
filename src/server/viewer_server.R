viewer_server <- function(input, output, session) {
  
  art <- reactive({
  
    pkg_selected <- pkg_options$results %>%
      filter(title == input$package)
    
    pkg_selected <- pkg_options$results %>%
      filter(title == "Malawi Naomi Inputs and Outputs 2020")
    
    pkg_resources <- pkg_selected$resources[[1]]
    
    areas_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-geographic"]
    # anc_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-anc"]
    art_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-art"]
    
    areas_file <- ckan_fetch(areas_url, store = "disk", key = my_adr_key,
                             path = tempfile(fileext = ".geojson"))
    # anc_file <- ckan_fetch(anc_url, store = "disk", key = my_adr_key,
    #                        path = tempfile(fileext = ".csv"))
    art_file <- ckan_fetch(art_url, store = "disk", key = my_adr_key,
                           path = tempfile(fileext = ".csv"))
    
    areas <- naomi::read_area_merged(areas_file$path)
    # anc_raw <- naomi::read_anc_testing(anc_file$path)
    art_raw <- naomi::read_art_number(art_file$path)
    
    areas_merge <- areas %>%
      sf::st_drop_geometry() %>%
      dplyr::select(area_id, area_name, area_sort_order) %>%
      dplyr::mutate(
        area_label = paste0(area_name, "\n(", area_id, ")") %>%
          forcats::fct_reorder(area_sort_order)
      )
    
    art <- art_raw %>%
      left_join(areas_merge, by = "area_id") %>%
      group_by(area_id, area_label, calendar_quarter) %>%
      summarise(
        art_total = sum(current_art, na.rm = TRUE),
        art_adult_f = sum(current_art * as.integer(sex == "female" & age_group == "15+"), na.rm = TRUE),
        art_adult_m = sum(current_art * as.integer(sex == "male" & age_group == "15+"), na.rm = TRUE),
        art_adult = sum(current_art * as.integer(age_group == "15+"), na.rm = TRUE),
        art_child = sum(current_art * as.integer(age_group == "00-14"), na.rm = TRUE),
        art_adult_sex_ratio = art_adult_f / art_adult_m,
        art_adult_child_ratio = art_adult / art_child,
        .groups = "drop"
      )

  })

  anc <- reactive({

    pkg_selected <- pkg_options$results %>%
      filter(title == input$package)

    # pkg_selected <- pkg_options$results %>%
    #   filter(title == "Malawi Naomi Inputs and Outputs 2020")

    pkg_resources <- pkg_selected$resources[[1]]

    areas_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-geographic"]
    anc_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-anc"]

    areas_file <- ckan_fetch(areas_url, store = "disk", key = my_adr_key,
                             path = tempfile(fileext = ".geojson"))
    anc_file <- ckan_fetch(anc_url, store = "disk", key = my_adr_key,
                           path = tempfile(fileext = ".csv"))

    areas <- naomi::read_area_merged(areas_file$path)
    anc_raw <- naomi::read_anc_testing(anc_file$path)

    areas_merge <- areas %>%
      sf::st_drop_geometry() %>%
      dplyr::select(area_id, area_name, area_sort_order) %>%
      dplyr::mutate(
        area_label = paste0(area_name, "\n(", area_id, ")") %>%
          forcats::fct_reorder(area_sort_order)
      )

    anc <- anc_raw %>%
      left_join(areas_merge, by = "area_id") %>%
      mutate(
        area_label = paste0(area_name, "\n(", area_id, ")") %>%
          fct_reorder(area_sort_order),
        anc_total_pos = ancrt_known_pos + ancrt_test_pos,
        anc_status = ancrt_known_pos + ancrt_tested,
        anc_prevalence = anc_total_pos / anc_status,
        anc_art_among_known = ancrt_already_art / ancrt_known_pos,
        anc_art_coverage = ancrt_already_art / anc_total_pos
      )

  })


  plot_height <- reactive({
      200*ceiling(length(unique(art()$area_id))/6)
  })


  output$art_count_plot <- renderPlot({

    art() %>%
      ggplot(aes(year_labels(calendar_quarter_to_quarter_id(art()$calendar_quarter)), art_total, group = 1)) +
        geom_line() +
        geom_point() +
        facet_rep_wrap(~area_label, ncol = 6, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
        scale_y_continuous(labels = scales::label_number()) +
        expand_limits(y = 0) +
        theme_minimal() +
        labs(title = "Total receiving ART", x = element_blank(), y = element_blank()) +
        theme(strip.text = element_text(face = "bold", size=13),
              plot.title = element_text(size=16),
              axis.text = element_text(size = 12)
        )

    }, height = reactive(plot_height())
  )
  
  output$art_sex_plot <- renderPlot({
    if(nrow(art() %>% filter(!is.nan(art_adult_sex_ratio)))) {
      
      art() %>%
        filter(!is.nan(art_adult_sex_ratio)) %>%
        ggplot(aes(year_labels(calendar_quarter_to_quarter_id(art()$calendar_quarter)), art_adult_sex_ratio, group = 1)) +
          geom_line() +
          geom_point() +
          facet_rep_wrap(~area_label, ncol = 6, repeat.tick.labels = c("left", "bottom")) +
          scale_y_continuous(labels = scales::label_number()) +
          theme_minimal() +
          labs(title = "Ratio of females-to-males among adults on ART", x = NULL, y = NULL) +
        theme(strip.text = element_text(face = "bold", size=13),
              plot.title = element_text(size=16),
              axis.text = element_text(size = 12)
        )
    }
    
  }, height = reactive(plot_height())
  )
  
  output$art_sex_plot_n <- renderText({
    
    paste0("Districts with sex disaggregated data: ",
           length(art() %>%
              filter(!is.nan(art_adult_sex_ratio)) %>%
              .$area_id %>%
              unique),
           " of ", 
           length(art() %>%
                    .$area_id %>%
                    unique
           )
    )
  })
  output$art_paeds_plot <- renderPlot({
    
    ggplot(art(), aes(year_labels(calendar_quarter_to_quarter_id(art()$calendar_quarter)), art_adult_child_ratio, group = 1)) +
      geom_line() +
      geom_point() +
      facet_rep_wrap(~area_label, ncol = 6, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = scales::label_number()) +
      theme_minimal() +
      labs(title = "Ratio of children-to-adult on ART", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )
    
  }, height = reactive(plot_height())
  )
  
  output$anc_count_plot <- renderPlot({
    
    ggplot(anc(), aes(year, anc_clients)) +
      geom_line() +
      geom_point() +
      facet_rep_wrap(~area_label, ncol = 6, scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_number()) +
      expand_limits(y = 0) +
      theme_minimal() +
      labs(title = "Number of ANC clients", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )
    
  }, height = reactive(plot_height())
  )
  
  output$anc_prev_plot <- renderPlot({
    
    ggplot(anc(), aes(year, anc_prevalence)) +
      geom_line() +
      geom_point() +
      facet_rep_wrap(~area_label, ncol = 6, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "HIV prevalence among ANC attendees", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )
    
  }, height = reactive(plot_height())
  )
  
  output$anc_known_plot <- renderPlot({
    
    ggplot(anc(), aes(year, anc_art_among_known)) +
      geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
      geom_line() +
      geom_point() +
      facet_rep_wrap(~area_label, ncol = 6, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "Percentage of known positive already on ART", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )
    
  }, height = reactive(plot_height())
  )
  
  output$anc_art_plot <- renderPlot({
    
    ggplot(anc(), aes(year, anc_art_coverage)) +
      geom_hline(yintercept = 1.0, color = "grey30", linetype = "dashed") +
      geom_line() +
      geom_point() +
      facet_rep_wrap(~area_label, ncol = 6, repeat.tick.labels = c("left", "bottom")) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      labs(title = "ART coverage prior to first ANC visit", x = NULL, y = NULL) +
      theme(strip.text = element_text(face = "bold", size=13),
            plot.title = element_text(size=16),
            axis.text = element_text(size = 12)
      )
    
    }, height = reactive(plot_height())
  )
  
}