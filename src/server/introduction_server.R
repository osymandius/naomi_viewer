introduction_server <- function(input, output, session) {
  
  observeEvent(input$api_button, {
    
    adr_url <- "https://dev.adr.fjelltopp.org/"
    my_adr_key <<- input$api_key
    
    pkg_options <<- package_search(q = "type:naomi-data-package", as = "table",
                                  url = adr_url, key = my_adr_key)
    
    updateSelectInput(session, "package",
                      # label = paste("Select input label", length(x)),
                      choices = pkg_options$results$title,
                      selected = head(pkg_options$results$title, 1)
    )
    
  })
  
  # output$package = renderUI({
  #   req(pkg_options)
  #     selectInput("package", 'Select package', pkg_options$results["title"])
  # })
  
  # observe({
  #   req(pkg_options)
  #     x <- pkg_options$results$title
  #     
  #     # Can also set the label and select items
  #     
  # })
    
  #'   #' List the package names and titles from which to select
  #'   pkg_options$results[c("id", "name", "title", "geo-location", "type_name",
  #'                         "private", "year", "isopen", "state")]
  #'   
  #'   
  #'   #' Choose the first one
  #'   pkg_selected <- pkg_options$results[4,]
  #'   
  #'   
  #'   #' View the resources in this package
  #'   pkg_resources <- pkg_selected$resources[[1]]
  #'   pkg_resources[c("resource_type", "schema", "format", "name",
  #'                   "description", "id", "url")]
  #'   
  #'   #' Get areas, ANC, and ART datasets
  #'   #'
  #'   #' NOTE: these will change if input package is "inputs-unaids-estimates"
  #'   #'
  #'   #' NOTE: ckan_fetch() doesn't return very useful message if the file isn't
  #'   #'   available to my API key. Need to pre-check that.
  #'   #' 
  #'   
  #'   areas_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-geographic"]
  #'   # anc_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-anc"]
  #'   art_url <- pkg_resources$url[pkg_resources$resource_type == "naomi-art"]
  #'   
  #'   areas_file <- ckan_fetch(areas_url[1], store = "disk", key = my_adr_key,
  #'                            path = tempfile(fileext = ".geojson"))
  #'   # anc_file <- ckan_fetch(anc_url[1], store = "disk", key = my_adr_key,
  #'                          # path = tempfile(fileext = ".csv"))
  #'   art_file <- ckan_fetch(art_url[1], store = "disk", key = my_adr_key,
  #'                          path = tempfile(fileext = ".csv"))
  #'   
  #'   areas <- naomi::read_area_merged(areas_file$path)
  #'   # anc_raw <- naomi::read_anc_testing(anc_file$path)
  #'   art_raw <- naomi::read_art_number(art_file$path)
  #'   
  #'   
  #'   #' ## Plot indicators
  #'   #'
  #'   #' * ART
  #'   #'   * Number on ART
  #'   #'   * Adult F:M ratio on ART
  #'   #'   * Adult:Child ratio on ART
  #'   #' * ANC
  #'   #'   * Number of ANC clients
  #'   #'   *
  #'   #'
  #'   #' TODO: Update / add to the number on ART and number ANC clients to be as a
  #'   #'   proportion of the district population.
  #'   #' 
  #'   
  #'   areas_merge <- areas %>%
  #'     sf::st_drop_geometry() %>%
  #'     dplyr::select(area_id, area_name, area_sort_order) %>%
  #'     dplyr::mutate(
  #'       area_label = paste0(area_name, "\n(", area_id, ")") %>%
  #'         forcats::fct_reorder(area_sort_order)
  #'     )
  #' 
  #' #' ### ART plots
  #' 
  #' art <<- art_raw %>%
  #'   left_join(areas_merge, by = "area_id") %>%
  #'   group_by(area_id, area_label, calendar_quarter) %>%
  #'   summarise(
  #'     art_total = sum(current_art, na.rm = TRUE),
  #'     art_adult_f = sum(current_art * as.integer(sex == "female" & age_group == "15+"), na.rm = TRUE),
  #'     art_adult_m = sum(current_art * as.integer(sex == "male" & age_group == "15+"), na.rm = TRUE),
  #'     art_adult = sum(current_art * as.integer(age_group == "15+"), na.rm = TRUE),
  #'     art_child = sum(current_art * as.integer(age_group == "00-14"), na.rm = TRUE),
  #'     art_adult_sex_ratio = art_adult_f / art_adult_m,
  #'     art_adult_child_ratio = art_adult / art_child,
  #'     .groups = "drop"
  #'   )
  #' 
  #' 
  #' })
  #' 
  output$value <- renderText({ input$api_key })
  
  

}