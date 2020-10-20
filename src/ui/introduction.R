introduction <- function() {
  div(style="margin-left:2%; margin-right: 2%",
    br(),
    fluidRow(
      # "c9acd056-2872-4b77-aa56-cfd1a8b34076",
      textInput(inputId = "api_key", label = "ADR API key"),
      actionButton(inputId = "api_button", label = "Submit"),
      textOutput("value")
    ),
    fluidRow(
      # uiOutput("package")
      selectInput("package", label="Select ADR package", choices = "No API key selected")
    )
  )
  
}