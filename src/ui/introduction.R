introduction <- function() {
  div(style="margin-left:2%; margin-right: 2%",
    br(),
    fluidRow(
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