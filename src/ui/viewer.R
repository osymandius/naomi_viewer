viewer <- function() {
  div(style="margin-left:2%; margin-right: 2%",
      h2("Visualise data"),
      br(),
      p(style="font-size:16px", "Here's where you can view stuff"),
      tags$ul(
        tags$li("ART count: Here's a description"),
        tags$li("ART sex ratio: Here's a description"),
        tags$li("ART paeds ratio: Here's a description"),
        tags$li("ANC count: Here's a description"),
        tags$li("ANC prevalence: Here's a description"),
        tags$li("ANC known positive: Here's a description"),
        tags$li("ANC ART coverage: Here's a description")
      ),
      br(),
      tabsetPanel(
        tabPanel(title="ART count",
                 "Some note about how the years are summed to end of Q4",
                 plotOutput("art_count_plot")
                 ),
        tabPanel(title="ART sex ratio",
                 textOutput("art_sex_plot_n"),
                 plotOutput("art_sex_plot")
        ),
        tabPanel(title="ART paeds ratio",
                 plotOutput("art_paeds_plot")
        ),
        tabPanel(title="ANC count",
                 plotOutput("anc_count_plot")
                 ),
        tabPanel(title="ANC prevalence",
                 plotOutput("anc_prev_plot")
        ),
        tabPanel(title="ANC known positive",
                 plotOutput("anc_known_plot")
        ),
        tabPanel(title="ANC ART coverage",
                 plotOutput("anc_art_plot")
        )
      )
  )
  
}