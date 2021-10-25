tabPanel("Power generation",
         value="power",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             shinyjs::useShinyjs(),

             # Baseline
             h4("Options"),
             uiOutput("selectCountry"),
             uiOutput("selectSources"),
             uiOutput("selectFrequency"),
             uiOutput("selectPlotType"),
             uiOutput("selectYears"),
             
             
             actionButton("power_refresh",
                          "Refresh",
                          class="btn-primary"),

             downloadButton(outputId="download_csv",
                            "Download (.csv)",
                            class="btn-secondary")


           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             htmlOutput("power_message", class="hia-msg"),
             plotlyOutput("power_plot", height="calc(100vh - 50px)")  %>% withSpinner(color="#8cc9D0")
           )
         )
)
