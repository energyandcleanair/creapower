tabPanel("Power generation",
         value="power",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             shinyjs::useShinyjs(),

             # Baseline
             h4("Region"),
             uiOutput("selectCountry"),
             
             h4("Presets"),
             uiOutput("selectPreset"),
             
             h4("Customise"),
             uiOutput("selectSources"),
             uiOutput("selectFrequency"),
             uiOutput("selectPlotType"),
             uiOutput("selectYears"),
             
             h4("Download"),
             # actionButton("power_refresh",
             #              "Refresh",
             #              class="btn-primary"),

             downloadButton(outputId="download_csv",
                            "Download (.csv)",
                            class="btn-secondary")
           ),
           
           mainPanel(
             width=10,
             htmlOutput("power_message", class="hia-msg"),
             plotlyOutput("power_plot", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
           )
         )
)
