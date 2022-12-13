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
             # uiOutput('selectRolling'),
             conditionalPanel('input.plot_type == "lines" && input.frequency == "day"',
                              uiOutput("selectRolling")),
             
             div(
               class="row-inline",
               height=50,
               uiOutput("selectYearFrom"),
               uiOutput("selectYearTo")
             ),
             
             
             
             h4("Download"),
             downloadButton(outputId="downloadCsv",
                            "Download (.csv)",
                            class="btn-secondary"),
             h4("Share"),
             shinyURL.ui(label = NULL, copyURL=F, tinyURL=F),
             uiOutput("buttonClip")
           ),
           
           mainPanel(
             width=10,
             htmlOutput("power_message", class="hia-msg"),
             plotlyOutput("power_plot", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
           )
         )
)
