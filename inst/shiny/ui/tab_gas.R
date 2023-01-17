tabPanel("Fossil Gas",
         value="gas",
         
         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             shinyjs::useShinyjs(),
             
             # Baseline
             h4("Region"),
             uiOutput("selectCountry_gas"),
             
             # h4("Presets"),
             # uiOutput("selectPreset"),
             
             h4("Customise"),
             uiOutput("selectDataType_gas"),
             uiOutput("selectFrequency_gas"),
             uiOutput("selectPlotType_gas"),
             # uiOutput('selectRolling'),
             
             conditionalPanel(paste('(input.plot_type_gas == "lines" ||', # javascript condition
                                    'input.plot_type_gas == "lines_yearly" ||',
                                    'input.plot_type_gas == "area" ||',
                                    'input.plot_type_gas == "area_pct")', 
                                    '&& input.frequency_gas == "day"'),
                              uiOutput("selectRolling_gas")),
             
             div(
               class="row-inline",
               height=50,
               uiOutput("selectYearFrom_gas"),
               uiOutput("selectYearTo_gas")
             ),
             
             h4("Download"),
             downloadButton(outputId="downloadCsv_gas",
                            "Download (.csv)",
                            class="btn-secondary"),
             h4("Share"),
             shinyURL.ui(label = NULL, copyURL=F, tinyURL=F),
             uiOutput("buttonClip_gas")
           ),
           
           mainPanel(
             width=10,
             htmlOutput("gas_message", class="gas-msg"),
             plotlyOutput("gas_plot", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
           )
         )
)
