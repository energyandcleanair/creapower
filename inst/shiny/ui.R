library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg",
                  height=44)),
    windowTitle="CREA",
    theme = "theme.css",
    id = "nav-page",
    source(file.path("ui", "tab_power.R"),  local = TRUE)$value,
    # source(file.path("ui", "tab_methodology.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_about.R"),  local = TRUE)$value,
    
    # Responsive height of main plot
    tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        '))
    
    # tags$head(includeHTML(("www/gtm-head.html"))),
    # tags$body(includeHTML(("www/gtm-body.html")))
)


