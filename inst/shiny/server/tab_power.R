


# Event Observers --------------------------------------

# Preset -> other select inputs
observeEvent(input$preset,{
  
  preset <- input$preset
  req(preset)
  
  if(preset=="custom"){
    return(NULL)
  }
  
  params <- preset_params[[preset]]
  
  updateSelectInput(session, "frequency",
                    selected=params[["frequency"]])
  
  updateSelectInput(session, "sources",
                    selected=params[["sources"]])
  
  updateSelectInput(session, "plot_type",
                    selected=params[["plot_type"]])
})


# Other select inputs -> preset
observe({
  frequency <- input$frequency
  sources <- input$sources
  plot_type <- input$plot_type
  
  preset <- isolate(input$preset)
  req(frequency, preset, sources, plot_type)
  
  params <- preset_params[[preset]]
  
  if(
    preset != "custom" &&
    (
      frequency != params[["frequency"]] ||
      sort(sources) != sort(params[["sources"]]) ||
      plot_type != params[["plot_type"]]
    )
  ){
    updateSelectInput(session, "preset",
                      selected="custom")
  }
})
 
# observe({
#   # Remove plotly parameters
#   url <- input$.shinyURL
#   req(url)
#   url_new <- gsub("&plotly[^&]*","", url)
#   
#   if(url != url_new){
#     updateTextInput(session, ".shinyURL", value=url_new)
#   }
# })


# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$downloadCsv <- downloadHandler(
  filename = function() {
    paste("power.csv", sep = "")
  },
  content = function(file) {
    write.csv(power(), file, row.names = FALSE)
  }
)

output$buttonClip <- renderUI({
  rclipButton("clipbtn", " Copy URL", input$.shinyURL, icon("copy"))
})


# Output Elements --------------------------------------
output$selectPreset <- renderUI({
  selectInput("preset", NULL,
              multiple=F,
              choices=presets,
              selected="custom")
})


output$selectCountry <- renderUI({
  selectInput("country", NULL,
              multiple=F,
              choices=countries,
              selected="EU")
})


output$selectSources <- renderUI({
  pickerInput("sources", "Source",
              choices=rev(sources),
              options =list(`actions-box` = TRUE),
              multiple = T,
              selected = sources)
  
})


output$selectFrequency <- renderUI({
  selectInput("frequency", "Frequency", multiple=F, choices = frequency, selected="week")
})


output$selectPlotType <- renderUI({
  selectInput("plot_type", "Plot Type", multiple=F, choices = plot_types, selected="area")
})

output$selectYearFrom <- renderUI({
  selectInput("year_from", "From", multiple=F,
              choices = seq(2016, lubridate::year(lubridate::today())), selected="2018")
})
 
output$selectYearTo <- renderUI({
  selectInput("year_to", "To", multiple=F,
              choices = seq(2016, lubridate::year(lubridate::today())), selected="2021")
})
# output$selectYears <- renderUI({
#   sliderTextInput("years", "Years",
#     choices = seq(2016, lubridate::year(lubridate::today())),
#     selected = c(2020, 2021)
#   )
# })


# Reactive Elements --------------------------------------

power_raw <- reactive({
  
  # To trigger refresh
  # input$power_refresh
  country <- input$country
  year_from <- input$year_from
  year_to <- input$year_to
  req(country, year_from, year_to)
    
  # Get data
  print("Getting power data")
  power <- creapower::get_generation(
    date_from=sprintf("%s-01-01", year_from),
    date_to=sprintf("%s-12-31", year_to),
    iso2 = country,
    homogenise = T,
    freq = "day"
  )
  print("Done")
  return(power)
})


power <- reactive({
  
  power_raw <- power_raw()
  frequency <- input$frequency
  sources <- input$sources
  req(power_raw, frequency, sources)
  
  print("Processing power data")
  power <- power_raw %>%
    filter(source %in% sources) %>%
    mutate(date=lubridate::floor_date(date, unit=frequency)) %>%
    group_by(across(c(-output_mw))) %>%
    summarise_at("output_mw", mean) %>%
    ungroup()
  print("Done")
  
  return(power)
  
})


caption <- reactive({
  power <- power()
  req(power)
  
  ds <- unique(power$data_source)
  ref <- paste0("Source: ", data_source_reference(ds),". ")
  update <- paste0("Last updated on ", strftime(max(power$date), "%d %B %Y."))
  return(paste0(ref, update))
})


output$power_plot <- renderPlotly({

  plot_type <- input$plot_type
  sources <- input$sources
  power <- power()
  caption <- caption()
  frequency <- isolate(input$frequency)

  req(power, plot_type, sources, caption, frequency)
  
  power_sources <- power %>% filter(source %in% sources) %>%
    group_by(date, data_source, iso2, region) %>%
    mutate(output_pct = output_mw / sum(output_mw)) %>%
    ungroup()

  if(plot_type=="lines"){
    plt <- plot_ly(power_sources,
            x = ~date,
            y = ~output_mw,
            color = ~source,
            customdata=~source,
            colors=creapower::palette_power(),
            type = "scatter",
            mode="lines+marker",
            hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
            showlegend = T) %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = 'Power generation (MW)'),
        xaxis = list(title = ''))
  }
  
  if(plot_type=="area"){
    plt <- plot_ly(power_sources,
            x = ~date,
            y = ~output_mw,
            color = ~source,
            customdata = ~source,
            colors = creapower::palette_power(),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 0),
            alpha = 0.9,
            stackgroup = 'one',
            hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
            showlegend = T) %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = 'Power generation (MW)'),
        xaxis = list(title = ''))
  }
  
  if(plot_type=="area_pct"){
    plt <- plot_ly(power_sources,
            x = ~date,
            y = ~output_pct,
            color = ~factor(source),
            customdata = ~source,
            colors = creapower::palette_power(),
            type = 'scatter',
            mode = 'lines',
            line = list(width = 0),
            alpha = 0.9,
            stackgroup = 'one',
            hovertemplate = '%{customdata} %{y:.0%}<extra></extra>',
            showlegend = T) %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = 'Share of power generation',
                     tickformat = '.0%'),
        xaxis = list(title = ''))
  }
  
  if(plot_type=="bar"){
    
    power_deyeared <- power_sources %>%
      mutate(year=lubridate::year(date),
             date2000 = lubridate::`year<-`(date, 2000)) %>%
      group_by(iso2, region, date2000, year) %>%
      summarise(output_mw=mean(output_mw)) %>%
      ungroup()
    
    tickformat <- recode(frequency,
                         "day"="%e %b",
                         "week"= "%W",
                         "month"="%b",
                         "year"="%Y")
    
    dtick <- ifelse(frequency=="month", "M1", NA)
    
    plt <- plot_ly(power_deyeared,
                   x = ~date2000,
                   y = ~output_mw,
                   color = ~factor(year),
                   customdata = ~year,
                   colors = 'Reds',
                   type = 'bar',
                   alpha = 0.9,
                   hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
                   showlegend = T) %>%
             layout(
               hovermode = "x unified",
               yaxis = list(title = 'Power generation (MW)'),
               xaxis = list(title = '',
                            dtick = dtick,
                            tickformat=tickformat))
  }
  
  plt <- plt %>%
    layout(
      annotations = list(x = 1, y = 0, text = caption, 
         showarrow = F, xref='paper', yref='paper', 
         xanchor='right', yanchor='auto', xshift=0, yshift=-60,
         font=list(color="#AAAAAA")),
      margin = list(b=60),
      yaxis = list(fixedrange=T))
  
  return(plt)
})