


# Event Observers --------------------------------------
observe({
  plot_type <- input$plot_type
  req(plot_type)

  to_disable_enable <- c("frequency")

  if(plot_type=="monthly_bar"){
    for(d in to_disable_enable) shinyjs::disable(d)
  }else{
    for(d in to_disable_enable) shinyjs::enable(d)
  }
})


observeEvent(input$dimension,{
  print(input$dimension)
})


# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$download_csv <- downloadHandler(
  filename = function() {
    paste("power.csv", sep = "")
  },
  content = function(file) {
    write.csv(power(), file, row.names = FALSE)
  }
)



# Output Elements --------------------------------------
output$selectCountry <- renderUI({
  selectInput("country", "Region",
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
  selectInput("frequency", "Frequency", multiple=F, choices = frequency, selected="day")
})


output$selectPlotType <- renderUI({
  selectInput("plot_type", "Plot Type", multiple=F, choices = plot_types, selected="ts")
})


output$selectYears <- renderUI({
  sliderTextInput("years", "Years",
    choices = seq(2016, lubridate::year(lubridate::today())),
    selected = c(2020, 2021)
  )
})


# Reactive Elements --------------------------------------


power <- reactive({
  
  # To trigger refresh
  input$power_refresh

  country <- isolate(input$country)
  sources <- isolate(input$sources)
  frequency <- isolate(input$frequency)
  years <- isolate(input$years)
  req(country, sources, frequency, years)
    
  # Get data
  power <- creapower::get_generation(
    date_from=sprintf("%d-01-01", years[1]),
    date_to=sprintf("%d-12-31", years[2]),
    iso2 = country,
    homogenise = T,
    freq = frequency
  )
  
  return(power)
  
})


output$power_plot <- renderPlotly({

  plot_type <- input$plot_type
  sources <- input$sources
  power <- power()

  req(power, plot_type, sources)
  
  power_sources <- power %>% filter(source %in% sources) %>%
    group_by(date, data_source, iso2, region) %>%
    mutate(output_pct = output_mw / sum(output_mw)) %>%
    ungroup()

  if(plot_type=="lines"){
    return(plot_ly(power_sources,
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
        xaxis = list(title = '')
      ))
  }
  
  if(plot_type=="area"){
    return(plot_ly(power_sources,
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
        xaxis = list(title = '')
      ))
  }
  
  if(plot_type=="area_pct"){
    return(plot_ly(power_sources,
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
        xaxis = list(title = '')
      ))
  }
  
  if(plot_type=="monthly_bar"){
    
    power_month <- power_sources %>%
      mutate(year=lubridate::year(date),
             month0000 = lubridate::floor_date(lubridate::`year<-`(date,0), "month")) %>%
      group_by(iso2, region, month0000, year) %>%
      summarise(output_mw=mean(output_mw))
    
    return(plot_ly(power_month,
                   x = ~month0000,
                   y = ~output_mw,
                   color = ~factor(year),
                   customdata = ~year,
                   colors = 'Reds',
                   type = 'bar',
                   alpha = 0.9,
                   hovertemplate = '%{year} %{y:,.0f} MW<extra></extra>',
                   showlegend = T) %>%
             layout(
               hovermode = "x unified",
               yaxis = list(title = 'Power generation (MW)'),
               xaxis = list(title = '',
                            dtick = "M1", tickformat="%b")
             ))
  }
})