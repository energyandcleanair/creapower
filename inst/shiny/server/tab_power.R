


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

# Only show relevant sources 
observe({
  power_raw <- power_raw()
  req(power_raw)
  
  # sources <- unique(power_raw$source)
  sources <- as.character(unique(power_raw$source))
  updatePickerInput(session, "sources",
                    choices=sources,
                    selected=sources)
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

output$selectRolling <- renderUI({
  radioButtons('rolling', 'Rolling Average', choices = c('1 day' = 1, '7 days' = 7, '14 days' = 14),
               selected = 1)
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
  country_input <- input$country
  year_from <- input$year_from
  year_to <- input$year_to
  req(country_input, year_from, year_to)
  
  # api_data has the last 4 years, get more if required 
  print("Getting power data")
  if(!year_from %in% downloaded_years){
    api_data_exp <- get_generation_api(date_from = sprintf("%s-01-01", year_from),
                                       date_to = sprintf("%s-01-01", min(downloaded_years))) %>%
      filter(!is.na(country)) # database has empty country and/or region
    api_data <<- api_data %>% bind_rows(api_data_exp) %>% distinct() # udpate global, prevent double counting
    downloaded_years <<- c(downloaded_years, (year_from):min(downloaded_years)) %>%
      unique()
  }
  
  power <- api_data %>%
    filter(date >= lubridate::date(sprintf("%s-01-01", year_from)),
           date < lubridate::date(sprintf("%s-01-01", as.numeric(year_to) + 1))) %>% 
    mutate(iso2 = countries[country]) %>%
    filter(if(country_input == 'EU') region == country_input else iso2 == country_input)
  
  # power <- creapower::get_generation(
  #   date_from=sprintf("%s-01-01", year_from),
  #   date_to=sprintf("%s-12-31", year_to),
  #   iso2 = country,
  #   # homogenise = T,
  #   freq = "day"
  # )
  print("Done")
  return(power)
})


power <- reactive({
  
  power_raw <- power_raw() %>% select(-iso2)
  frequency_input <- input$frequency
  sources <- input$sources
  req(power_raw, frequency, sources)
  country_input <- input$country
  
  
  print("Processing power data")
  power <- power_raw %>%
    filter(source %in% sources) %>%
    mutate(date=lubridate::floor_date(date, unit=frequency_input)) %>%
    group_by(across(-c(value_mw, value_mwh))) %>%
    summarise_at("value_mw", mean) %>%
    ungroup()
  
  if(country_input == 'EU'){
    power <- power %>% 
      group_by(across(-c(country, value_mw))) %>%
      summarise_at('value_mw', sum) %>%
      ungroup() %>%
      mutate(country = 'EU')
  }
  
  print("Done")
  
  return(power)
})


caption <- reactive({
  power <- power_raw()
  req(power)
  
  ds <- unique(power$data_source)
  ref <- paste0("Source: ", data_source_reference(ds),". ")
  update <- paste0("Last updated on ", strftime(max(lubridate::date(power$date), na.rm=T), "%d %B %Y."))
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
    group_by(date, data_source, country, region) %>%
    mutate(output_pct = value_mw / sum(value_mw)) %>%
    ungroup()
  
  
  if(plot_type=="lines"){
    if(input$rolling != 1 && frequency == "day"){
      power_sources <- power_sources %>%
        arrange(date) %>%
        group_by(source, data_source, country, region) %>%
        mutate(value_mw = zoo::rollmean(x = value_mw, as.numeric(input$rolling), fill = NA)) %>%
        ungroup()
    }
    
    plt <- plot_ly(power_sources,
                   x = ~date,
                   y = ~value_mw,
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
                   y = ~value_mw,
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
      group_by(country, region, date2000, year) %>%
      summarise(value_mw=sum(value_mw)) %>%
      ungroup()
    # browse()
    
    tickformat <- recode(frequency,
                         "day"="%e %b",
                         "week"= "%W",
                         "month"="%b",
                         "year"="%Y")
    # print(frequency)
    dtick <- ifelse(frequency=="month", "M1", NA)
    
    plt <- plot_ly(power_deyeared,
                   x = ~date2000,
                   y = ~value_mw,
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