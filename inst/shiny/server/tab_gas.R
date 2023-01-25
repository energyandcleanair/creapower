


# Event Observers --------------------------------------

observeEvent(input$country_gas,{
  
  updateSelectInput(session, "data_type_gas",
                    choices = (if(input$country_gas == 'EU') data_types else data_types[1]),
                    selected = "fossil_gas")
})

observeEvent(input$frequency_gas,{
  
  updateSelectInput(session, "plot_type_gas",
                    choices = (if(input$frequency_gas == 'year') plot_types[3:5] else plot_types[3:4]),
                    selected = "lines")
})

# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$downloadCsv_gas <- downloadHandler(
  filename = function() {
    paste("gas.csv", sep = "")
  },
  content = function(file) {
    write.csv(gas(), file, row.names = FALSE)
  }
)

output$buttonClip_gas <- renderUI({
  rclipButton("clipbtn_gas", " Copy URL", clipText=input$.shinyURL, icon=icon("copy"))
})


# Output Elements --------------------------------------

output$selectCountry_gas <- renderUI({
  selectInput("country_gas", NULL,
              multiple=F,
              choices=countries_gas,
              selected="EU")
})

output$selectFrequency_gas <- renderUI({
  selectInput("frequency_gas", "Frequency", multiple=F, choices = frequency,
              selected="week")
})

output$selectPlotType_gas <- renderUI({
  selectInput("plot_type_gas", "Plot Type", multiple=F, choices = plot_types[3:4],
              selected="lines") # TODO
})

output$selectRolling_gas <- renderUI({
  selectInput('rolling_gas', 'Rolling Average',
              choices = c('1 day' = 1, '7 days' = 7, '14 days' = 14),
              selected = 1)
})

output$selectYearFrom_gas <- renderUI({
  selectInput("year_from_gas", "From", multiple=F,
              choices = seq(2022, lubridate::year(lubridate::today())),
              selected="2022") # TODO update if data before 2022 is available
})

output$selectYearTo_gas <- renderUI({
  selectInput("year_to_gas", "To", multiple=F,
              choices = seq(2022, lubridate::year(lubridate::today())),
              selected="2023")
})

output$selectDataType_gas <- renderUI({
  selectInput("data_type_gas", "Data Type", multiple=F, choices = data_types,
              selected="fossil_gas")
})


# Reactive Elements --------------------------------------

gas_raw <- reactive({
  
  country_input <- input$country_gas
  year_from <- input$year_from_gas
  year_to <- input$year_to_gas
  req(year_from, year_to)
  
  # gas_api_data has the last 2 years, get more if required
  if(!year_from %in% downloaded_years_gas){
    print("Expanding gas data")
    gas_api_data_exp <- get_gas_api(date_from = sprintf("%s-01-01", year_from),
                                    date_to = sprintf("%s-01-01", year_to)) %>%
      filter(!is.na(region_id))
    gas_api_data <<- gas_api_data %>% bind_rows(gas_api_data_exp) %>% distinct() # update global, prevent double counting
    downloaded_years_gas <<- c(downloaded_years_gas, (year_from):min(downloaded_years_gas)) %>%
      unique()
  }
  
  gas_data <- gas_api_data %>%
    filter(date >= lubridate::date(sprintf("%s-01-01", year_from)),
           date < lubridate::date(sprintf("%s-01-01", as.numeric(year_to) + 1))) %>%
    # mutate(iso2 = countries[country]) %>%
    filter(region_id == country_input)
  
  print("Done")
  return(gas_data)
})


gas <- reactive({
  
  gas_raw <- gas_raw()
  frequency_input <- input$frequency_gas
  data_type <- input$data_type_gas
  # sources <- input$sources_gas
  req(gas_raw, frequency)
  
  print("Processing power data")
  gas <- gas_raw %>%
    filter(fuel == data_type) %>%
    mutate(date=lubridate::floor_date(date, unit=frequency_input)) %>%
    group_by(across(-c(value))) %>%
    summarise_at("value", sum) %>%
    ungroup()
  
  print("Done")
  return(gas)
})


caption_gas <- reactive({ 
  gas <- gas_raw()
  req(gas)
  
  ds <- unique(gas$data_source)
  ref <- paste0("Source: ", data_source_reference(ds),". ")
  update <- paste0("Last updated on ",
                   strftime(max(lubridate::date(gas$date), na.rm=T), "%d %B %Y."))
  return(paste0(ref, update))
})


output$gas_plot <- renderPlotly({
  
  y_labels <- c(
    "fossil_gas" = 'Fossil Gas Use (m3)',
    "fossil_gas_temperature_corrected" = 'Fossil Gas Use (TWh/day)',
    "electricity_temperature_corrected" = 'Electricity production (GW)'
  )
  
  plot_type <- input$plot_type_gas
  # sources <- input$sources_gas
  gas <- gas()
  caption <- caption_gas()
  frequency <- isolate(input$frequency_gas)
  
  req(gas, plot_type, caption, frequency)
  
  if(plot_type %in% c("lines", 'lines_yearly')){
    # running average, covers NA dates
    if(input$rolling_gas != 1 && frequency == "day"){
      year_from <- input$year_from_gas
      year_to <- input$year_to_gas
      
      dates <- seq(as.Date(paste0(year_from, '-01-01')), as.Date(paste0(year_to, '-12-31')),
                   by = 'days') %>% tibble(date = .) # all possible date combination
      
      gas <- dates %>% left_join(gas) %>%
        arrange(date) %>%
        group_by(fuel, region_id, region_type) %>%
        mutate(value = zoo::rollmean(x = value, as.numeric(input$rolling_gas),
                                        fill = NA, na.rm = T)) %>%
        ungroup() # %>%
        # group_by(date, data_source, country, region) %>%
        # mutate(output_pct = value_mw / sum(value_mw)) %>%
        # ungroup()
    }
    
    # TODO change x-asis to date-month
    gas <- gas %>% mutate(year = lubridate::year(date),
                          month = lubridate::month(date),
                          doy = lubridate::yday(date)) # for line_yearly
    
    if(plot_type == 'lines'){
      plt <- plot_ly(gas,
                     x = ~date,
                     y = ~value,
                     # color = ~source,
                     # customdata=~source,
                     # colors=creapower::palette_power(),
                     type = "scatter",
                     mode="lines+marker",
                     # hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
                     showlegend = T) %>%
        layout(
          hovermode = "x unified",
          yaxis = list(title = y_labels[input$data_type_gas]),
          xaxis = list(title = ''))
    }
    
    if(plot_type == 'lines_yearly'){
      p <- ggplot(gas, aes(x = doy, y = value, color = as.factor(year),
                           group = as.factor(year),
                           text = paste0('Date: ', date, '<br>',
                                         y_labels[input$data_type_gas], ': ', sprintf(value, fmt = '%#.2f'), '<br>',
                                         'Year: ', year))) +
        geom_path() +
        # facet_wrap(~ source, scales = 'free_y') +
        expand_limits(y = 0) +
        labs(x = 'Day of year', # TODO change to date-month
             y = y_labels[input$data_type_gas],
             color = 'Year')
      
      plt <- ggplotly(p, tooltip = 'text')
    }
  }
  
  if(plot_type=="bar"){
    gas_deyeared <- gas %>%
      mutate(year=lubridate::year(date),
             date2000 = lubridate::`year<-`(date, 2000)) %>%
      group_by(fuel, region_id, region_type, date2000, year) %>%
      summarise(value=sum(value)) %>%
      ungroup()

    tickformat <- recode(frequency,
                         "day"="%e %b",
                         "week"= "%W",
                         "month"="%b",
                         "year"="%Y")
    # print(frequency)
    dtick <- ifelse(frequency=="month", "M1", NA)

    plt <- plot_ly(gas_deyeared,
                   x = ~date2000,
                   y = ~value,
                   color = ~factor(year),
                   # customdata = ~year,
                   colors = 'Reds',
                   type = 'bar',
                   alpha = 0.9,
                   # hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
                   showlegend = T) %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = y_labels[input$data_type_gas]),
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