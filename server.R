server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath, col_types = cols(
      date = col_date(format = ""),
      state = col_character(),
      fips = col_integer(),
      cases = col_integer(),
      deaths = col_integer()
    ))
  })
  
  data_MO <- reactive({
    subset(data(), state == "Missouri")
  })
  
  output$dateRangeUI <- renderUI({
    req(data_MO())
    dateRangeInput("dateRange", "Select Date Range:", 
                   start = min(data_MO()$date), 
                   end = max(data_MO()$date),
                   min = min(data_MO()$date), 
                   max = max(data_MO()$date))
  })
  
  # Reactive expression to read and preprocess data
  reactive_data <- reactive({
    req(data_MO())
    data_filtered <- subset(data_MO(), date >= input$dateRange[1] & date <= input$dateRange[2])
    
    #  check the data
    print(head(data_filtered))
    
    # Ensure date is a Date object
    data_filtered$date <- as.Date(data_filtered$date)
    
    # Select only the 'cases' and 'deaths' columns for normalization
    selected_col <- data_filtered[, c("cases", "deaths")]
    
    # Standardize the data
    mydata <- scale(selected_col)
    list(data_filtered = data_filtered, mydata = mydata)
  })
  
  observeEvent(input$update, {
    # Gap Statistic plot
    output$gapPlot <- renderPlot({
      fviz_nbclust(reactive_data()$mydata, kmeans, method = "gap_stat", nboot = 500, iter.max = 1000)
    })
    
    # Elbow Method plot
    output$elbowPlot <- renderPlot({
      fviz_nbclust(reactive_data()$mydata, kmeans, method = "wss", k.max = 10)
    })
    
    # K-means clustering and visualizations
    km.res <- reactive({
      set.seed(123)
      kmeans(reactive_data()$mydata, input$clusters, nstart = 25, iter.max = 1000)
    })
    
    # Render cluster plot
    output$clusterPlot <- renderPlot({
      fviz_cluster(km.res(), data = reactive_data()$mydata, palette = "jco", ggtheme = theme_minimal())
    })
    
    output$silhouettePlot <- renderPlot({
      sil <- silhouette(km.res()$cluster, dist(reactive_data()$mydata))
      fviz_silhouette(sil)
    })
    
    # Render parallel coordinates plot
    output$parallelPlot <- renderPlotly({
      data_filtered <- reactive_data()$data_filtered
      data_filtered$Cluster <- km.res()$cluster
      p <- ggparcoord(data_filtered, columns = 4:5, groupColumn = "Cluster", scale = "std", alphaLines = 0.8) +
        theme_minimal() +
        labs(title = "Parallel Coordinates Plot of Clusters",
             x = "Variables",
             y = "Standardized Value")
      ggplotly(p)
    })
    
    output$clusterCenters <- renderTable({
      km.res()$centers
    })
    
    # Render Cases vs Deaths plot
    output$casesDeathsPlot <- renderPlotly({
      data_filtered <- reactive_data()$data_filtered
      p <- ggplot(data_filtered, aes(x = cases, y = deaths)) +
        geom_point() +
        labs(title = "Cases vs Deaths Scatter Plot", x = "Cases", y = "Deaths") +
        theme_minimal()
      ggplotly(p)
    })
    
    output$summaryTable <- renderTable({
      data_filtered <- reactive_data()$data_filtered
      summary(data_filtered)
    })
    
    output$dataTable <- renderDT({
      datatable(reactive_data()$data_filtered)
    })
    
    # Render interactive line plot for cases and deaths
    output$linePlot <- renderPlotly({
      data_long <- reactive_data()$data_filtered %>%
        select(date, cases, deaths) %>%
        pivot_longer(cols = c("cases", "deaths"), names_to = "variable", values_to = "value")
      
      #  check the transformed data
      print(head(data_long))
      
      p <- ggplot(data_long, aes(x = date, y = value, color = variable, group = variable)) +
        geom_line() +
        labs(title = "Cases and Deaths Over Time", x = "Date", y = "Count", color = "Metric") +
        theme_minimal()
      ggplotly(p)
    })
    
  })
  
}