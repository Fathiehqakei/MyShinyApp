ui <- fluidPage(
  titlePanel("K-means Clustering of COVID-19 Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv")),
      sliderInput("clusters", "Number of Clusters:", min = 2, max = 10, value = 3),
      uiOutput("dateRangeUI"),
      actionButton("update", "Update Clustering")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("dataTable")),
        tabPanel("Summary Table", tableOutput("summaryTable")),
        tabPanel("Gap Statistic", plotOutput("gapPlot")),
        tabPanel("Elbow Method", plotOutput("elbowPlot")),
        tabPanel("Cluster Plot",  plotOutput("clusterPlot")),
        tabPanel("Silhouette Plot", plotOutput("silhouettePlot")),
        tabPanel("Parallel Coordinates Plot", plotlyOutput("parallelPlot")),
        tabPanel("Cluster Centers", tableOutput("clusterCenters")),
        tabPanel("Cases vs Deaths Scatter Plot", plotlyOutput("casesDeathsPlot")),
        tabPanel("Interactive Line Plot", plotlyOutput("linePlot"))
      )
    )
  )
)

