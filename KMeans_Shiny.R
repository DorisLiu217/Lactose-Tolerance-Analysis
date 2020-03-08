library(shiny)
library("imputeTS")

data <- read.csv("two_label_with_selected_features_rn_v3.csv")
continuous <- data[c(2:971)]

# Replace NAs with mean
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Lactose Intolerance k-means clustering'),
    sidebarPanel(
      #selecesizeinput: can enter as a search box
      # include checkboxInput for color by phenotype and cluster
      selectInput('xcol', 'X Variable', names(dataset)),
      selectInput('ycol', 'Y Variable', names(dataset)),
      numericInput('clusters', 'Cluster count', 2,
                   min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)
server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    dataset[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    # upgrade to ggplot for label and jitter
    # there is a function choose each data point 
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

shinyApp(ui = ui, server = server)

