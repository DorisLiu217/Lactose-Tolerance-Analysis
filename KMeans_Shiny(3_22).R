library(shiny)
library("imputeTS")
library(ggplot2)
library(RColorBrewer)
#install.packages("viridisLite")
library(viridis)

setwd("/Users/andy/Desktop/Spring2020/Shiny")

data <- read.csv("two_label_with_selected_features_rn_v3.csv")
continuous <- data[c(2:971)]

# Replace NAs with mean
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)
summary(dataset$rs309233)
summary(data$pheno)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Lactose Intolerance k-means clustering'),
    sidebarPanel(

      selectizeInput('xcol', 'X Variable', names(dataset), options = list(create = TRUE)),
      selectizeInput('ycol', 'Y Variable', names(dataset), options = list(create = TRUE)),

      checkboxInput('cluster', 'Colored by Clusters', FALSE),
      
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9),
      checkboxInput('pheno', 'Colored by Pheno', FALSE)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)

server <- function(input, output, session) {
  selectedData <- reactive({
    dataset[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    
    g <- ggplot(selectedData(), aes_string(input$xcol, input$ycol)) +
       geom_jitter(position = position_jitter(width = 0.5, height = 0.5), shape = 21)
     if (input$cluster){
       g <- g + aes(col = clusters()$cluster, fill = clusters()$cluster) +
         
       scale_color_viridis(discrete = FALSE, option = "D") +
       scale_fill_viridis(discrete = FALSE) +
         annotate("point", x = clusters()$centers[, 1], y = clusters()$centers[, 2],
                  size = 5, colour = 'red')

         # scale_color_continuous(type = "viridis") + scale_fill_continuous(type = "viridis") +
         #    annotate("point", x = clusters()$centers[, 1], y = clusters()$centers[, 2],
         #            size = 5, colour = 'red')
         
     if (input$pheno){
       g <- g + aes(col = data$pheno, fill = data$pheno) + 
         scale_fill_manual(values=c("yellow", "gray")) + 
         scale_color_manual(values=c("yellow", "gray"))
     }
     }
     g
    
})
}

shinyApp(ui = ui, server = server)

