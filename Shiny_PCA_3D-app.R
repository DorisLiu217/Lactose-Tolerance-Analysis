library(shiny)
library(tidyverse)
library(imputeTS)
library(plotly)


data <- read.csv("two_label_with_selected_features_rn_v3.csv")
continuous <- data[c(2:971)]
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)
myPCA <- prcomp(dataset)
PCdata <- cbind(data, myPCA$x[,1:8])

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Lactose Intolerance PCA'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(PCdata[973:980])),
      selectInput('ycol', 'Y Variable', names(PCdata[973:980])),
      selectInput("zcol", "Z Variable", names(PCdata[973:980]))
      ),
    mainPanel(
      plotlyOutput("plot", height = 750, width = 750)
      )
    )
)


server <- function(input, output, session) {
    PData <- reactive({PCdata(,c(input$xcol, input$ycol, input$zcol))})
    output$plot <- renderPlotly({
      plot_ly(x = input$xcol, y = input$ycol, z = input$zcol) %>%
        add_surface() %>%
        layout(
          title = paste(input$xcol, "by", input$ycol, "by", input$zcol),
          scene = list(
            xaxis = list(title = input$xcol),
            yaxis = list(title = input$ycol),
            zaxis = list(title = input$zcol)
          )
        )
    })

}

shinyApp(ui = ui,server = server)    
  
