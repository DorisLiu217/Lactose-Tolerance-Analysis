library(shiny)
library(ggplot2)
library(imputeTS)

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
      selectInput('ycol', 'Y Variable', names(PCdata[973:980]),
                  selected = names(PCdata)[973:974]),
      ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)

server <- function(input, output, session) {
    PData <- reactive({PCdata[, c(input$xcol, input$ycol)]})
    output$plot1 <- renderPlot({
      ggplot(PData(), aes_string(input$xcol, input$ycol, col = PCdata$pheno, fill = PCdata$pheno)) +
        stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
        geom_point(shape = 21, col = "black")
    })
    
  }

shinyApp(ui = ui,server = server)

# add options for plots which they would like
# global app function
# call global app before ui
