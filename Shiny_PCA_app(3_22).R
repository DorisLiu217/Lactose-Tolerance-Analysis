library(shiny)
library(ggplot2)
library(imputeTS)
#install.packages("colourpicker")
library(colourpicker)


setwd("/Users/andy/Desktop/Spring2020/Shiny")

data <- read.csv("two_label_with_selected_features_rn_v3.csv")
continuous <- data[c(2:971)]
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)
myPCA <- prcomp(dataset, center = TRUE, scale = TRUE) 
PCdata <- cbind(data, myPCA$x[,1:8])

ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Lactose Intolerance PCA'),
    sidebarPanel(
      
      selectizeInput('xcol', 'X Variable', choices = names(PCdata[973:980]), options = list(create = TRUE)),
      selectizeInput('ycol', 'Y Variable', choices = names(PCdata[973:980]), options = list(create = TRUE)),
      checkboxInput("col", "Color by Pheno", FALSE),
      checkboxInput("stat_ellipse", "Adding Ellipse", FALSE)
    )
      ,
    mainPanel(
      plotOutput('plot')
    )
  )
)


server <- function(input, output, session) {

    PData <- reactive({PCdata[, c(input$xcol, input$ycol)]})
    output$plot <- renderPlot({
      g <- ggplot(PData(), aes_string(input$xcol, input$ycol)) + 
                    geom_point(shape = 21, col = "black") + 
                    ggtitle("Lactose Intolerance PCA Interaction") + 
                    xlab(input$xcol) + ylab(input$ycol)
        if (input$col){
          g <- g + aes(col = PCdata$pheno, fill = PCdata$pheno)
        }

        if (input$stat_ellipse){
        g <- g + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) 
        }
      g
    })

}
    
  

shinyApp(ui = ui,server = server)
