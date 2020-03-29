library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
library(imputeTS)
library(ggplot2)
library(viridis)
library(plotly)

data <- read.csv("two_label_with_selected_features_rn_v3.csv")
continuous <- data[c(2:971)]
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)
myPCA <- prcomp(dataset, center = TRUE, scale. = TRUE)
PCdata <- cbind(data, myPCA$x[,1:8])

# Hierarchy Clustering
df <- data[, 2:971]
tolerance <- data[, c(1, 972)]
df_remove_col <- df[ , which(colMeans(!is.na(df)) > 0.5)]
cleaned_data<-df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 0.1),]
dataset2 <- na_mean(cleaned_data, option = "mode", maxgap = Inf)

ui <- dashboardPage(
  dashboardHeader(title = "Lactose Intolerance"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("PCA", tabName = "pca", icon = icon("tree")),
    menuItem("K-Means", tabName = "k-means", icon = icon("tree")),
    menuItem("3D-PCA", tabName = "3d-pca", icon = icon("tree")),
    menuItem("Hierarchy Clustering", tabName = "hierarchy", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "pca",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('Principal Component Analysis'),
                    selectizeInput('xcol', 'X Variable', choices = names(PCdata[973:980]), options = list(create = TRUE)),
                    selectizeInput('ycol', 'Y Variable', choices = names(PCdata[973:980]), options = list(create = TRUE)),
                    hr(),
                    checkboxInput("col", "Color by Phenotype", FALSE),
                    checkboxInput("stat_ellipse", "Add Ellipse", FALSE)),
                  mainPanel(
                    plotOutput("plot1")
                  )
                )
              )
              ),
      tabItem(tabName = "k-means",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   titlePanel('K-means Clustering'),
                   selectizeInput('xcol2', 'X Variable', names(dataset), options = list(create = TRUE)),
                   selectizeInput('ycol2', 'Y Variable', names(dataset), options = list(create = TRUE)),
                   hr(),
                   checkboxInput('cluster', 'Color by Clusters', FALSE),
                   numericInput('clusters', 'Cluster count', 3,
                                min = 1, max = 9),
                   checkboxInput('pheno', 'Color by Pheno', FALSE)),
                 mainPanel(
                   plotOutput("plot2")
                 )
               )
             )
      ),
      tabItem(tabName = "3d-pca",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('3D PCA Plot'),
                    selectizeInput('xcol3', 'X Variable', names(PCdata[973:980]), options = list(create = TRUE)),
                    selectizeInput('ycol3', 'Y Variable', names(PCdata[973:980]), options = list(create = TRUE)),
                    selectizeInput('zcol3', 'Z Variable', names(PCdata[973:980]), options = list(create = TRUE))
                    ),
                  mainPanel(
                    plotlyOutput("plot3")
                    )
                  )
                )
              ),
      tabItem(tabName = "hierarchy",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('Lactose Intolerance hierarchical clustering'),
                    selectInput('distance', 'Distance Method', c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                    selectInput('linkage', 'Linkage Method', c( "average", "single", "complete", "ward.D", "ward.D2")),
                    checkboxInput('hcluster', 'Colored by Clusters', FALSE),
                    conditionalPanel(
                      condition = "input.hcluster",
                      selectInput("hclusterNum", "Cluster Number", c(1,2,3,4,5,6,7,8,9))
                    ),
                    checkboxInput('hpheno', 'Colored by Pheno', FALSE)
                  ),
                  mainPanel(
                    plotOutput('plot4')
                  )
                )
              )
            )
      )
    )
  )

server <- function(input, output, session){
  PData <- reactive({PCdata[, c(input$xcol, input$ycol)]})
  output$plot1 <- renderPlot({
    g <- ggplot(PData(), aes_string(input$xcol, input$ycol)) + 
      geom_point(shape = 21, col = "black") + 
      ggtitle("PCA Plot") + 
      xlab(input$xcol2) + ylab(input$ycol2)
    if (input$col){
      g <- g + aes(col = PCdata$pheno, fill = PCdata$pheno)
    }
    
    if (input$stat_ellipse){
      g <- g + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) 
    }
    g
  })
  
  selectedData <- reactive({
    dataset[, c(input$xcol2, input$ycol2)]
  })
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot2 <- renderPlot({
    g1 <- ggplot(selectedData(), aes_string(input$xcol2, input$ycol2)) +
      geom_jitter(position = position_jitter(width = 0.5, height = 0.5), shape = 21) + 
      ggtitle("K-means Clustering Plot") + 
      xlab(input$xcol2) + ylab(input$ycol2)
    if (input$cluster){
      g1 <- g1 + aes(col = as.factor(clusters()$cluster),
                   fill = as.factor(clusters()$cluster)) +
        annotate("point", x = clusters()$centers[, 1], y = clusters()$centers[, 2],
                 size = 5, colour = 'red')
    }
    if (input$pheno){
      g1 <- g1 + aes(col = data$pheno, fill = data$pheno)
    }
    g1
  })

  x <- reactive({
    PCdata[,input$xcol3]
  })
  y <- reactive({
    PCdata[,input$ycol3]
  })
  z <- reactive({
    PCdata[,input$zcol3]
  })
  output$plot3 <- renderPlotly({
    plot_ly(x = x(), y = y(), z = as.matrix(z()),
            color = PCdata$pheno,
            type = 'scatter3d') %>%
      add_markers() %>%
      layout(
        title = paste(input$xcol3, "by", input$ycol3, "by", input$zcol3),
        scene = list(
          xaxis = list(title = input$xcol3),
          yaxis = list(title = input$ycol3),
          zaxis = list(title = input$zcol3))
      )
  })
  
  distance <- reactive({
    dist(dataset2, method = input$distance)
  })
  
  clusters <- reactive({
    hclust(distance(), method = input$linkage)
  })
  
  output$plot4 <- renderPlot({
    hcd <- as.dendrogram(clusters())
    clusterColors <- as.vector(viridis(9))
    labelColors <- c("red", "blue")
    
    if (input$hcluster) {
      # Color according to clusters
      clusterCut <- cutree(clusters(), input$hclusterNum)
      
      # function to get color labels
      colLab <- function(n) {
        if (is.leaf(n)) {
          a <- attributes(n)
          labCol <- labelColors[clusterCut[which(names(clusterCut) == a$label)]]
          attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
        }
        n
      }
      clusDendro <- dendrapply(hcd, colLab)
      plot(clusDendro, type = "rectangle", ylab = "Height")
      
    } else if (input$hpheno) {
      # function to get color labels
      colLab <- function(n) {
        if (is.leaf(n)) {
          a <- attributes(n)
          index <- as.numeric(as.character(a$label))
          label <- data[index, ncol(data)]
          col <- "red"
          if (label == "tolerant") {
            col <- "blue"
          } 
          attr(n, "nodePar") <- c(a$nodePar, lab.col = col)
        }
        n
      }
      clusDendro <- dendrapply(hcd, colLab)
      plot(clusDendro, type = "rectangle", ylab = "Height")
      legend("topright", 
             legend = c("Intolerant" , "Tolerant"), 
             col = c("red", "blue"), 
             pch = c(20,20,4,4,4), bty = "n",  pt.cex = 1.5, cex = 0.8 , 
             text.col = "black", horiz = FALSE, inset = c(0, 0))
    } else {
      # No coloring
      plot(hcd, type = "rectangle", ylab = "Height")
    }
  })
}

shinyApp(ui = ui, server = server)
