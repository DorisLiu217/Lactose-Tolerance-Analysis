library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
library(imputeTS)
library(ggplot2)
library(viridis)
library(plotly)
library("DT") 


data <- read.csv("two_label_with_selected_features_rn_v3.csv")

data_filtered <- data[, 2:971]

dataset <- na_mean(data_filtered, option = "mode", maxgap = Inf)
myPCA <- prcomp(dataset, center = TRUE, scale. = TRUE)
PCdata <- cbind(dataset, myPCA$x[,1:8])

ui <- dashboardPage(
  dashboardHeader(title = "Lactose Intolerance"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File input", tabName = "fileinput", icon = icon("tree")),
      menuItem("PCA", tabName = "pca", icon = icon("tree")),
      menuItem("K-Means", tabName = "k-means", icon = icon("tree")),
      menuItem("3D-PCA", tabName = "3d-pca", icon = icon("tree")),
      menuItem("Hierarchy Clustering", tabName = "hierarchy", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "fileinput",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('Uploading files'),
                    fileInput("file", HTML("Choose CSV File <br/>(Please input a file with the first column as id and last column as pheno type)"),
                              multiple = FALSE,
                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    selectInput('colMissingPercent', 'Drop colums with selected percentage of missing values', c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10)),
                    selectInput('rowMissingPercent', 'Drop rows with selected percentage of missing values', c(100, 90, 80, 70, 60, 50, 40, 30, 20, 10)),
                    selectInput('replacement', 'Replace missing values with', c('mode','mean', 'median'))),
                  mainPanel(
                    #tableOutput("table")
                    DT::dataTableOutput("table")
                  )
                )
              )
      ),
      tabItem(tabName = "pca",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('Principal Component Analysis'),
                    selectizeInput('xcol', 'X Variable', choices = names(PCdata[c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]), 
                                   options = list(create = TRUE), selected = names(PCdata[c('PC1')])),
                    selectizeInput('ycol', 'Y Variable', choices = names(PCdata[c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]), 
                                   options = list(create = TRUE), selected = names(PCdata[c('PC2')])),
                    hr(),
                    checkboxInput("col", "Color by Phenotype", FALSE),
                    checkboxInput("stat_ellipse", "Add Ellipse", FALSE),
                    hr(),
                    h4("Scree and Cumulative Plots:"),
                    numericInput('ScreeNum', 'Number of Components', 1, min = 1, max = 8)
                    ),
                  mainPanel(
                    tabsetPanel(id = "tabs", 
                                tabPanel("PCA Plot", 
                                         plotOutput("plot1")
                                ), 
                                tabPanel("Scree Plot",
                                         plotOutput("plot5")
                                ), 
                                tabPanel("Cumulative Plot",
                                         plotOutput("plot6")
                                )
                    )
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
                    checkboxInput('kmeans_colcluster', 'Color by Clusters', FALSE),
                    numericInput('kmeans_countclusters', 'Cluster count', 2,
                                 min = 1, max = 9),
                    checkboxInput('kmeans_pheno', 'Color by Pheno', FALSE)),
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
                    selectizeInput('xcol3', 'X Variable', names(PCdata[c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]), 
                                   options = list(create = TRUE), selected = names(PCdata[c('PC1')])),
                    selectizeInput('ycol3', 'Y Variable', names(PCdata[c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]), 
                                   options = list(create = TRUE), selected = names(PCdata[c('PC2')])),
                    selectizeInput('zcol3', 'Z Variable', names(PCdata[c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8')]), 
                                   options = list(create = TRUE), selected = names(PCdata[c('PC3')]))
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
                    titlePanel('Hierarchical clustering'),
                    selectInput('distance', 'Distance Method', c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                    selectInput('linkage', 'Linkage Method', c( "average", "single", "complete", "ward.D", "ward.D2")),
                    radioButtons("hcoloring", "Color option:",
                                 c("Default color" = "na",
                                   "Colored by clusters" = "cluster",
                                   "Colored by pheno type" = "pheno")),
                    conditionalPanel(
                      condition = "input.hcoloring == 'cluster'",
                      selectInput("hclusterNum", "Cluster Number", c(1,2,3,4,5,6,7,8,9))
                    )
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
  dataset <- reactive({
    # default file if no user input
    filePath = "two_label_with_selected_features_rn_v3.csv"
    if (!is.null(input$file)) {
      filePath <- input$file$datapath
    }
    df <- read.csv(filePath)
    # check file format
    colnames<- names(df)
    if (colnames[1] != 'names' || colnames[ncol(df)] != 'pheno') {
      return("Input file formate error: Please make sure the first column of the input file is names and the last column is pheno.")
    }
    df_filtered <- df[, 2:ncol(df) - 1]
    df_remove_col <- data_filtered[ , which(colMeans(!is.na(data_filtered)) > 1 - colPercent())]
    cleaned_data<-df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 1 - rowPercent()),]
    dataset2 <- na_mean(cleaned_data, option = replace(), maxgap = Inf)
    return (dataset2)
  })
  colPercent <- reactive({as.numeric(input$colMissingPercent) / 100})
  rowPercent <- reactive({as.numeric(input$rowMissingPercent) / 100})
  replace <- reactive({input$replacement})

  output$table <- DT::renderDataTable(
    dataset(), options = list(scrollX = TRUE))
  
  PData <- reactive({PCdata[, c(input$xcol, input$ycol)]})
  output$plot1 <- renderPlot({
    g <- ggplot(PData(), aes_string(input$xcol, input$ycol)) + 
      geom_point(shape = 21, col = "black") + 
      ggtitle("PCA Plot") + 
      xlab(input$xcol2) + ylab(input$ycol2)
    if (input$col){
      g <- g + aes(col = data$pheno, fill = data$pheno)
    }
    
    if (input$stat_ellipse){
      g <- g + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) 
    }
    g
  })
  
  PData2 <- reactive({PCdata[, input$ScreeNum]})
  output$plot5 <- renderPlot({
    screeplot(PData2(),
              type = "lines", main = 'Scree Plot')
  })
  
  selectedData <- reactive({
    dataset[, c(input$xcol2, input$ycol2)]
  })
  kmeans_countclusters <- reactive({
    kmeans(selectedData(), input$kmeans_countclusters)
  })
  
  output$plot2 <- renderPlot({
    g1 <- ggplot(selectedData(), aes_string(input$xcol2, input$ycol2)) +
      geom_jitter(position = position_jitter(width = 0.5, height = 0.5), shape = 21) + 
      ggtitle("K-means Clustering Plot") + 
      xlab(input$xcol2) + ylab(input$ycol2)
    if (input$kmeans_colcluster){
      g1 <- g1 + aes(col = (kmeans_countclusters()$kmeans_colcluster), fill = (kmeans_countclusters()$kmeans_colcluster)) +
        annotate("point", x = kmeans_countclusters()$centers[, 1], y = kmeans_countclusters()$centers[, 2],
                 size = 5, colour = 'red')
    }
    if (input$kmeans_pheno){
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
            color = data$pheno,
            type = 'scatter3d') %>%
      add_markers(showlegend = FALSE) %>%
      layout(
        title = paste(input$xcol3, "vs", input$ycol3, "vs", input$zcol3),
        scene = list(
          xaxis = list(title = input$xcol3),
          yaxis = list(title = input$ycol3),
          zaxis = list(title = input$zcol3))
      )
  })
  
  distance <- reactive({
    dist(dataset(), method = input$distance)
  })
  
  clusters <- reactive({
    hclust(distance(), method = input$linkage)
  })
  
  output$plot4 <- renderPlot({
    hcd <- as.dendrogram(clusters())
    clusterColors <- as.vector(viridis(9))
    labelColors <- c("red", "blue")
    
    if (input$hcoloring == 'cluster') {
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
      
    } else if (input$hcoloring == 'pheno') {
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
