library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
library(imputeTS)
library(dplyr)
library(ggplot2)
library(viridis)
library(plotly)
library("DT") 

ui <- dashboardPage(
  dashboardHeader(title = "Lactose intolerance"),
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
                    selectizeInput('xcol', 'X Variable', choices = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8'), 
                                   options = list(create = TRUE), selected = c('PC1')),
                    selectizeInput('ycol', 'Y Variable', choices = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8'), 
                                   options = list(create = TRUE), selected = c('PC2')),
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
                    selectizeInput('xcol2', 'X Variable', choices = c(""), options = list(maxItems = 1)),
                    selectizeInput('ycol2', 'Y Variable', choices = c(""), options = list(maxItems = 1)),
                    hr(),
                    checkboxInput('kmeans_colcluster', 'Color by Clusters', FALSE),
                    numericInput('kmeans_clusters', 'Cluster count', 2,
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
                    selectizeInput('xcol3', 'X Variable', choices = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8'), 
                                   options = list(create = TRUE), selected = c('PC1')),
                    selectizeInput('ycol3', 'Y Variable', choices = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8'), 
                                   options = list(create = TRUE), selected = c('PC2')),
                    selectizeInput('zcol3', 'Z Variable', choices = c('PC1','PC2','PC3','PC4','PC5','PC6','PC7','PC8'),
                                   options = list(create = TRUE), selected = c('PC3'))
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
    df <- read.csv("two_label_with_selected_features_rn_v3.csv")
    if (!is.null(input$file)) {
      df <- read.csv(input$file$datapath)
    }
    colnames<- names(df)
    if (colnames[1] != 'names' || colnames[ncol(df)] != 'pheno') {
      return("Input file formate error: Please make sure the first column of the input file is names and the last column is pheno.")
    }
    return (df)
  })
  
  
  # Filtered Dataset
  dataset_filtered <- reactive({
    df2 <- dataset()
    df_filtered <- df2[ , 2:length(df2)]
    df_remove_col <- df_filtered[ , which(colMeans(!is.na(df_filtered)) > 1 - colPercent())]
    cleaned_data <- df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 1 - rowPercent()),]
    dataset2 <- na_mean(cleaned_data, option = replace(), maxgap = Inf)
    return(dataset2)
  })
  
  # Remove x% of rows and columns 
  colPercent <- reactive({as.numeric(input$colMissingPercent) / 100})
  rowPercent <- reactive({as.numeric(input$rowMissingPercent) / 100})
  replace <- reactive({input$replacement})
  
  
  output$table <- DT::renderDataTable(
    dataset_filtered(), options = list(scrollX = TRUE))
  
  # Extract pheno
  phenotype <- reactive({
    if (class(dataset_filtered()[, length(dataset_filtered())]) == "factor") {
      dataset_filtered()[, length(dataset_filtered())]
    } else {
      as.factor(dataset_filtered()[, length(dataset_filtered())])
    }
  })
  
  data_without_labs <- reactive(dataset_filtered()[, 1:length(dataset_filtered()) - 1])
  
  # PCA
  pr.out <- reactive({
    prcomp(data_without_labs(), center = TRUE, scale. = TRUE)
  })
  
  pca.x <- reactive({
    as.data.frame(pr.out()$x[,1:8])
  })
  
  output$plot1 <- renderPlot({
    g <- ggplot(pca.x(), aes_string(input$xcol, input$ycol)) + 
      geom_point(shape = 21, col = "black") + 
      ggtitle("PCA Plot") + 
      xlab(input$xcol) + ylab(input$ycol)
    if (input$col){
      g <- g + aes(col = phenotype(), fill = phenotype())
    }
    
    if (input$stat_ellipse){
      g <- g + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) 
    }
    g
  })
  
  # Variability of each principal component: pr.var
  pr.var <- reactive({pr.out()$sdev^2})
  # Variance explained by each principal component: pve
  pve <- reactive({pr.var()/sum(pr.var())})
  
  output$plot5 <- renderPlot({
    plot(pve(), xlab = "Principal Component",
         ylab = "Proportion of Variance Explained",
         main = "PCA Scree Plot",
         ylim = c(0, 1), type = "b")
  })
  
  output$plot6 <- renderPlot({
    plot(cumsum(pve()), xlab = "Principal Component",
         ylab = "Cumulative Proportion of Variance Explained",
         main = "PCA Cumulative Plot",
         ylim = c(0, 1), type = "l")
  })
  
  observe({
    xcol2 <- names(data_without_labs()[,1:length(data_without_labs())])
    updateSelectizeInput(session, "xcol2",
                         choices = xcol2,
                         selected = "rs4988235",
                         options = list(maxItems = 1))
  })
  
  observe({
    ycol2 <- names(data_without_labs()[,1:length(data_without_labs())])
    updateSelectizeInput(session, "ycol2",
                         choices = ycol2,
                         options = list(maxItems = 1),
                         selected = "rs182549")
  })
  
  kmeans_countclusters <- reactive({
    kmeans(data_without_labs(), input$kmeans_clusters)
  })
  
  km_cluster <- reactive({as.factor(kmeans_countclusters()$cluster)})
  
  km_center <- reactive({as.data.frame(kmeans_countclusters()$centers)})
  
  output$plot2 <- renderPlot({
    g1 <- ggplot(data_without_labs(), aes_string(input$xcol2, input$ycol2)) +
      geom_jitter(position = position_jitter(width = 0.5, height = 0.5), shape = 21) + 
      ggtitle("K-means Clustering Plot") + 
      xlab(input$xcol2) + ylab(input$ycol2)
    if (input$kmeans_colcluster){
      g1 <- g1 + aes(col = km_cluster(), fill = km_cluster()) +
        annotate("point", x = km_center()[,1], y = km_center()[,2],
                 size = 5, colour = 'red')
    }
    if (input$kmeans_pheno){
      g1 <- g1 + aes(col = phenotype(), fill = phenotype())
    }
    g1
  })
  
  x <- reactive({
    pca.x()[, c(input$xcol3)]
  })
  
  y <- reactive({
    pca.x()[, c(input$ycol3)]
  })
  
  z <- reactive({
    pca.x()[, c(input$zcol3)]
  })
  
  output$plot3 <- renderPlotly({
    plot_ly(x = x(), y = y(), z = as.matrix(z()),
            color = phenotype(),
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
    dist(data_without_labs(), method = input$distance)
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
          label <- dataset()[index, ncol(dataset())]
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
