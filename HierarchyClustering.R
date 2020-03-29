library(shiny)
library("imputeTS")
library(viridis)

# Read and clean data
data <- read.csv("two_label_with_selected_features_rn_v3.csv")
df <- data[, 2:971]
tolerance <- data[, c(1, 972)]
df_remove_col <- df[ , which(colMeans(!is.na(df)) > 0.5)]
cleaned_data<-df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 0.1), ]

# Replace NAs with mean
dataset <- na_mean(cleaned_data, option = "mode", maxgap = Inf)


ui <- fluidPage(
  pageWithSidebar(
    titlePanel(
      h1("Lactose Intolerance hierarchical clustering", align = "center")
    ),
    sidebarPanel(
      selectInput('distance', 'Distance Method', c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
      selectInput('linkage', 'Linkage Method', c( "average", "single", "complete", "ward.D", "ward.D2")),
      checkboxInput('cluster', 'Colored by Clusters', FALSE),
      conditionalPanel(
        condition = "input.cluster",
        selectInput("clusterNum", "Cluster Number", c(1,2,3,4,5,6,7,8,9))
      ),
      checkboxInput('pheno', 'Colored by Pheno', FALSE)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)

server <- function(input, output, session) {
  # Set selected distance
  distance <- reactive({
    dist(dataset, method = input$distance)
  })
  
  clusters <- reactive({
    hclust(distance(), method = input$linkage)
  })
  
  output$plot1 <- renderPlot({
    hcd <- as.dendrogram(clusters())
    clusterColors <- as.vector(viridis(9))
    labelColors <- c("red", "blue")
    
    if (input$cluster) {
      # Color according to clusters
      clusterCut <- cutree(clusters(), input$clusterNum)
      
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
      
    } else if (input$pheno) {
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