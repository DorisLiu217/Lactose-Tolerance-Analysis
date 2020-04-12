library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)
library(imputeTS)
library(ggplot2)
library(viridis)
library(plotly)
library("DT") 

ui <- dashboardPage(
  dashboardHeader(title = "ML App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("File input", tabName = "fileinput", icon = icon("tree")),
      menuItem("KNN", tabName = "knn", icon = icon("tree"))
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
      tabItem(tabName = "knn",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    titlePanel('K-nearest Neighbors Algorithm'),
                    sliderInput('trainPercent', 'Training Set Percentage',min = 1, max = 99, value = 80),
                  ),
                  mainPanel(
                    plotOutput('plot1')
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
    df_remove_col <- df[ , which(colMeans(!is.na(df)) > 1 - colPercent())]
    cleaned_data<-df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 1 - rowPercent()),]
    dataset2 <- na_mean(cleaned_data, option = replace(), maxgap = Inf)
    return (dataset2)
  })
  colPercent <- reactive({as.numeric(input$colMissingPercent) / 100})
  rowPercent <- reactive({as.numeric(input$rowMissingPercent) / 100})
  replace <- reactive({input$replacement})

  output$table <- DT::renderDataTable(
    dataset(), options = list(scrollX = TRUE))
  
  
  # Prepare plot and KNN model
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  percent <- reactive({input$trainPercent / 100})
  output$plot1 <- renderPlot({
    # Normalize data
    knn_df <- dataset()
    nData <- as.data.frame(lapply(knn_df[, 2:(ncol(knn_df) - 1)], normalize))
    print("line 91")
    print(dim(nData))
    # Divide into train and test set
    trainNum <- floor(nrow(knn_df) * percent())
    print("train num")
    print(trainNum)
    train_X <- nData[1:trainNum,]
    test_X <- nData[(trainNum + 1): nrow(nData),]
    train_label <- knn_df[1: trainNum, ncol(knn_df)]
    test_label <- knn_df[(trainNum + 1): nrow(knn_df), ncol(knn_df)]
    # Calculate accuracy
    sqrtNum<- floor(sqrt(trainNum))
    acc = vector()
    for (i in 1:(sqrtNum + 10)) {
      knnModel <- knn(train=train_X, test=test_X, cl=train_label, k=i)
      acc[i] <- 100 * sum(test_label == knnModel)/NROW(test_label) 
    }
    plot(acc, type="b", xlab="K- Value",ylab="Accuracy level")
  })
}

shinyApp(ui = ui, server = server)
