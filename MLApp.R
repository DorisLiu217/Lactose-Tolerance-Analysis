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
                    titlePanel('KNN'),
                  ),
                  mainPanel(
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
    df_remove_col <- df_filtered[ , which(colMeans(!is.na(df_filtered)) > 1 - colPercent())]
    cleaned_data<-df_remove_col[which(rowMeans(!is.na(df_remove_col)) > 1 - rowPercent()),]
    dataset2 <- na_mean(cleaned_data, option = replace(), maxgap = Inf)
    return (dataset2)
  })
  colPercent <- reactive({as.numeric(input$colMissingPercent) / 100})
  rowPercent <- reactive({as.numeric(input$rowMissingPercent) / 100})
  replace <- reactive({input$replacement})

  output$table <- DT::renderDataTable(
    dataset(), options = list(scrollX = TRUE))
  
}

shinyApp(ui = ui, server = server)
