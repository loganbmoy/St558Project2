#
# St558 Project 2
# Logan Moy
#

# Packages
library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(shinydashboard)


# Loading Data
readRDS("data/mhData.rds")

# Define UI for application that explores the Melbourne Data

ui <- dashboardPage(
  
  dashboardHeader(title = "Melbourne Housing"),
  
  dashboardSidebar(
    
    h2("Subset Settings"),
    
    # Cate vars
    h2("Year Property Was Sold"),
    selectInput(
      inputId = "subYear",
      label = "Year:",
      choices = c("All", "2016", "2017", "2018"),
      selected = "All"
    ),
    
    h2("Region Property is in"),
    selectInput(
      inputId = "subRegion",
      label = "Region Name:",
      choices = c("All", 
                  "Northern Metro",
                  "Western Metro",
                  "Southern Metro",
                  "South-Eastern Metro",
                  "Eastern Metro",
                  "Northern Victoria",
                  "Western Victoria",
                  "Eastern Victoria"
                  ),
      selected = "All"
    ),
    
    # Num vars
    h2("Distance (km) from Central Business District (CBD)"),
    sliderInput(
      inputId = "distance",
      label = "Distance:",
      min = 0,
      max = 65,
      value = c(20,40)
    ),
    
    h2("Price (AUD) of Property"),
    sliderInput(
      inputId = "price",
      label = "Price:",
      min = 85000,
      max = 11200000,
      value = c(100000, 1000000)
    ),
    
    actionButton(
      inputId = "houseSub",
      label = "Subset Data"
    ),
    
    br(),
    
    sidebarMenu(
      menuItem(
        text = "About",
        tabName = "about",
        icon = icon("question")
      ),
      
      menuItem(
        text = "Data Download",
        tabName = "downloader",
        icon = icon("download")
      ),
      
      menuItem(
        text = "Data Exploration",
        tabName = "explorer",
        icon = icon("chart-line")
      )
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            title = "About",
            width = 12,
            solidHeader = TRUE,
            p("This app looks at data from the Melbourne Housing Market. I use the MELBOURNE_HOUSE_PRICES_LESS dataset."),
            p("The data, gathered from Domain.com.au, contains information about properities' attributes, such as cost, rooms, and type."),
            p("The Sidebar allows the user to filter the dataset based on the given variables. At the bottom of it the user can select some main tabs."),
            p("The main tab Data Download allows the user to look at the dataset and download it. Dataset will reflect the subsetted variables."),
            p("The main tab Data Exploration allows the user to explore the data using contingency tables and various graphs (box, bar, histogram, etc.)."),
            p("For more information about the dataset, visit ", 
              tags$a(href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market", "Melbourne-Housing-Market", target = "_blank"),
              ".")
            
          )
        ),
        
        tags$figure(
          style = "text-align:center;",
          tags$img(
            src = "images.jpg",
            width = "600px",
            alt = "Picture of Melbourne"
          ),
          tags$figcaption(
            "Photo of Melbourne",
            "Source: ",
            tags$a(
              href = "https://www.smartpropertyinvestment.com.au/hotspots/20543-property-market-update-melbourne-december-2019",
              target = "_blank",
              "Smart Property Investment"
            )
          )
        )
      ),
      
      tabItem(
        tabName = "downloader",
        
        fluidRow(
          box(
            title = "Data Download",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            DT::dataTableOutput(outputId = "dataTable")
          ),
          style = "text-align:center;",
          downloadButton("downloadData", label = "Download")
        )
      ),
      
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          box(
            title = "Data Exploration",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
 
  
  # Main Panel Date Download
  
  ## Data Table
  output$dataTable <- DT::renderDataTable({
    req(input$subRegion)
    mhSub <- mhData ######## sub here
    DT::datatable(data = mhSub,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  ## Download Button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data, con)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
