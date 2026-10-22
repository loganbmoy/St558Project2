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
library(shinycssloaders)

# Loading Data
mhData <- readRDS("data/mhData.rds")

numVars <- mhData[sapply(mhData, is.numeric)] %>%
  #select(-c(Day, Postcode)) %>%
  names

# charVars <- mhData[sapply(mhData, is.character)] %>%
#   select(-c(Address, Date)) %>% # not really interested in looking at these, too many unique values or accounted for elsewhere
#   names

catVars <- mhData[sapply(mhData, is.factor)] %>%
  select(-c(Method)) %>% # Methodlab is a bit more descriptive and CouncilArea should be reserved for certain bar graphs
  names

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
      choices = c(
        "All",
        "Northern Metropolitan",
        "Western Metropolitan",
        "Southern Metropolitan",
        "South-Eastern Metropolitan",
        "Eastern Metropolitan",
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
      value = c(20, 40)
    ),
    
    h2("Price (AUD) of Property"),
    sliderInput(
      inputId = "price",
      label = "Price:",
      min = 85000,
      max = 11200000,
      value = c(100000, 1000000)
    ),
    
    actionButton(inputId = "houseSub", label = "Subset Data"),
    
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
  
  dashboardBody(tabItems(
    # About tab
    tabItem(
      tabName = "about",
      
      fluidRow(
        titlePanel(title = "About"),
        box(
          width = 12,
          solidHeader = TRUE,
          p(
            "This app looks at data from the Melbourne Housing Market. I use the MELBOURNE_HOUSE_PRICES_LESS dataset."
          ),
          p(
            "The data, gathered from Domain.com.au, contains information about properities' attributes, such as cost, rooms, and type."
          ),
          p(
            "The Sidebar allows the user to filter the dataset based on the given variables. At the bottom of it the user can select some main tabs."
          ),
          p(
            "The main tab Data Download allows the user to look at the dataset and download it. Dataset will reflect the subsetted variables."
          ),
          p(
            "The main tab Data Exploration allows the user to explore the data using contingency tables and various graphs (box, bar, histogram, etc.)."
          ),
          p(
            "For more information about the dataset, visit ",
            tags$a(
              href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
              "Melbourne-Housing-Market",
              target = "_blank"
            ),
            "."
          )
          
        )
      ),
      
      # pic
      tags$figure(
        style = "text-align:center;",
        tags$img(src = "images.jpg", width = "600px", alt = "Picture of Melbourne"),
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
    
    # Download tab
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
    
    # Explorer tab
    tabItem(
      tabName = "explorer",
      
      # Row for graphs
      fluidRow(
        # If a certain number the following plots use the same vars, something like this could be useful
        # prob will restrict though, so unique choices will be used
        # fluidRow(
        #   box(
        #     width = 12,
        #
        #     selectInput("numVar", "Numeric Variable", numVars),
        #     selectInput("catVar", "Categorical Variable", catVars)
        #   )
        # ),
        
        tabBox(
          title = "Graphs",
          width = 12,
          
          tabPanel(
            "Histogram",
            radioButtons(
              "histVar",
              "Numeric Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              ),
              selected = "Price"
            ),
            radioButtons(
              "histColor",
              "Color Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            withSpinner(plotOutput("mhHistogram"))
          ),
          
          ########################## maybe add pos option for side-by-side here
          tabPanel(
            "Bar Graph",
            radioButtons(
              "barVarx",
              "X Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              )
            ),
            radioButtons(
              "barColor",
              "Color Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            withSpinner(plotOutput("mhBar"))
          ),
          
          tabPanel(
            "Facet Bar Graph",
            radioButtons(
              "barFacetVarx",
              "X Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              )
            ),
            radioButtons(
              "barFacetColor",
              "Color Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            radioButtons(
              "barFacetFacet",
              "Facet Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            withSpinner(plotOutput("mhBarFacet"))

          ),
          
          tabPanel(
            "Flipped Bar Graph",
            radioButtons(
              "barFlipVarx",
              "X Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              )
            ),
            radioButtons(
              "barFlipColor",
              "Color Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            withSpinner(plotOutput("mhBarFlip"))
          ),
          
          tabPanel(
            "Scatterplot",
            radioButtons(
              "scatterVarx",
              "X Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              )
            ),
            
            radioButtons(
              "scatterVary",
              "Y Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              ),
              selected = "Propertycount"
            ),
            radioButtons(
              "scatterColor",
              "Color Variable",
              choices = c(
                "None" = "None",
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "None"
            ),
            withSpinner(plotOutput("mhScatter"))
          ),
          tabPanel(
            "Boxplot",
            radioButtons(
              "boxVarx",
              "X Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              )
            ),
            radioButtons(
              "boxVary",
              "Y Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              )
            ),
            
            plotOutput("mhBoxplot"),
            tabPanel(
              "Beeswarm",
              radioButtons(
                "beeVarx",
                "X Variable",
                choices = c(
                  "Rooms" = "Rooms",
                  "Type" = "Type",
                  "Month" = "Month",
                  "Year" = "Year",
                  "Region" = "Regionname",
                  "Method of Sale" = "Method"
                )
              ),
              radioButtons(
                "beeVary",
                "Y Variable",
                choices = c(
                  "Price" = "Price",
                  "Property Count" = "Propertycount",
                  "Distance" = "Distance"
                )
              ),
              radioButtons(
                "beeColor",
                "Color Variable",
                choices = c(
                  "None" = "None",
                  "Rooms" = "Rooms",
                  "Type" = "Type",
                  "Month" = "Month",
                  "Year" = "Year",
                  "Region" = "Regionname",
                  "Method of Sale" = "Method"
                ),
                selected = "None"
              ),
              withSpinner(plotOutput("mhBee"))
            )
          )
        ),
        
        # Row for summaries
        fluidRow(tabBox(
          title = "Summaries",
          width = 12,
          
          # Summary
          tabPanel(
            "Summary Statistics",
            selectInput(
              "sumNumVar",
              "Numeric Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              )
            ),
            selectInput(
              "sumCatVar",
              "Categorical Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method",
                "Council Area" = "CouncilArea"
              )
            ),
            withSpinner(tableOutput("summaryStats")),
            
            # Action Button
            actionButton(inputId = "makeSummary", label = "Make Summary")
          ),
          
          # Contingency Table, does 1 or 2 way depending on input count
          tabPanel(
            "Contingency Table",
            # Make sure something is selected here
            selectizeInput(
              "tabCat",
              "Categorical Variable",
              choices = c(
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method",
                "Council Area" = "CouncilArea"
              ),
              multiple = TRUE,
              options = list(maxItems = 2),
              selected = c("Rooms", "Type")
            ),
            withSpinner(tableOutput("contingencyTable")),
            
            # Action Button
            actionButton(inputId = "makeTable", label = "Make Table")
          )
        ))
      )
    )
  ))
)


# Server
server <- function(input, output, session) {
  ########## Data Download  ##########
  
  # Reactive filter
  mhSub <- eventReactive(input$houseSub, {
    data <- mhData
    
    # Filter cate
    if (input$subYear != "All") {
      data <- filter(data, Year == as.numeric(input$subYear))
    }
    
    if (input$subRegion != "All") {
      data <- filter(data, Regionname == input$subRegion)
    }
    
    # Filter numeric
    data <- filter(
      data,
      between(Distance, input$distance[1], input$distance[2]),
      between(Price, input$price[1], input$price[2])
    )
    
    data
  })
  
  # The table
  output$dataTable <- DT::renderDataTable({
    req(mhSub())
    
    DT::datatable(mhSub(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  # Downloading
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(mhSub(), file, row.names = FALSE)
    }
  )
  
  ########## Data Exploration  ##########
  
  
  # Plots
  # output$mhHistogram <- renderPlot({
  #   validate(
  #     need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
  #   )
  #   ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
  #     geom_point()
  # })
  
  
  
  # Summary, only remakes when makeSummary used
  sumTable <- eventReactive(input$makeSummary, {
    req(mhSub())
    
    validate(
      need(
        !is.null(input$sumCatVar),
        "Select a categorical variable."
      ),
      need(!is.null(input$sumNumVar), "Select a numeric variable.")
    )
    
    summaryQ <- function(df, group, var) {
      df %>%
        group_by(.data[[group]]) %>%
        summarize(
          n = n(),
          mean = mean(.data[[var]], na.rm = TRUE),
          sd = sd(.data[[var]], na.rm = TRUE),
          median = median(.data[[var]], na.rm = TRUE),
          min = min(.data[[var]], na.rm = TRUE),
          max = max(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    summaryQ(mhSub(), input$sumCatVar, input$sumNumVar)
  })
  
  output$summaryStats <- renderTable({
    sumTable()
  })
  
  # Table, only remakes when makeTable used
  contTable <- eventReactive(input$makeTable, {
    req(mhSub())
    
    validate(need(length(input$tabCat) >= 1, "Select one or two variables."))
    
    if (length(input$tabCat) == 1) {
      table(mhSub()[[input$tabCat[1]]])
      
    } else {
      table(mhSub()[[input$tabCat[1]]], mhSub()[[input$tabCat[2]]])
    }
  })
  
  output$contingencyTable <- renderTable({
    contTable()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
