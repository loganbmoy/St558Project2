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
library(ggbeeswarm)

# Loading Data
mhData <- readRDS("data/mhData.rds")


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
    ################## making it dynamic
    
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
        
        box(
          title = "Subset Information",
          width = 12,
          
          textOutput("subsetInfo")
        ),
        
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
          
          # Bar graph allows the user to control pos as well
          tabPanel(
            "Bar Graph",
            radioButtons(
              "barVar",
              "Categorical Variable",
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
            radioButtons(
              "barPos",
              "Bar Position",
              choices = c(
                "Stack",
                "Dodge",
                "Fill"
              ),
              selected = "Stack"
            ),
            withSpinner(plotOutput("mhBar"))
          ),
          
          # Multiple tabs of bar exist, too lazy to combine them into one with dropdown or smthing
          tabPanel(
            "Facet Bar Graph",
            radioButtons(
              "barFacetVar",
              "Categorical Variable",
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
                "Rooms" = "Rooms",
                "Type" = "Type",
                "Month" = "Month",
                "Year" = "Year",
                "Region" = "Regionname",
                "Method of Sale" = "Method"
              ),
              selected = "Type"
            ),
            withSpinner(plotOutput("mhBarFacet"))

          ),
          
          # Only exists so I can include council area, which has really long names
          tabPanel(
            "Flipped Bar Graph",
            radioButtons(
              "barFlipVar",
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
                "Method of Sale" = "Method",
                "Council Area" = "CouncilArea"
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
              "Categorical Variable",
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
              "Numeric Variable",
              choices = c(
                "Price" = "Price",
                "Property Count" = "Propertycount",
                "Distance" = "Distance"
              )
            ),
            radioButtons(
              "boxColor",
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
            withSpinner(plotOutput("mhBoxplot"))
          ),
          
          tabPanel(
            "Beeswarm",
            radioButtons(
              "beeVarx",
              "Categorical Variable",
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
              "Numeric Variable",
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
                "None" = "None",
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
  
  # Subset Information
  output$subsetInfo <- renderText({
    
    req(mhSub())
    
    if (nrow(mhSub()) == 0) {
      "No observations remain after filtering."
    } else {
      paste(
        "Current subset contains", nrow(mhSub()), "observations."
      )
    }
    
  })
  
  
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
  
  
  ###### Plots all remake whenever user changes anything
  
  # Histogram
  output$mhHistogram <- renderPlot({
    req(mhSub())
    
    validate(
      need(!is.null(input$histVar),"Select a numerical variable.")
    )
    if (input$histColor != "None") {
      ggplot(mhSub(), aes(.data[[input$histVar]], fill = .data[[input$histColor]])) + geom_histogram(bins = 25) +
        labs(caption = "Bins = 25") +
        scale_fill_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$histVar]])) + geom_histogram(bins = 25) +
        labs(caption = "Bins = 25") +
        theme_minimal()
    }
    
  })
  
  # Bar
  output$mhBar <- renderPlot({
    req(mhSub())
    
    validate(
      need(
        input$barColor == "None" ||
        input$barVar != input$barColor,
        "Color variable must be different from the categorical variable."
      )
    )
    if (input$barColor != "None") {
      ggplot(mhSub(), aes(.data[[input$barVar]], fill = .data[[input$barColor]])) + geom_bar(position = input$barPos) +
        scale_fill_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$barVar]])) + geom_bar(position = input$barPos) +
        theme_minimal()
    }
    
  })
  
  # Faceted Bar
  output$mhBarFacet <- renderPlot({
    req(mhSub())
    
    validate(
      need(
        (input$barFacetVar != input$barFacetFacet &&
         input$barFacetColor != input$barFacetFacet &&
         input$barFacetVar != input$barFacetColor
        ) |
        input$barFacetColor == "None",
        "Cannot use the same categorical variable."
      )
    )
    if (input$barFacetColor != "None") {
      ggplot(mhSub(), aes(.data[[input$barFacetVar]], fill = .data[[input$barFacetColor]])) + geom_bar() +
        facet_wrap(~.data[[input$barFacetFacet]]) +
        scale_fill_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$barFacetVar]])) + geom_bar() +
        facet_wrap(~.data[[input$barFacetFacet]]) +
        theme_minimal()
    }
    
  })
  
  # Flipped Bar
  output$mhBarFlip <- renderPlot({
    req(mhSub())
    
    validate(
      need(
        input$barFlipColor == "None" ||
        input$barFlipVar != input$barFlipColor,
        "Color variable must be different from the categorical variable."
      )
    )
    if (input$barFlipColor != "None") {
      ggplot(mhSub(), aes(x = reorder(.data[[input$barFlipVar]], .data[[input$barFlipVar]], length), fill = .data[[input$barFlipColor]])) +
        geom_bar() +
        coord_flip() +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(x = reorder(.data[[input$barFlipVar]], .data[[input$barFlipVar]], length))) +
        geom_bar() +
        coord_flip() +
        theme_minimal()
    }
    
  })
  
  
  # Scatter
  output$mhScatter <- renderPlot({
    req(mhSub())
    
    validate(
      need(
        input$scatterVarx != input$scatterVary,
        "Scatterplot should use two different numerical variables."
      )
    )
    if (input$scatterColor != "None") {
      ggplot(mhSub(), aes(.data[[input$scatterVarx]], .data[[input$scatterVary]], color = .data[[input$scatterColor]])) + geom_point(na.rm = TRUE, alpha = 0.5) +
        scale_color_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$scatterVarx]], .data[[input$scatterVary]])) + geom_point(na.rm = TRUE, alpha = 0.5) +
        theme_minimal()
    }
    
  })
  
  # Box
  output$mhBoxplot <- renderPlot({
    req(mhSub())

    validate(
      need(
        input$boxColor == "None" ||
        input$boxVarx != input$boxColor,
        "Color variable must be different from the categorical variable."
      )
    )
    if (input$boxColor != "None") {
      ggplot(mhSub(), aes(.data[[input$boxVarx]], .data[[input$boxVary]], color = .data[[input$boxColor]])) + geom_boxplot() +
        scale_color_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$boxVarx]], .data[[input$boxVary]])) + geom_boxplot() +
        theme_minimal()
    }

  })

  # Beeswarm
  output$mhBee <- renderPlot({
    req(mhSub())
    
    validate(
      need(
        input$beeColor == "None" ||
          input$beeVarx != input$beeColor,
        "Color variable must be different from the categorical variable."
      )
    )
    if (input$beeColor != "None") {
      ggplot(mhSub(), aes(.data[[input$beeVarx]], .data[[input$beeVary]], color = .data[[input$beeColor]])) + geom_quasirandom(na.rm = TRUE, alpha = 0.5) +
        scale_color_viridis_d(option = "C") +
        theme_minimal()
    } else {
      ggplot(mhSub(), aes(.data[[input$beeVarx]], .data[[input$beeVary]])) + geom_quasirandom(na.rm = TRUE, alpha = 0.5) +
        theme_minimal()
    }

  })
  
  
  
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
    
    if (input$sumCatVar == "None") {
      
      mhSub() %>%
        summarize(
          n = n(),
          mean = mean(.data[[input$sumNumVar]], na.rm = TRUE),
          sd = sd(.data[[input$sumNumVar]], na.rm = TRUE),
          median = median(.data[[input$sumNumVar]], na.rm = TRUE),
          min = min(.data[[input$sumNumVar]], na.rm = TRUE),
          max = max(.data[[input$sumNumVar]], na.rm = TRUE)
        )
      
    } else {
      
      mhSub() %>%
        group_by(.data[[input$sumCatVar]]) %>%
        summarize(
          n = n(),
          mean = mean(.data[[input$sumNumVar]], na.rm = TRUE),
          sd = sd(.data[[input$sumNumVar]], na.rm = TRUE),
          median = median(.data[[input$sumNumVar]], na.rm = TRUE),
          min = min(.data[[input$sumNumVar]], na.rm = TRUE),
          max = max(.data[[input$sumNumVar]], na.rm = TRUE),
          .groups = "drop"
        )
      
    }

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
