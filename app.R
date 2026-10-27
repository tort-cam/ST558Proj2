library(shiny)
library(shinyalert)
library(tidyverse)
library(RKaggle)

df <- RKaggle::get_dataset("maxhorowitz/nflplaybyplay2009to2016")
plays <- df[[1]]
remove(df)
plays <- plays|>
  mutate(Location = coalesce(PassLocation, RunLocation))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(title = "Data Exploration",
             windowTitle = "Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Select your numeric variable"),
      
      selectizeInput(inputId = "xvar",
                     label = "Numeric Variable",
                     choiceNames = c("Yards Gained", "Time Under Center", "Yards to Go"),
                     choicevalues = c("Yards.Gained", "TimeUnder", "ydstogo"),
                     selected = "Yards.Gained"),
      
      h2("Select a category"),
      
      selectizeInput(inputId = "cat",
                     label = "Categorical Variable",
                     choiceNames = c("All", "Play Type", "Section of Field", "Quarter"),
                     choiceValues = c("all", "Play_Type", "Location", "qtr"),        
                     selected = "Yards.Gained"),
      
      h2("Choose a subset of the data:"),
      
      ## Play type
      
      radioButtons(#inputId = "hhl_corr",
                   inputId = "play_type",
                   label = "Type of Play",
                   choiceNames = c("Pass", "Run", "Both"),
                   choiceValues = c("Pass", "Run", "Both"),
                   selected = "Pass"),
      
      ## Section of field
      
      radioButtons(#inputId = "fs_corr",
                   inputId = "location",
                   label = "Play Location",
                   choiceNames = c("All", "Left", "Right", "Middle"),
                   choiceValues = c("all", "left", "right", "middle"),
                   selected = "all"),
      
      ## Quarter
      
      radioButtons(#inputId = "fs_corr",
                    inputId = "quarter",
                    label = "Select a Quarter",
                    choiceNames = c("All", "First", "Second", "Third", "Fourth", "Overtime"),
                    choiceValues = c("all", "1", "2", "3", "4", "5"),
                    selected = "all"),
      
      h2("Subset Numeric Variable (optional)"),
      
      sliderInput(inputId = "sample_size",
                  label = NULL,
                  min = 1,
                  max = 100,
                  value = 100,
                  step = 1),
      
      actionButton("corr_sample","Get a Sample!")
    ),
    
    mainPanel(
      plotOutput("plot"),
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    d <- plays
    if(input$play_type != "Both") d <- filter(d, PlayType == input$play_type)
    if(input$location != "all") d <- filter(d, location == input$location)
    if(input$quarter != "all") d <- filter(d, qtr == input$quarter)
    d
  })
  
  observeEvent(list(input$xvar, filtered_data()), {
    n <- sum(!is.na(filtered_data()[[input$xvar]]))
    req(n > 0)
    
    updateSliderInput(session, "sample_size",
                      min   = min(10, n),
                      max   = n,
                      value = min(input$sample_size, n))
  })
  
  sampled_data <- reactive({
    req(input$sample_size)
    filtered_data() |>
      filter(!is.na(.data[[input$xvar]])) |>
      slice_sample(n = min(input$sample_size, nrow(filtered_data())))
  })
  
  output$plot <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sampled_data, aes(x = xvar, y = )) +
      geom_point()
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
