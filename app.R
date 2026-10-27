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
      
      selectizeInput(inputId = "numvar",
                     label = "Numeric Variable",
                     choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                     selected = "Yards.Gained"),
      
      h2("Select a category"),
      
      selectizeInput(inputId = "cat",
                     label = "Categorical Variable",
                     choices = c("Play Type" = "PlayType", "Section of Field" = "Location", "Quarter" = "qtr"),
                     selected = "PlayType"),
      
      h2("Choose a subset of the data:"),
      
      ## Play type
      
      selectInput(inputId = "play_type",
                  label = "Type of Play",
                  choices = c("Pass", "Run", "Both"),
                  selected = "Pass"),
      
      ## Section of field
      
      checkboxGroupButtons("location", "Play Location",
                           choices = c("left", "right", "middle"),
                           selected = c("left", "right", "middle")),
      
      ## Quarter
      
      radioButtons(inputId = "quarter",
                   label = "Select a Quarter",
                   choiceNames = c("All", "First", "Second", "Third", "Fourth", "Overtime"),
                   choiceValues = c("all", "1", "2", "3", "4", "5"),
                   selected = "all"),
      
      h2("Subset Numeric Variable (optional)"),
      
      sliderInput(inputId = "num_range",
                  label = NULL,
                  min = 0,
                  max = 100,
                  value = c(0,100)),
      
      actionButton("update","Update plot!") #### NEEDS TO BE CHANGED EVENTUALLy
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    d <- plays
    if(input$play_type != "Both") d <- filter(d, PlayType == input$play_type)
    if(input$location != "all") d <- filter(d, Location == input$location)
    if(input$quarter != "all") d <- filter(d, qtr == input$quarter)
    d
  })
  
  observeEvent(list(input$numvar, filtered_data()), {
    x <- filtered_data()[[input$numvar]]
    x <- x[!is.na(x)]
    req(length(x) > 0)
    
    rng <- range(x)
    
    updateSliderInput(session, "num_range",
                      min   = floor(rng[1]),
                      max   = ceiling(rng[2]),
                      value = c(floor(rng[1]), ceiling(rng[2])))
  })
  
  subset_data <- eventReactive(input$update, {
    filtered_data() |>
      filter(!is.na(.data[[input$numvar]]),
             .data[[input$numvar]] >= input$num_range[1],
             .data[[input$numvar]] <= input$num_range[2])
  })
  
  output$plot <- renderPlot({
    validate(
      need(input$update > 0,
           "Please select your variables and subset range, then click **Show Me**")
    )
    
    d <- subset_data()
    req(nrow(d)>0)
    
    d$group <- as.factor(d[[input$cat]])
    
    ggplot(d, aes(x = group, y = .data[[input$numvar]])) +
      geom_boxplot()
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
