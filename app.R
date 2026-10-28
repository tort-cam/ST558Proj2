library(shiny)
library(shinyalert)
library(tidyverse)
library(RKaggle)
library(shinyWidgets)

df <- RKaggle::get_dataset("maxhorowitz/nflplaybyplay2009to2016")
plays <- df[[1]]
remove(df)
plays <- plays|>
  mutate(Location = coalesce(PassLocation, RunLocation))|>
  filter(PlayType %in% c("Pass", "Run"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel(title = "Data Exploration",
             windowTitle = "Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      h2("Select your X variable"),
      
      selectizeInput(inputId = "xvar",
                     label = "X Variable",
                     choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                     selected = "Yards.Gained"),
      
      h2("Select a Y variable"),
      
      selectizeInput(inputId = "yvar",
                     label = "Y Variable",
                     choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                     selected = "Yards.Gained"),
      
      h2("Select a category"),
      
      selectizeInput(inputId = "cat",
                     label = "Categorical Variable",
                     choices = c("Play Type" = "PlayType", "Section of Field" = "Location", "Quarter" = "qtr"),
                     selected = "PlayType"),
      
      h2("Choose a subset of the data"),
      
      ## Play type
      
      selectizeInput(inputId = "play_type",
                  label = "Type of Play",
                  choices = c("Pass", "Run", "Both"),
                  selected = "Pass"),
      
      ## Section of field
      
      checkboxGroupButtons(inputId = "location", 
                           label = "Play Location",
                           choices = c("Left" = "left", "Right" = "right", "Middle" = "middle"),
                           selected = c("left", "right", "middle")),
      
      ## Quarter
      
      radioButtons(inputId = "quarter",
                   label = "Select a Quarter",
                   choiceNames = c("All", "First", "Second", "Third", "Fourth", "Overtime"),
                   choiceValues = c("all", "1", "2", "3", "4", "5"),
                   selected = "all"),
      
      h2("Subset X Variable (optional)"),
      
      sliderInput(inputId = "x_range",
                  label = NULL,
                  min = 0,
                  max = 100,
                  value = c(0,100)),
      
      h2("Subset Y Variable (optional)"),
      
      sliderInput(inputId = "y_range",
                  label = NULL,
                  min = 0,
                  max = 100,
                  value = c(0,100)),
      
      actionButton("update","Show Me!"), #### NEEDS TO BE CHANGED EVENTUALLy
      
      h2("Not enough color for you?")
      switchInput(inputId = "football",label = "Football Mode?", value = FALSE)
      
      ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
      req(input$location)
      
      d <- plays
      if (input$play_type != "Both") d <- filter(d, PlayType == input$play_type)
      if (input$quarter   != "all")  d <- filter(d, qtr == as.numeric(input$quarter))
      
      if (length(input$location) < 3) d <- filter(d, Location %in% input$location)
      
      d
    
  })
  
  observeEvent(list(input$xvar, filtered_data()), {
    x <- filtered_data()[[input$xvar]]
    x <- x[!is.na(x)]
    req(length(x) > 0)
    
    rng <- range(x)
    
    updateSliderInput(session, "x_range",
                      min   = floor(rng[1]),
                      max   = ceiling(rng[2]),
                      value = c(floor(rng[1]), ceiling(rng[2])))
  })
  
  observeEvent(list(input$yvar, filtered_data()), {
    y <- filtered_data()[[input$yvar]]
    y <- y[!is.na(y)]
    req(length(y) > 0)
    
    rng <- range(y)
    
    updateSliderInput(session, "y_range",
                      min   = floor(rng[1]),
                      max   = ceiling(rng[2]),
                      value = c(floor(rng[1]), ceiling(rng[2])))
  })
  
  subset_data <- eventReactive(input$update, {
    filtered_data() |>
      filter(!is.na(.data[[input$xvar]]),
             .data[[input$xvar]] >= input$x_range[1],
             .data[[input$xvar]] <= input$x_range[2],
             !is.na(.data[[input$yvar]]),
             .data[[input$yvar]] >= input$y_range[1],
             .data[[input$yvar]] <= input$y_range[2])
  })
  
  output$plot <- renderPlot({
    validate(
      need(input$update > 0,
           "Please select your variables and subset range, then click 'Show Me'")
    )
    
    d <- subset_data()
    req(nrow(d)>0)
    
    d <- d |> filter(!is.na(.data[[input$cat]]))
    
    d$group <- as.factor(d[[input$cat]])

    ## Plotting with football checks
    
    p <- ggplot(d, aes(x = .data[[input$xvar]],
                       y = .data[[input$yvar]],
                       color = group))
    
    if (input$football) {
      p <- p + theme(
        panel.background   = element_rect(fill = "darkgreen"),
        panel.grid.major   = element_line(color = "white"),
        panel.grid.minor   = element_blank()
      )
      
      if (input$yvar == "Yards.Gained") {
        p <- p + list(
          geom_hline(yintercept = 10, color = "yellow", linewidth = 1),
          geom_hline(yintercept = 0,  color = "blue",   linewidth = 1)
        )
      }
      if (input$xvar == "Yards.Gained") {
        p <- p + list(
          geom_vline(xintercept = 10, color = "yellow", linewidth = 1),
          geom_vline(xintercept = 0,  color = "blue",   linewidth = 1)
        )
      }
    }
    
    p + geom_point()
        
      })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
