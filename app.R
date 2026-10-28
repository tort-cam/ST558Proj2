library(shiny)
library(shinyalert)
library(tidyverse)
library(RKaggle)
library(shinyWidgets)
library(DT)

df <- RKaggle::get_dataset("maxhorowitz/nflplaybyplay2009to2016")
plays <- df[[1]]
remove(df)
plays <- plays|>
  select(c(1:67, 102))|>
  mutate(Location = coalesce(PassLocation, RunLocation))|>
  filter(PlayType %in% c("Pass", "Run"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(title = "ST558 Project 2", windowTitle = "Test"),
             
   sidebarLayout(
     sidebarPanel(
       h3("Select your X variable"),
       
       selectizeInput(inputId = "xvar",
                      label = "X Variable",
                      choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                      selected = "Yards.Gained"),
       
       h3("Select a Y variable"),
       
       selectizeInput(inputId = "yvar",
                      label = "Y Variable",
                      choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                      selected = "Yards.Gained"),
       
       h3("Select a category"),
       
       selectizeInput(inputId = "cat",
                      label = "Categorical Variable",
                      choices = c("Play Type" = "PlayType", "Section of Field" = "Location", "Quarter" = "qtr"),
                      selected = "PlayType"),
       
       h3("Choose a subset of the data"),
       
       ## Play type
       
       selectizeInput(inputId = "Season",
                      label = "Season",
                      choices = c("All" = "all", 2009:2016),
                      selected = "all"),
       
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
       
       h3("Subset Y Variable (optional)"),
       
       sliderInput(inputId = "y_range",
                   label = NULL,
                   min = 0,
                   max = 100,
                   value = c(0,100)),
       
       actionButton("update","Show Me!"),
       
       h3("Not enough color for you?"),
       switchInput(inputId = "football",label = "Football Mode?", value = FALSE)
     ),
     
     mainPanel(
       tabsetPanel(
         id = "tabs",
         
         tabPanel("About",
                  h3("Hello! This app was made by Cameron Mullaney for ST558 Project 2!"),
                  p("This app explores passing and runnings plays in the NFL from 2009-2016. This data comes from" ,
                    a("NFL Play by Play 2009-2016 dataset on Kaggle",
                      href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data",
                      target = "_blank"),
                    "and was created by a group of Carnegie Mellon University researchers by scraping the 
                      NFL's official API - It includes far more data than I explore here.\n
                     The sidebar on the Custom Visualization allows you to select and subset the data. 
                     You can choose to subset by year, play location, and quarter, and further subset
                     by setting ranges for your selected x and y variables."),
                  p("The Custom Visualization tab allows for custom data exploration, as outlined above. 
                     Data Download provides a link to download the current subset of the data you've selected 
                     in the Custom Visualization tab. Data Exploration contains a collection of pre-made data 
                     summaries made by me. \n\n MORE WILL NEED TO GO HERE ONCE I MAKE THAT PAGE."),
                  img(src = "https://toppng.com/uploads/preview/nfl-logo-vector-1157397131993ne6xvvwg.png", width = "300px")
         ),
         
         tabPanel("Custom Visualization",
         plotOutput("plot")
        ),
   
          
          tabPanel("Data Download",
                    h3("Download the data currently being used on 'Custom Visualization'"),
                    downloadButton("download_data", "Download data (CSV)"),
                    p(""),
                    DTOutput("data_table")
           ),
           
          tabPanel("Data Exploration",
                    h3("Placeholder"),
                    p("Placeholder")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
      req(input$location)
      
      d <- plays
      if (input$Season != "all") d <- filter(d, Season == input$Season)
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
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste('NFLdata.csv')
    },
    content = function(file) {
      write.csv(subset_data(), file, row.names = FALSE)
    }
  )
  
  output$data_table <- renderDT({
    validate(need(input$update > 0,
                  "Click 'Show Me!' on the Custom Visualization tab first :)"))
    subset_data()
  })
  
  my_plot <- eventReactive(input$update, {
    d <- subset_data() |>
      filter(!is.na(.data[[input$cat]]))
    req(nrow(d) > 0)
    
    d$group <- as.factor(d[[input$cat]])
    
    p <- ggplot(d, aes(x = .data[[input$xvar]],
                       y = .data[[input$yvar]],
                       color = group))
    
    if (input$football) {
      p <- p + theme(
        panel.background = element_rect(fill = "darkgreen"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank()
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
    
    p + geom_point(alpha = 0.3, size = 1, position = "jitter")
  })
  
  output$plot <- renderPlot({
    validate(
      need(input$update > 0,
           "Select your variables, then click 'Show Me!'")
    )
    my_plot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
