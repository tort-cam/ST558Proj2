library(shiny)
library(tidyverse)
library(patchwork)
library(RKaggle)
library(rsconnect)
library(shinyWidgets)
library(DT)

#df <- RKaggle::get_dataset("maxhorowitz/nflplaybyplay2009to2016")
#plays <- df[[1]]
#remove(df)

## This was for ease of testing
plays <- read_csv("NFL Play by Play 2009-2016 (v3).csv")

## QMD item, needed for premade plotting
columnsOfInterest <- c(3, 5, 16, 21, 23, 33, 36, 37, 39, 40)

## Create a shared "Location" column - these columns only coincide
## ~20 out of the 360,000 columns, so it's not a serious issue.

plays <- plays|>
  select(1:67, 102)|>
  mutate(Location = coalesce(PassLocation, RunLocation))|>
  filter(PlayType %in% c("Pass", "Run"))

## Lists for later
num_vars <- c("Yards Gained" = "Yards.Gained",
              "Time Under Center" = "TimeUnder",
              "Yards to Go" = "ydstogo")

cat_vars <- c("Play Type" = "PlayType",
              "Section of Field" = "Location",
              "Quarter" = "qtr",
              "Season" = "Season")

## Creating a graph-making widget for Data Exploration

graphUI <- function(id) {
  ns <- NS(id)
  div(id = ns("box"),
      wellPanel(
        fluidRow(
          column(9, h4("Figure")),
          ## Remove button off to the side
          column(3, actionButton(ns("remove"), "Remove", class = "btn-sm"))
        ),
        selectInput(ns("type"), "Plot type",
                    choices = c("Histogram" = "hist", "Density" = "dens",
                                "Boxplot" = "box", "Bar chart (counts)" = "bar",
                                "Scatterplot" = "scatter")),
        
        uiOutput(ns("options")), 
        
        selectInput(ns("fill"),  "Color by", choices = c("None" = "none", cat_vars)),
        selectInput(ns("facet"), "Facet by", choices = c("None" = "none", cat_vars))
      ),
      plotOutput(ns("plot"), height = "450px")
  )
}

## Actual graph-making function

graphServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns   
    
    output$options <- renderUI({
      switch(input$type,
             hist = tagList(
               selectInput(ns("num"), "Numeric variable", choices = num_vars),
               sliderInput(ns("bins"), "Number of bins", min = 5, max = 100, value = 40)
             ),
             dens = selectInput(ns("num"), "Numeric variable", choices = num_vars),
             box  = tagList(
               selectInput(ns("num"), "Numeric variable", choices = num_vars),
               selectInput(ns("cat"), "Categorical variable (x-axis)", choices = cat_vars)
             ),
             bar  = selectInput(ns("cat"), "Categorical variable (x-axis)", choices = cat_vars),
             scatter = tagList(
               selectInput(ns("x"), "X variable", choices = num_vars),
               selectInput(ns("y"), "Y variable", choices = num_vars, selected = "ydstogo")
             )
      )
    })
    
    output$plot <- renderPlot({
      d <- data()
      
      ## Wait until the controls for the chosen type exist
      switch(input$type,
             hist = , 
             dens = req(input$num),
             box = req(input$num, input$cat),
             bar = req(input$cat),
             scatter = req(input$x, input$y))
      
      if (input$type == "hist") req(input$bins)
      
      ## Clip extreme outliers
      
      clip <- function(v) quantile(d[[v]], c(0.01, 0.99), na.rm = TRUE)
      
      ## Different settings for each plot type
      
      p <- switch(input$type,
                  hist = ggplot(d, aes(x = .data[[input$num]])) +
                    geom_histogram(bins = input$bins, alpha = 0.7, position = "identity") +
                    coord_cartesian(xlim = clip(input$num)),
                  
                  dens = ggplot(d, aes(x = .data[[input$num]])) +
                    geom_density(alpha = 0.5) +
                    coord_cartesian(xlim = clip(input$num)),
                  
                  box = ggplot(d, aes(x = .data[[input$cat]], y = .data[[input$num]])) +
                    geom_boxplot() +
                    coord_cartesian(ylim = clip(input$num)),
                  
                  bar = ggplot(d, aes(x = .data[[input$cat]])) +
                    geom_bar(position = "dodge") +
                    labs(y = "Number of plays"),
                  
                  scatter = {
                    validate(need(input$x != input$y, "Pick different X and Y variables."))
                    d |> slice_sample(n = min(5000, nrow(d))) |>
                      ggplot(aes(x = .data[[input$x]], y = .data[[input$y]])) +
                      geom_point(alpha = 0.3, position = "jitter")
                  }
      )
      
      ## Optional color & fill grouping
      if (input$fill != "none") {
        p <- if (input$type == "scatter") p + aes(color = .data[[input$fill]])
        else p + aes(fill = .data[[input$fill]])
        p <- p + labs(fill = input$fill, color = input$fill)
      }
      
      ## Optional faceting
      if (input$facet != "none") p <- p + facet_wrap(input$facet)
      
      p
    })
    
    ## Check remove button
    observeEvent(input$remove, {
      removeUI(selector = paste0("#", ns("box")), immediate = TRUE)
    })
  })
}

## Widget to get table settings

tableUI <- function(id) {
  ns <- NS(id)
  div(id = ns("box"),
      wellPanel(
        fluidRow(
          column(9, h4("Table")),
          column(3, actionButton(ns("remove"), "Remove", class = "btn-sm"))
        ),
        radioButtons(ns("type"), "Table type",
                     choices = c("Numeric summary" = "summ",
                                 "Contingency table" = "ct"),
                     inline = TRUE),
        
        uiOutput(ns("options"))
      ),
      DTOutput(ns("table"))
  )
}

## Function for actually building the tables

tableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$options <- renderUI({
      
      ## Simpler settings here than for figures
      switch(input$type,
             summ = tagList(
               selectInput(ns("var"), "Numeric variable", choices = num_vars),
               selectInput(ns("group"), "Group by",
                           choices = c("None" = "none", cat_vars),
                           selected = "PlayType"),
               checkboxGroupInput(ns("stats"), "Statistics",
                                  choices = c("n", "mean", "median", "sd", "min", "max", "IQR"),
                                  selected = c("n", "mean", "median"),
                                  inline = TRUE)
             ),
             ct = tagList(
               selectInput(ns("row"), "Rows", choices = cat_vars, selected = "PlayType"),
               selectInput(ns("col"), "Columns", choices = cat_vars, selected = "qtr"),
               radioButtons(ns("show"), "Show",
                            choices = c("Counts" = "n", "Row proportions" = "prop"),
                            inline = TRUE)
             )
      )
    })
    
    ## Table printing in Data Exploration
    
    output$table <- renderDT({
      d <- data()
      validate(need(nrow(d) > 0, "No plays match the current subset."))
      
      if (input$type == "summ") {
        req(input$var, input$group, input$stats)
        
        fns <- list(
          n      = ~sum(!is.na(.x)),
          mean   = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd     = ~sd(.x, na.rm = TRUE),
          min    = ~min(.x, na.rm = TRUE),
          max    = ~max(.x, na.rm = TRUE),
          IQR    = ~IQR(.x, na.rm = TRUE)
        )
        
        if (input$group != "none") d <- group_by(d, across(all_of(input$group)))
        
        d |>
          summarise(across(all_of(input$var), fns[input$stats], .names = "{.fn}"),
                    .groups = "drop") |>
          mutate(across(where(is.double), \(x) round(x, 2)))
        
      } else {
        req(input$row, input$col, input$show)
        validate(need(input$row != input$col,
                      "Pick different row and column variables."))
        
        tab <- d |> count(across(all_of(c(input$row, input$col))))
        
        if (input$show == "prop") {
          tab <- tab |>
            group_by(across(all_of(input$row))) |>
            mutate(n = round(n / sum(n), 3)) |>
            ungroup()
        }
        
        pivot_wider(tab, names_from = all_of(input$col),
                    values_from = n, values_fill = 0)
      }
    }, options = list(dom = "t"))
    
    observeEvent(input$remove, {
      removeUI(selector = paste0("#", ns("box")), immediate = TRUE)
    })
  })
}

## Okay! Finally time to make the UI!
ui <- fluidPage(
  titlePanel(title = "ST 558 Project 2 - Cameron Mullaney", windowTitle = "ST 558 Project 2 - Mullaney"),
             
   sidebarLayout(
     sidebarPanel(
       h4("Select your X variable"),
       
       selectizeInput(inputId = "xvar",
                      label = NULL,
                      choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                      selected = "Yards.Gained"),
       
       h4("Select a Y variable"),
       
       selectizeInput(inputId = "yvar",
                      label = NULL,
                      choices = c("Yards Gained"= "Yards.Gained", "Time Under Center" = "TimeUnder", "Yards to Go" = "ydstogo"),
                      selected = "ydstogo"),
       
       h4("Select a category"),
       
       selectizeInput(inputId = "cat",
                      label = NULL,
                      choices = c("Play Type" = "PlayType", "Section of Field" = "Location", "Quarter" = "qtr"),
                      selected = "PlayType"),
       
       h4("Choose a subset of the data"),
       p("You can choose a season, play location, and quarter"),
       
       ## Season
       
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
                    choiceNames = c("All", "First", "Second", "Third", "Fourth", "Overtime [5]"),
                    choiceValues = c("all", "1", "2", "3", "4", "5"),
                    selected = "all"),
       
       h4("Subset X Variable (optional)"),
       
       sliderInput(inputId = "x_range",
                   label = NULL,
                   min = 0,
                   max = 100,
                   value = c(0,100)),
       
       h4("Subset Y Variable (optional)"),
       
       sliderInput(inputId = "y_range",
                   label = NULL,
                   min = 0,
                   max = 100,
                   value = c(0,100)),
       
       actionButton("update","Show Me!"),
       
       p("Want more thematic graphs?"),
       switchInput(inputId = "football",label = "Football Mode", value = FALSE)
     ),
     
     ## About tab
     mainPanel(
       tabsetPanel(
         id = "tabs",
         
         tabPanel("About",
                  h4("Hello! This app was made by Cameron Mullaney for ST558 Project 2!"),
                  p("This app explores passing and runnings plays in the NFL from 2009-2016. This data comes from" ,
                    a("NFL Play by Play 2009-2016 dataset on Kaggle",
                      href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data",
                      target = "_blank"),
                    "and was created by a group of Carnegie Mellon University researchers by scraping the 
                      NFL's official API - It includes far more data than I explore here."),
                  p("The sidebar on the Subset Scatterplot allows you to select and subset the data. 
                     You can choose to subset by year, play location, and quarter, and further subset
                     by setting ranges for your selected x and y variables."),
                  p("The Subset Scatterplot tab allows for custom data exploration, as outlined above."),
                  p("Data Download provides a link to download the current subset of the data you've selected 
                     in the Subset Scatterplot tab."),
                  p("Data Exploration lets you subset and visualize in greater 
                    detail, as opposed to the scatterplot format of Subset Scatterplot. Here, you can also see
                    the figures I've made in my .qmd file by clicking the 'pre-made' boxes. Even though 
                    they are pre-made, they still use the data subset chosen on the sidebar!"),
                  img(src = "https://cdn.freebiesupply.com/logos/large/2x/nfl-logo-bw.png", width = "300px")
         ),
         
         ## Subset plot
         
         tabPanel("Subset Scatterplot",
         plotOutput("plot")
        ),
   
          
          tabPanel("Data Download",
                    h4("Download the data currently being used on 'Subset Scatterplot'"),
                    downloadButton("download_data", "Download data (CSV)"),
                    hr(),
                    DTOutput("data_table")
           ),
           
          tabPanel("Data Exploration",
                    h4("What do you want to see?"),
                    checkboxGroupInput(inputId = "figureType", 
                                       label = NULL,
                                       choices = c("Graphics (Custom)" = "graph", 
                                                   "Numeric (Custom)"= "num",
                                                   "Graphics (Pre-made)" = "pgraph",
                                                   "Numeric (Pre-made)" = "pnum")),
                    hr(),
                    uiOutput("graph_box"), # Custom figures
                    uiOutput("num_box"), # Custom tables
                    uiOutput("pgraph_box"), # Pre-made figures
                    uiOutput("pnum_box") # Pre-made tables
                   
        )
      )
    )
  )
)

## Server setup time!

server <- function(input, output, session) {
  
  ## Subsetting data based on sidebar
  filtered_data <- reactive({
    
      req(input$location)
      
      d <- plays
      if (input$Season != "all") d <- filter(d, Season == input$Season)
      if (input$quarter   != "all")  d <- filter(d, qtr == as.numeric(input$quarter))
      
      if (length(input$location) < 3) d <- filter(d, Location %in% input$location)
      
      d
    
  })
  
  ## Set x_range slider
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
  
  ## Set y_range slider
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
  
  ## Subset data to selected x and y ranges
  
  subset_data <- eventReactive(input$update, {
    filtered_data() |>
      filter(!is.na(.data[[input$xvar]]),
             .data[[input$xvar]] >= input$x_range[1],
             .data[[input$xvar]] <= input$x_range[2],
             !is.na(.data[[input$yvar]]),
             .data[[input$yvar]] >= input$y_range[1],
             .data[[input$yvar]] <= input$y_range[2])
  })
  
  ## CSV download
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste('NFLdata.csv')
    },
    content = function(file) {
      write.csv(subset_data(), file, row.names = FALSE)
    }
  )
  
  ## Data Download table render
  output$data_table <- renderDT({
    validate(need(input$update > 0,
                  "Click 'Show Me!' on the Subset Scatterplot tab first :)"))
    subset_data()
  })
  
  ## Custom Viz scatterplot
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
  
  ## Render plot from above
  
  output$plot <- renderPlot({
    validate(
      need(input$update > 0,
           "Select your variables, then click 'Show Me!'")
    )
    my_plot()
  })
  
  ## Data Exploration df
  explore_data <- reactive({
    validate(need(input$update > 0,
                  "Choose your subset in the sidebar, then click 'Show Me!'"))
    subset_data() |>
      mutate(across(c(qtr, Season), as.factor))
  })
  
  ## Button to add custom figure
  output$graph_box <- renderUI({
    req("graph" %in% input$figureType)
    tagList(
      actionButton("add_graph", "+ Add a figure"),
      br(),
      div(id = "graph_area")
    )
  })
  
  ## Button to add custom table
  output$num_box <- renderUI({
    req("num" %in% input$figureType)
    tagList(
      actionButton("add_table", "+ Add a table"),
      br(),
      div(id = "table_area")
    )
  })
  
  ## Adding figures
  g_count <- reactiveVal(0)
  
  observeEvent(input$add_graph, {
    g_count(g_count() + 1)
    id <- paste0("graph", g_count())
    insertUI(selector = "#graph_area", where = "beforeEnd", ui = graphUI(id))
    graphServer(id, explore_data)
  })
  
  ## Adding tables
  t_count <- reactiveVal(0)
  
  observeEvent(input$add_table, {
    t_count(t_count() + 1)
    id <- paste0("table", t_count())
    insertUI(selector = "#table_area", where = "beforeEnd", ui = tableUI(id))
    tableServer(id, explore_data)
  })
 
  ## Render premade figures
  output$pgraph_box <- renderUI({
    req("pgraph" %in% input$figureType)
    tagList(
      h4("Pre-made graphics"),
      plotOutput("pre_playByDown"),br(),
      plotOutput("pre_downByPlay"),br(),
      plotOutput("pre_offEff"),br(),
      p("      Passing yards gained before and after catch, by pass location"),
      plotOutput("pre_passes"),br(),
      plotOutput("pre_football"),br(),
      plotOutput("pre_penalties")
    )
  })
  
  ## Render premade tables
  output$pnum_box <- renderUI({
    req("pnum" %in% input$figureType)
    tagList(
      h4("Pre-made tables"),
      p("Accepted Penalties"),
      DTOutput("pre_penTable"), br(),
      p("Play Type Frequency by Down"),
      
      DTOutput("pre_playTable"), br(),
      p("Seasonal Offensive Stats"),
      
      DTOutput("pre_effTable"), br(),
      p("Pass Efficiency by Location"),
      
      DTOutput("pre_passLocTable")
    )
  })
  
  ## ALL PREMADE FIGURES AND TABLES ARE BELOW ##
  output$pre_penTable <- renderDT({
    id <- showNotification("Building the tables...",
                           duration = 25, closeButton = FALSE)
    on.exit(removeNotification(id))
    subset_data() |>
      ## Many PenaltyType cells have unnecessary commas and additional info, 
      ## this line shortens those cells' text
      mutate(PenaltyType = str_extract(PenaltyType, "^[^,]+"))|>
      
      ## Penalties only
      filter(Accepted.Penalty == 1)|>
      group_by(PenaltyType)|>
      summarize(count = n())|>
      arrange(desc(count)) |>
      drop_na() |>
      mutate(PenaltyType = factor(PenaltyType, levels = unique(PenaltyType)))
  })
  
  output$pre_playTable <- renderDT({
    subset_data()|>
      
      # Drop NA values in down
      filter(!is.na(down))|>
      
      # Get counts for each play type in each down
      count(PlayType, down) |>
      
      # Get count values for each play type regardless of down
      group_by(PlayType)|>
      mutate(total = sum(n))|>
    
      # Get proportions for each play type per down
      group_by(down) |>
      mutate(prop = round(n / sum(n) * 100, 2))|>
      ungroup()|>
      
      # Drop n from the output
      select(-n)|>
      
      # Format the final table
      pivot_wider(names_from = down, 
                  values_from = prop,
                  values_fill = 0)|>
      arrange(desc(total)) |>
      select(-total)
  })
  
  output$pre_effTable <- renderDT({
    subset_data()|>
      
      ## JAX and JAC are both Jacksonville (Bortles!)
      ## Rams moved to LA
      mutate(posteam = if_else(posteam == "STL", "LA", posteam),
             posteam = if_else(posteam == "JAC", "JAX", posteam))|>
      
      ## Dropping interceptions because defensive TDs aren't my focus here
      filter(InterceptionThrown == 0) |>
      group_by(Season, posteam)|>
      summarize(tds = sum(Touchdown), 
                passes = sum(PlayType == "Pass"),
                runs = sum(PlayType == "Run"))|>
      drop_na()
  })
  
  output$pre_passLocTable <- renderDT({
    subset_data()|>
      filter(PlayType == "Pass" & PassOutcome == "Complete")|>
      select(all_of(columnsOfInterest))|>
      drop_na()|>
      group_by(PassLocation)|>
      summarize("Mean air yards" = mean(AirYards),
                "Air yards sd" = sd(AirYards),
                "Median air yards" = median(AirYards),
                "Mean ground yards" = mean(YardsAfterCatch),
                "Ground yards sd" = sd(YardsAfterCatch),
                "Median ground yards" = median(YardsAfterCatch))
  })
  
  output$pre_playByDown <- renderPlot({
    id <- showNotification("Building the figures...",
                           duration = 25, closeButton = FALSE)
    on.exit(removeNotification(id))
    
    subset_data() |>
      
      # Drop NA values in down
      filter(!is.na(down))|>
      
      # Get counts for each play type in each down
      count(PlayType, down) |>
      
      # Get count values for each play type regardless of down
      group_by(PlayType)|>
      mutate(total = sum(n))|>
      group_by(down) |>
      mutate(prop = round(n / sum(n) * 100, 2))|>
      ungroup()|>
      
      filter(PlayType %in% c("Pass", "Run", "Sack", "Punt", "Field Goal")) |>
      mutate(down = as.factor(down),
             PlayType = factor(PlayType,
                               levels = c("Pass", "Run", "Sack", "Field Goal", "Punt"))
      )|>
      ggplot(aes(x = down, y = prop, fill = PlayType)) +
      geom_col(position = "stack") +
      labs(x = "Down", y = "Play type distribution", title = "Play type distribution by down") +
      theme_minimal()
  })
  
  output$pre_downByPlay <- renderPlot({
    subset_data() |>
      
      # Drop NA values in down
      filter(!is.na(down))|>
      
      # Get counts for each play type in each down
      count(PlayType, down) |>
      
      # Get count values for each play type regardless of down
      group_by(PlayType)|>
      mutate(total = sum(n))|>
      
      filter(PlayType %in% c("Pass", "Run", "Sack", "Punt", "Field Goal")) |>
      mutate(down = as.factor(down),
             PlayType = factor(PlayType,
                               levels = c("Pass", "Run", "Sack", "Field Goal", "Punt")))|>
      group_by(PlayType) |>
      mutate(prop = round(n / sum(n) * 100, 2))|>
      ungroup()|>
      
      ggplot(aes(x = down, y = prop, fill = down)) +
      geom_col() +
      facet_wrap(~ PlayType, nrow = 1) +
      
      labs(x = NULL, y = "Percent of plays on that down", title = "Down distribution by play type") +
      theme_minimal()
  })
  
  output$pre_offEff <- renderPlot({
    subset_data() |>

      ## JAX and JAC are both Jacksonville (Bortles!)
      ## Rams moved to LA
      mutate(posteam = if_else(posteam == "STL", "LA", posteam),
             posteam = if_else(posteam == "JAC", "JAX", posteam))|>
      ## Dropping interceptions because defensive TDs aren't my focus here
      filter(InterceptionThrown == 0) |>
      group_by(GameID, posteam, DefensiveTeam)|>
      summarize(tds = sum(Touchdown), 
                passes = sum(PlayType == "Pass"),
                runs = sum(PlayType == "Run"))|>
      drop_na()|>
      
      group_by(posteam)|>
      summarize(total_tds = sum(tds),
                total_offensive_plays = sum(passes) + sum(runs))|>
      ggplot(aes(x = total_offensive_plays, 
                 y = total_tds, 
                 label = posteam))+
      geom_text()+
      labs(x = "Total offensive plays", 
           y = "Total touchdowns", 
           title = "Offensive Efficiency")
  })
  
  output$pre_passes <- renderPlot({
    passes <- subset_data() |>
      filter(PlayType == "Pass" & PassOutcome == "Complete")|>
      select(all_of(columnsOfInterest))|>
      drop_na()
    
    air <- passes|>
      group_by(PassLocation)|>
      ggplot(aes(x = PassLocation, y = AirYards))+
      ylim(-25,100)+
      geom_boxplot()+
      labs(x = "Pass Location", y = "Air Yards Gained")
    
    ground <- passes|>
      group_by(PassLocation)|>
      ggplot(aes(x = PassLocation, y = YardsAfterCatch))+
      ylim(-25,100)+
      geom_boxplot()+
      labs(x = "Pass Location", y = "Yards after catch")
    
    air | ground
  })
  
  output$pre_football <- renderPlot({
    passes <- subset_data() |>
      filter(PlayType == "Pass" & PassOutcome == "Complete")|>
      select(all_of(columnsOfInterest))|>
      drop_na()
    
    passes|>
      ggplot(aes(x = PassLocation, y = Yards.Gained))+
      geom_hline(yintercept = 10, color = "yellow", linewidth = 1)+
      geom_hline(yintercept = 0, color = "blue", linewidth = 1)+
      geom_boxplot(outlier.color = "white", 
                   outlier.size = .75, 
                   outlier.alpha = .2, 
                   fill = "chocolate4", 
                   color = "white")+
      
      scale_y_continuous(limits = c(-25, 100),
                         breaks = seq(-20, 100, by = 10)) +
      theme(
        panel.background   = element_rect(fill = "darkgreen"),
        panel.grid.major.y = element_line(color = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank()
      )+
      labs(x = "Pass Location", y = "Yards Gained", title = "Yards gained by pass location [Football edition]")
  })
  
  output$pre_penalties <- renderPlot({
    subset_data() |>
      ## Many PenaltyType cells have unnecessary commas and additional info, 
      ## this line shortens those cells' text
      mutate(PenaltyType = str_extract(PenaltyType, "^[^,]+"))|>
      
      ## Penalties only
      filter(Accepted.Penalty == 1)|>
      group_by(PenaltyType)|>
      summarize(count = n())|>
      arrange(desc(count)) |>
      drop_na() |>
      mutate(PenaltyType = factor(PenaltyType, levels = unique(PenaltyType)))|>
      ggplot(aes(x = count, y = PenaltyType))+
      geom_bar(stat = "identity", color = "#595959", linewidth = .1)+
      geom_text(aes(label = count), hjust = -.2, size = 3)+
      labs(x = "Number of times penalty was accepted", 
           y = "Type of penalty",
           title = "Penalty Type Frequency")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
