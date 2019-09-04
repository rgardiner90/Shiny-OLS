library(shiny)
library(magrittr)
library(dplyr)


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Code Free Exlporatory OLS"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c(".csv")),
      
      helpText("This panels to the right show an example dataset, your dataset (after it is loaded), and",
               "model results for an OLS model. The questions below reduce the likelihood of getting",
               "inappropriate answers about an OLS model. The questions deserve serious consideration.",
               "If you answer yes to the three questions below, you will see OLS results in the",
               "'Model Output' tab."),

      checkboxInput("header", "The first row includes variable names", TRUE),
  
      radioButtons("first", "Is the first variable your dependent variable?",
                   choices = c("Yes", "No"),
                   selected = "No"),
      
      radioButtons("continuous", "Is your dependent variable continuous?",
                   choices = c("Yes", "No"),
                   selected = "No"),
      radioButtons("obs", "Do you have at least 30 observations",
                   choices = c("Yes", "No"),
                   selected = "No"),
      radioButtons("sig", "What threshold for significance would you like?",
                   choices = c(0.1, 0.05, 0.001),
                   selected = 0.05)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Example Dataset", tableOutput("example")),
                  tabPanel("Your Dataset", tableOutput("data")),
                  tabPanel("Model Output", tableOutput("contents"))
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    first <- ifelse(input$first == "No", 
                    "WARNING: This version requires that the first column be your dependent variable",
                    NA)
    continuous <- ifelse(input$continuous == "No", 
                         "WARNING: OLS won't work when your dependent variable is not continuous",
                         NA)
    obs <- ifelse(input$obs == "No", "WARNING: You really shouldn't run a regression with too few observations",
                  NA)
    
    errors <- rbind(first, continuous, obs) %>%
      as_tibble() %>%
      dplyr::rename(Warnings = 1) %>%
      na.omit()
    
    
    
    if(input$first == "No") {
      return(errors)
    }
    else {
      if(input$continuous == "Yes") {
        
        if(input$obs == "Yes") {
          
          model <- lm(df[, 1] ~ ., data = df[, -1])
          
          broom::tidy(model) %>%
            dplyr::mutate(term = ifelse(term == "(Intercept)", "Intercept", term),
                          significance = ifelse(p.value < as.numeric(input$sig), "Significant", "Not Significant")) %>%
            dplyr::select(-std.error, -statistic, -p.value) %>%
            dplyr::rename(variable = term,
                          influence = estimate)
          
        } else {
          return(errors)
        }
      } else {
        return(errors)
      }
      
    }
    
    
    
  })
  
  output$data <- renderTable({
    
    req(input$file1)
    
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    
    tibble::as_tibble(head(df, 20))
    
  })
  
  
  output$example <- renderTable({
    
    mtcars %>%
      dplyr::mutate(`mpg (DV)` = mpg) %>%
      dplyr::select(-mpg) %>%
      dplyr::select(`mpg (DV)`, everything())
    
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)