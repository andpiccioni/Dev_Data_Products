#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

require(shinycssloaders)


# List of choices for selectInput
towns <- read.csv("towns.csv")
cities <- as.list(towns[,2])
contract <- c("Full-Time", "Part-Time", "Permanent", "Temporary", "Contract", "Apprenticeship", "Internship", "Commission")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Data Science Jobs on Indeed.co.uk"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Search", br(),
       sliderInput("pay",
                   "Salary per year",
                   min = 0,
                   max = 150000,
                   value = c(0,30000), step = 1000,
                   pre = "£"),
       
       # Job type
       selectInput('in5', 'Job Type', c('Choose'='', contract), multiple = TRUE, selectize=TRUE),
       
       # Dropdown list for UK main cities
       selectInput('in6', 'Location', c('Select a city'='', cities), selectize=FALSE),
       submitButton("Submit")
    ),
    tabPanel("Documentation", br(),"This app returns the jobs advertised at indeed.co.uk for data science.", br(),
             "You can set the salary or its range, and the types of jobs for each of the main cities in UK.", br(),
             "Click on submit to start the search; and please be patient at the beginning because the first search might take a while.",br(),
             "A histogram with the number of jobs per town will be displayed on top; and the table with the relevant jobs' details at the bottom.", br(),
             "Good Luck with your career in Data science!"),
    tabPanel("About", br(),"This is a project for the course", em("Developing Data Products"), "for the Data Science Specialisation in coursera.org.",br(),br(),
             "Created by Andrea Piccioni."))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      withSpinner(plotOutput("distPlot")),
      withSpinner(DT::dataTableOutput("values"))
    )
  )
))
