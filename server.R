#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


require(rvest)
require(xml2)
require(XML)
require(dplyr)
require(stringr)
require(ggplot2)
require(DT)


job_search <- function(query, loc, job_type, pay) {
  require(rvest)
  library(XML)
  require(dplyr)
  session <- html_session("https://www.indeed.co.uk/advanced_search")
  form <- html_form(session)[[1]]
  form <-
    set_values(
      form,
      as_and = query,
      l = loc,
      jt = job_type,
      salary = pay
    )
  
  
  # running the crawl to find the number of pages per search
  submit_form1 <- function(session, form) {
    url <- XML::getRelativeURL(form$url, session$url)
    url <- paste(url, '?', sep = '')
    values <- as.vector(rvest:::submit_request(form)$values)
    att <- names(values)
    if (tail(att, n = 1) == "NULL") {
      values <- values[1:length(values) - 1]
      att <- att[1:length(att) - 1]
    }
    q <- paste(att, values, sep = '=')
    q <- paste(q, collapse = '&')
    q <- gsub(" ", "+", q)
    url <- paste(url, q, sep = '')
    html_session(url)
  }
  
  main_session <- submit_form1(session, form)
  
  qjobs <- main_session %>%
    html_nodes("#searchCount") %>%
    html_text() %>%
    strsplit(" ")
  
  

  # num_jobs is a list that we need to split, in order to select the last element
  
  while(length(qjobs)>0){
    tot_jobs <- qjobs[[1]][length(qjobs[[1]])]
    tot_jobs <- as.factor(tot_jobs)
    tot_jobs <- as.numeric(gsub(",", "", tot_jobs))
    # creating the sequences for the for loop
    # number of pages displaying results of the search
    if (tot_jobs < 1000) {
      num_pages <- seq(0, tot_jobs + 9, by = 10)
    } else{
      (num_pages <- seq(0, 990, by = 10))
    }
    
    
    # create an empty data frame
    indeed_jobs <- data.frame()
    
    # version 2 of submit form
    
    for (num_page in num_pages) {
      submit_form2 <- function(session, form) {
        url <- XML::getRelativeURL(form$url, session$url)
        url <- paste(url, '?', sep = '')
        values <- as.vector(rvest:::submit_request(form)$values)
        att <- names(values)
        if (tail(att, n = 1) == "NULL") {
          values <- values[1:length(values) - 1]
          att <- att[1:length(att) - 1]
        }
        q <- paste(att, values, sep = '=')
        q <- paste(q, collapse = '&')
        q <- gsub(" ", "+", q)
        q <- paste0(q, '&start=', num_page)
        url <- paste(url, q, sep = '')
        html_session(url)
      }
      
      
      # Submit form and get new url
      session1 <- submit_form2(session, form)
      
      # Store web url
      data_sci_indeed <- session1
      
      # Get job titles
      job_title <- data_sci_indeed %>%
        html_nodes(".jobtitle") %>%
        html_text()
      
      # Get companies
      company <- data_sci_indeed %>%
        html_nodes(".company") %>%
        html_text()
      
      # Get locations
      location <- data_sci_indeed %>%
        html_nodes(".location") %>%
        html_text()
      
      # Get descriptions
      description <- data_sci_indeed %>%
        html_nodes(".summary") %>%
        html_text()
      
      # Get the links
#      link <- data_sci_indeed %>%
#        html_nodes(".jobtitle") %>%
#        html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "jobtitle", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "turnstileLink", " " ))]') %>%
#        html_attr("href")
#      link <- paste0('https://www.indeed.co.uk/', link)
      
      # job not advertised 
      jn_x <-  '//*[contains(concat( " ", @class, " " ), concat( " ", "jobtitle", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "turnstileLink", " " ))]'
      
      rc_link <- data_sci_indeed %>%
        html_nodes(xpath = jn_x) %>%
        html_attr("href")      
      
      # any job
      link <- data_sci_indeed %>%
        html_nodes(".jobtitle") %>%
        html_attr("href")
      
      link[is.na(link)] <- rc_link
      link <- paste('https://www.indeed.co.uk', link, sep = '')
      
      df <- data.frame(job_title, company, location, description, link)
      indeed_jobs <- rbind(indeed_jobs, df)
      unique_jobs <- indeed_jobs %>% distinct
      
      salary <- rep(pay, nrow(unique_jobs))
      
      job_posts <- cbind(unique_jobs, salary)
      
      
    } # end of the for loop
    return(job_posts)
    
  }
} # end of the function

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  # Reactive expression to create data frame of the results ----
  searchResults <- reactive({

    salary <- paste(input$pay, collapse = "-")
    city <- paste(input$in6, collapse = ",")
    jobtype <- paste(input$in5, collapse = ",")
   
    all_posts <- job_search("data science", city, jobtype, salary)
    all_posts$Postcode <- str_extract(all_posts$location,'\\s[[:upper:]]{1,2}\\d.?')
    all_posts$location <- gsub('\\s[[:upper:]]{1,2}\\d.?',' ', all_posts$location)
    all_posts$job_title <- paste0("<a href='", all_posts$link,"'target='_blank'>",all_posts$job_title,"</a>")
    all_posts <- subset(all_posts, select = -c(salary, link))
    as.data.frame(all_posts)
    
  })
  
  # display plot
    output$distPlot <- renderPlot({  
      ggplot(searchResults(), aes(location))+ geom_bar() +
        theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank()) +
        ggtitle("Jobs by location") + 
        ylab("Number of Jobs") + coord_flip()
  
    })
   

  # return total number of jobs
#  output$total <- renderText(nrow(searchResults()))
    
  # Show the values in an HTML table ----
  
  output$values <- DT::renderDT({
   DT::datatable(searchResults(), escape = 1)
 })
#  output$values <- renderTable({
#    searchResults()
#  })
})

