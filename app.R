library(shiny)
library(ggplot2)
library(plotly)
library(lubridate)
library(DT)
library(stringr)
library(jsonlite)

# Load data
source("./Extract_data_from_web.R")

smy_date_ctl <- readRDS("smy_date_ctl.rds")
smy_date_airport <- readRDS("smy_date_airport.rds")
smy_categ_ct <- readRDS("smy_categ_ct.rds")
avg_eff_time <- readRDS("avg_eff_time.rds")

# Custom color palette
palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a",
             "#66a61e", "#e6ab02", "#a6761d", "#666666",
             "#1f78b4", "#b2df8a", "#fb9a99", "#fdbf6f",
             "#cab2d6", "#ffff99", "#b15928")


extract_advisory_text <- function(detail_list) {
  tryCatch({
    flattened <- unlist(detail_list, recursive = TRUE)
    flattened <- flattened[nzchar(flattened) & !is.na(flattened)]
    flattened <- as.character(flattened)
    
    return(paste0(flattened, collapse = "\n"))
  },
  error = function(e){
    print(e)
    warning("Convert to string Error!")
    return(NA_character_)
  })
}


api_config <- list(
  url = "https://api.deepseek.com/v1/chat/completions",
  model = "deepseek-chat",
  temperature = 0.1,
  max_tokens = 1000,
  delay = 1,
  api_key= "sk-4c49f4e440724c41906d8d1ebe02c1ed"  
)

get_advisory_explanation <- function(text) {
  
  # System prompt with clear instructions
  system_prompt <- paste(
    "You are an expert advisory analyst. Provide a concise explanation of this advisory. ",
    "Structure your response with:",
    "1. A brief overview of the advisory's purpose",
    "2. The key recommendations or warnings",
    "3. The potential impact or consequences",
    "4. Any specific action steps",
    "Use clear, professional language suitable for business audiences.",
    "Avoid technical jargon unless absolutely necessary.\n",
    "Your output MUST use HTML GRAMMER."
  )
  
  tryCatch({
    # Make the API call
    response <- POST(
      url = api_config$url,
      add_headers(
        "Authorization" = paste("Bearer", api_config$api_key),
        "Content-Type" = "application/json"
      ),
      body = list(
        model = api_config$model,
        temperature = api_config$temperature,
        max_tokens = api_config$max_tokens,
        messages = list(
          list(role = "system", content = system_prompt),
          list(role = "user", content = paste("Explain this advisory:", text))
        )
      ),
      encode = "json",
      timeout(60)
    )
    
    # Check response status
    if (httr::status_code(response) != 200) {
      stop(paste("API request failed with status", httr::status_code(response)))
    }
    
    # Parse response
    content <- httr::content(response, "parsed")
    
    # Validate response structure
    if (!is.list(content) || is.null(content$choices) || length(content$choices) == 0) {
      stop("Invalid API response structure")
    }
    
    # Return the content
    return(content$choices[[1]]$message$content)
    
  }, error = function(e) {
    # Log the error if we have a Shiny session
    print(e)
  })
  
}



ui <- fluidPage(
  titlePanel("Advisory Summary Dashboard"),
  
  tabsetPanel(
    tabPanel("By Control Center",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date1", "Select Date Range:",
                                start = min(smy_date_ctl$Date),
                                end = max(smy_date_ctl$Date)),
                 selectInput("control_code", "Control Code:",
                             choices = unique(smy_date_ctl$Control_Code),
                             selected = sort(c("DCC", "ZDC")),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("plot1"),
                 DTOutput("table1")
               )
             )
    ),
    
    tabPanel("By Airport",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("date2", "Select Date Range:",
                                start = min(smy_date_airport$Date),
                                end = max(smy_date_airport$Date)),
                 selectInput("airport_code", "Airport Code:",
                             choices = unique(smy_date_airport$Airport_Code),
                             selected = sort(c("SFO", 
                                               "LGA", "BOS", "IAD", "DTW"
                                               )),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("plot2"),
                 DTOutput("table2")
               )
             )
    ),
    
    tabPanel("Advisory Type Count",
             sidebarLayout(
               sidebarPanel(
                 selectInput("month", "Month:",
                             choices = c(1,2,3),
                             selected = c(1,2,3),
                             multiple = TRUE),
                 selectInput("advisory_type", "Advisory Type:",
                             choices = unique(smy_categ_ct$Type),
                             selected = unique(smy_categ_ct$Type),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("plot3"),
                 DTOutput("table3")
               )
             )
    ),
    
    tabPanel("Avg Effective Time",
             sidebarLayout(
               sidebarPanel(
                 selectInput("month2", "Month:",
                             choices = c(1,2,3,4),
                             selected = c(1,2,3,4),
                             multiple = TRUE),
                 selectInput("adv_type_eff", "Advisory Type:",
                             choices = unique(avg_eff_time$Type),
                             selected = unique(avg_eff_time$Type),
                             multiple = TRUE)
               ),
               mainPanel(
                 plotlyOutput("plot4"),
                 DTOutput("table4")
               )
             )
    ),
    
    tabPanel("Understand by Deepseek",
             fluidRow(
               column(
                 width = 6,
                 dateInput("http_date", "Select Date:",
                           min = as.Date("2025-01-01"),
                           max = Sys.Date() - 1),
                 actionButton("http_submit", "Submit"),
                 h5("The First Advisory of the Selected Date"),
                 htmlOutput("http_response") 
               ),
               column(
                 width = 6,
                 h3("Explanation"),
                 htmlOutput("translation_output")
               )
             )
    )
    
    
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlotly({
    data <- smy_date_ctl %>%
      filter(Date >= input$date1[1], Date <= input$date1[2],
             Control_Code %in% input$control_code)
    p <- plot_ly(
      data = data,
      x = ~Date,
      y = ~Count,
      type = "scatter",
      mode = 'lines+markers',  # show both lines and markers
      line = list(dash = 'dash', width = 1),  # dashed and thin
      marker = list(symbol = 'circle-open'),  # circle points
      color = ~Control_Code,
      colors = palette,
      marker = list(size = 6)
    ) %>%
      layout(
        title = "Daily Advisory Count by Control Center",
        yaxis = list(title = "Count"),
        xaxis = list(
          title = "Date",
          tickformat = "%Y-%m-%d"  # Format: yyyy-mm-dd
        )
      )
    
    p
  })
  
  output$table1 <- renderDT({
    datatable(smy_date_ctl %>%
                filter(Date >= input$date1[1], Date <= input$date1[2],
                       Control_Code %in% input$control_code),
              options = list(pageLength = 10))
  })
  
  output$plot2 <- renderPlotly({
    data <- smy_date_airport %>%
      filter(Date >= input$date2[1], Date <= input$date2[2],
             Airport_Code %in% input$airport_code)
    
    p <- plot_ly(
      data = data,
      x = ~Date,
      y = ~Count,
      type = "scatter",
      mode = 'lines',  # show both lines and markers
      line = list(dash = 'dash', width = 1),  # dashed and thin
      #marker = list(symbol = 'circle-open', size=6),  # circle points
      color = ~Airport_Code,
      colors = palette
    ) %>%
      layout(
        title = "Daily Advisory Count by Airport",
        yaxis = list(title = "Count"),
        xaxis = list(
          title = "Date",
          tickformat = "%Y-%m-%d"  # Format: yyyy-mm-dd
        )
      )
    
    p
    
  })
  
  output$table2 <- renderDT({
    datatable(smy_date_airport %>%
                filter(Date >= input$date2[1], Date <= input$date2[2],
                       Airport_Code %in% input$airport_code),
              options = list(pageLength = 10))
  })
  
  output$plot3 <- renderPlotly({
    data <- smy_categ_ct %>%
      filter(Month %in% input$month,
             Type %in% input$advisory_type)
    
    # Plotly version of the advisory type line plot
    p <- plot_ly(
      data = data,
      x = ~factor(Month, levels = c(12,1,2,3)),
      y = ~Count,
      type = 'scatter',
      mode = 'lines',
      color = ~Type,
      colors = palette,
      line = list(width = 2)
    ) %>%
      layout(
        title = "Advisory Count by Type",
        yaxis = list(title = "Count"),
        xaxis = list(
          title = "Month"        )
      )
    
    p
  })
  
  output$table3 <- renderDT({
    datatable(smy_categ_ct %>%
                filter(Month %in% input$month,
                       Type %in% input$advisory_type),
              options = list(pageLength = 10))
  })
  
  output$plot4 <- renderPlotly({
    data <- avg_eff_time %>% drop_na() %>%
      filter(Month %in% input$month2,
             Type %in% input$adv_type_eff)
    
    p <- plot_ly(
      data = data,
      x = ~factor(Month, levels = c(12,1,2,3,4)),
      y = ~abs(Mean_Effect_Duration),
      type = 'bar',
      color = ~Type,
      colors = palette
    ) %>%
      layout(
        barmode = "dodge",
        title = "Avg Effective Duration by Advisory Type & Month",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Average Minutes")
      )
    
    p
  })
  
  output$table4 <- renderDT({
    datatable(avg_eff_time %>%
                filter(Month %in% input$month2,
                       Type %in% input$adv_type_eff) %>%
                mutate(Mean_Effect_Duration = round(Mean_Effect_Duration, 2)),
              options = list(pageLength = 10))
  })
  
  
  observeEvent(input$http_submit, {
    req(input$http_date)
    
    # Simulate HTTP GET call
    query_url <- get_faa_advisory(input$http_date)
    
    page <- GET(query_url,user_agent("Mozilla/5.0"))
    content <- read_html(page)
    detail_query_url <- content %>% html_nodes("tr") %>% tail(-1)  %>% rev() %>%
      detect(function(x) {
        columns <- html_nodes(x, "td")
        length(columns) == 5
      }) %>% 
      {
        links <- html_nodes(., "a") %>% html_attr("href")
        ifelse(length(links) > 0, paste0("https://www.fly.faa.gov/adv/", trimws(links[1])), NA)
      }
    
    # HTTP GET call
    # Step 2: GET response
    res <- httr::GET(detail_query_url)
    res_content <- httr::content(res, as = "text", encoding = "UTF-8")
    
    # Step 3: Parse HTML and extract table with id="table3"
    html_parsed <- read_html(res_content)
    table3_node <- html_parsed %>% html_node("#Table3")  # assumes <table id="table3">
    
    table3_html <- if (!is.null(table3_node)) {
      as.character(table3_node)
    } else {
      "<p>No Table3 found in response.</p>"
    }
    
    # Step 4: Render only table3
    output$http_response <- renderUI({
      HTML(table3_html)
    })
    
    df <- html_parsed %>%
        html_element("table#Table3") %>%
        html_nodes("tr") %>%
        tail(-1) %>%  # Skip header row
        map(function(r) {
          columns <- html_nodes(r, "td") %>% html_text(trim = TRUE)
        }) %>%
        extract_advisory_text()
      
     output$translation_output <- renderUI({
        html_output <- get_advisory_explanation(df)

        HTML(html_output)
        })
  })
  
  
}

shinyApp(ui, server)
