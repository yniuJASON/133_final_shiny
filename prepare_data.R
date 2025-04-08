library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)
library(progress)

source("Extract_data_from_web.R")
source("GPT_classifier.R", echo=T)
source("utility_tables.R")

#Prepate data 
#Date range
dates <- seq(as.Date("2025-01-01"), as.Date("2025-04-01"), by="days")

# Fetch advisory content for each date
faa_data <- dates %>%
  map(function(d){
    urll <- get_faa_advisory(d)
    print(paste("Fetching:", urll, "\n"))  # Print URL being fetched
    page <- GET(url,user_agent("Mozilla/5.0"))
    Sys.sleep(5)
    content <- read_html(page)
    return(content)  # Extract text from the webpage
  }) 

print("Step 1 - Complete")

faa_data <- faa_data %>% 
  map(function(http_content) {
    http_content %>% 
      html_nodes("tr") %>%
      tail(-1) %>% # Skip the header row
      map(function(x){
        columns <- html_nodes(x, "td")
        links <- html_nodes(x, "a") %>% html_attr("href")
        
        if (length(columns) == 5) {
          tibble(
            Number = html_text(columns[1]),
            Control_Element = html_text(columns[2]),
            Date = html_text(columns[3]),
            Brief_Title = html_text(columns[4]),
            Send_Time = html_text(columns[5]),
            Link = ifelse(length(links) > 0, paste0("https://www.fly.faa.gov/adv/", links[1]), NA) # Create absolute URL
          )}
      }) %>% list_rbind() # it will return a dataframe, representing table of each date
  } ) %>% 
  list_rbind() %>% # it will return a dataframe, combining all dates
  mutate(
    Link = gsub(" ", "%20", trimws(Link)),
    Control_Code = case_when(
      str_sub(Control_Element, -3) %in% center_abbr ~ str_sub(Control_Element, -3),
      Control_Element == "DCC" ~ "DCC",
      TRUE ~ "Other Entity"
    ),
    Airport_Code = case_when(
      Control_Code=="Other Entity" ~ "Other Entity",
      TRUE ~ str_extract(Control_Element, "^[^/]+")
    )
  ) 

print("Step 2 - Complete")

saveRDS(faa_data, "faa_data_checkpoint_1.rds")


pb <- progress_bar$new(
  format = "  Scraping [:bar] :percent eta: :eta",
  total = dim(faa_data)[1],
  clear = FALSE,
  width = 60
)


faa_data <- faa_data %>% 
  mutate(
    Detail = Link %>% map(function(urll){
      
      pb$tick()
      # Request the page
      tryCatch({
          response <- GET(urll, user_agent("Mozilla/5.0"))
          
          # Proceed if the request was successful
          if (status_code(response) == 200) {
            http_content <- read_html(content(response, as = "text"))
            
            # Extract rows from table with id = "Table3"
            df <- http_content %>%
              html_element("table#Table3") %>%
              html_nodes("tr") %>%
              tail(-1) %>%  # Skip header row
              map(function(r) {
                columns <- html_nodes(r, "td") %>% html_text(trim = TRUE)
              }) #%>% list_rbind()
            df
          }
          else{
            return(list())
          }
      },
      error=function(e){
        print(e)
        print(urll)
        stop()
      })
      
    })
  )

  close(pb)
faa_data <- faa_data %>% process_advisories_safe()

saveRDS(faa_data, "faa_data_checkpoint_2.rds")

print("Step 3 - Complete")

faa_data <- readRDS("faa_data_checkpoint_2.rds")

faa_data <- faa_data %>% 
  mutate(
    # Convert Send_Date from UTC to EST
    Send_Date = mdy_hm(Send_Time, tz = "UTC"),
    Send_Date_EST = with_tz(Send_Date, tzone = "America/New_York"),
    
    Month = month(Send_Date_EST),
    Day = day(Send_Date_EST),
    Hour = hour(Send_Date_EST),
    Minute = minute(Send_Date_EST),
    
    # Extract EFFECTIVE TIME string from nested Detail list
    Effective_String = map_chr(Detail, function(x) {
      x <- unlist(x, recursive = T)
      if (length(x)>=4) {return(x[[4]])}
      else {return(NA_character_)}
    }),
    # Parse start and end from EFFECTIVE TIME (e.g., "010012 - 010145")
    Eff_Start = str_sub(Effective_String, 1, 6),
    Eff_End = str_sub(Effective_String, 10, 15),
    
    # Break down start and end into components
    Eff_Start_Day = as.integer(str_sub(Eff_Start, 1, 2)),
    Eff_Start_Hour = as.integer(str_sub(Eff_Start, 3, 4)),
    Eff_Start_Min = as.integer(str_sub(Eff_Start, 5, 6)),
    
    Eff_End_Day = as.integer(str_sub(Eff_End, 1, 2)),
    Eff_End_Hour = as.integer(str_sub(Eff_End, 3, 4)),
    Eff_End_Min = as.integer(str_sub(Eff_End, 5, 6)),
    
    # Construct datetimes assuming same year 
    Eff_Start_DT = make_datetime(year = year(Send_Date), 
                                 month = month(Send_Date_EST), 
                                 day = Eff_Start_Day,
                                 min = Eff_Start_Min, 
                                 hour = Eff_Start_Hour),
    Eff_End_DT = make_datetime(year = year(Send_Date), 
                               month = month(Send_Date_EST), 
                               day = Eff_End_Day,
                               min = Eff_End_Min, 
                               hour = Eff_End_Hour),
    
    # Duration in minutes
    Eff_Duration_Minutes = as.numeric(difftime(Eff_End_DT, Eff_Start_DT, units = "mins"))
  )

print("Complete")

print(head(faa_data))

saveRDS(faa_data, "faa_data.rds")


