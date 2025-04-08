library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)
library(progress)

source("GPT_classifier.R", echo=T)


faa_data <- readRDS("./faa_data_ck_1_bf_0401.rds") %>%
  process_advisories_safe()

saveRDS(faa_data, "faa_data_ck_2_bf_0401.rds")


##test-------------------------------------------------
#faa_data <- readRDS("./faa_data_checkpoint_1.rds") %>%
#  mutate( Link = gsub(" ", "%20", trimws(Link)))
##-------------------------------------
#
#pb <- progress_bar$new(
#  format = "  Scraping [:bar] :percent eta: :eta",
#  total = dim(faa_data)[1],
#  clear = FALSE,
#  width = 60
#)
#
#faa_data <- faa_data %>% filter(Date>"03/01/25" & Date<="04/01/25")
#print(dim(faa_data))
#
#faa_data <- faa_data %>% 
#  mutate(
#    Detail = Link %>% map(function(urll){
#      
#      pb$tick()
#      # Request the page
#      tryCatch({
#        response <- GET(urll, user_agent("Mozilla/5.0"))
#        
#        # Proceed if the request was successful
#        if (status_code(response) == 200) {
#          http_content <- read_html(content(response, as = "text"))
#          
#          # Extract rows from table with id = "Table3"
#          df <- http_content %>%
#            html_element("table#Table3") %>%
#            html_nodes("tr") %>%
#            tail(-1) %>%  # Skip header row
#            map(function(r) {
#              columns <- html_nodes(r, "td") %>% html_text(trim = TRUE)
#            }) #%>% list_rbind()
#          df
#        }
#        else{
#          return(list())
#        }
#      },
#      error=function(e){
#        print(e)
#        print(urll)
#        stop()
#      })
#      
#    })
#  )
#
#
#saveRDS(faa_data, "faa_data_ck_1_bf_0401.rds")