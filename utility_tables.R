library(rvest)
library(httr)
library(tidyverse)
library(jsonlite)

# get center data from wiki

#https://en.wikipedia.org/wiki/List_of_U.S._Air_Route_Traffic_Control_Centers

url <- "https://en.wikipedia.org/wiki/List_of_U.S._Air_Route_Traffic_Control_Centers"

# Read the HTML content
page <- read_html(url)

# Extract all tables from the page
tables <- page %>% html_table(fill = TRUE)

# View the table of interest
artcc_table <- tables[[1]]  # The first table is the one listing the ARTCCs

# center abbr
center_abbr <- artcc_table$`Facility[3]`[-26]
center_abbr <- c(center_abbr,
                 "CZV", "CZE", "CZW", "CZU", "CZY")

